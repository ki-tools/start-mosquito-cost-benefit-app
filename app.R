# options(shiny.autoreload = TRUE)
# shiny::runApp()

#loading packages
library(shiny)
library(plotly)
library(leaflet)
library(dplyr)
library(sf)
library(bslib)
library(showtext)
library(fontawesome)
library(reactable)

# <i class="fa-solid fa-signs-post"></i>

app_theme <- bslib::bs_theme(
  bootswatch = "cosmo",
  # bootswatch = "minty",
  # bootswatch = "yeti",
  # bootswatch = "cerulean",
  # base_font = bslib::font_google("Poppins"),
  # base_font = bslib::font_google("Open Sans"),
  # base_font = bslib::font_google("Lato"),
  base_font = bslib::font_google("Montserrat", wght = "300;400;500;600;700"),
  code_font = bslib::font_google("PT Mono"),
  # base_font = bslib::font_google("Raleway"),
  # font_scale = 1.2,
  "navbar-light-bg" = "#dddddd"
)

# loading datasets
dataset <- readRDS("data.rds")

# load meta data for countries
source("country_meta.R")

# the country tabs are handled by shiny modules
# country_tab_ui() and country_tab_server() code found in R/country_tab.R

# windowTitle = "Cost Benefit Tool",

tags <- htmltools::tags

app_nav_item <- function(id, val, active = FALSE) {
  tags$li(
    class = "nav-item",
    style = "border-bottom: 1px solid rgba(255, 255, 255, 0.2);",
    role = "presentation",
    tags$button(
      class = paste0("nav-link w-100 text-start fs-6", ifelse(active, " active", "")),
      style = "font-weight: 600; color: white;",
      id = paste0("pills-", id, "-tab"),
      "data-value" = id,
      "data-toggle" = "pill",
      "data-bs-toggle" = "pill",
      "data-bs-target" = paste0("#pills-", id),
      type = "button",
      role = "tab",
      "aria-controls" = paste0("pills-", id),
      "aria-selected" = ifelse(active, "true", "false"),
      val
    )
  )
}

app_nav_content <- function(id, ..., active = FALSE) {
  tags$div(
    class = paste0("tab-pane fade show", ifelse(active, " active", "")),
    id = paste0("pills-", id),
    role = "tabpanel",
    "aria-labelledby" = paste0("pills-", id, "-tab"),
    htmltools::tagList(...)
  )
}

# "The Benefits and Costs of Scaling Up Mosquito Release Technologies for Dengue Prevention"
app_header <- function() {
  tags$header(
    class = "py-2 px-0 mb-3 border-bottom",
    style = "background: #2a2a2a;",
    tags$div(
      class = "container-fluid d-flex justify-content-between",
      tags$div(
        class = "col",
        tags$div(
          class = "fs-3 fw-bold",
          style = "color: white;",
          "Scaling Mosquito Release Technologies for Dengue Prevention"
        ),
        tags$div(
          class = "fs-4",
          style = "color: white; font-weight: 500;",
          "Cost-Benefit Analysis Tool"
        )
      ),
      tags$div(
        class = "d-flex",
        tags$a(href = "https://www.washington.edu/research/research-centers/start-center/", target = "_blank",
          tags$img(src = "start_header_black.png", height = "70px")
        )
      )
    )
  )
}

sidebar_menu <- function(...) {

}

# front end interface
ui <- bslib::page_fill(
  shinyjs::useShinyjs(debug = TRUE),
  theme = app_theme,
  app_header(),
  tags$div(
    class = "d-flex",
    tags$div(
      class = "overflow-auto",
      style = "width: 240px; min-width: 240px; height: calc(100vh - 94px); background: #494949DD; margin-top: -17px; padding-top: 18px;",
      tags$ul(
        class = "nav nav-pills flex-column shiny-tab-input",
        id = "tabset",
        role = "tablist",
        app_nav_item("userguide", tags$span(fa("signs-post"), "User Guide")),
        app_nav_item("datasources", tags$span(fa("triangle-exclamation"), "Data Sources, Assumptions, & Cautions")),
        tags$li(
          class = "nav-item py-2 px-3",
          style = "border-bottom: 1px solid rgba(255, 255, 255, 0.2); background: #494949;",
          role = "presentation",
          tags$span(
            class = "w-100 fs-6",
            style = "font-weight: 600; color: white;",
            fa("angle-down"), "Countries"
          )
        ),
        app_nav_item("BGD", "Bangladesh"),
        app_nav_item("BRA", "Brazil"),
        app_nav_item("COL", "Colombia", active = TRUE),
        app_nav_item("IND", "India"),
        app_nav_item("IDN", "Indonesia"),
        app_nav_item("MEX", "Mexico"),
        app_nav_item("NGA", "Nigeria"),
        app_nav_item("LKA", "Sri Lanka"),
        app_nav_item("VNM", "Vietnam")
      )
    ),
    tags$div(
      class = "col",
      tags$div(
        class = "tab-content row",
        id = "pills-tabContent",
        app_nav_content("userguide", user_guide_tab()),
        app_nav_content("datasources", data_assumption_caution_tab()),
        app_nav_content("BGD", country_tab_ui("BGD", country_meta$BGD)),
        app_nav_content("BRA", country_tab_ui("BRA", country_meta$BRA)),
        app_nav_content("COL", active = TRUE, country_tab_ui("COL", country_meta$COL)),
        app_nav_content("IND", country_tab_ui("IND", country_meta$IND)),
        app_nav_content("IDN", country_tab_ui("IDN", country_meta$IDN)),
        app_nav_content("MEX", country_tab_ui("MEX", country_meta$MEX)),
        app_nav_content("NGA", country_tab_ui("NGA", country_meta$NGA)),
        app_nav_content("LKA", country_tab_ui("LKA", country_meta$LKA)),
        app_nav_content("VNM", country_tab_ui("VNM", country_meta$VNM))
      )
    )
  )
)

# back end logic
# input$tabset is a variable containing the currently-selected tab
server <- function(input, output, session) {
  country_tab_server("IDN", dataset, reactive(input$tabset), country_meta$IDN)
  country_tab_server("BRA", dataset, reactive(input$tabset), country_meta$BRA)
  country_tab_server("MEX", dataset, reactive(input$tabset), country_meta$MEX)
  country_tab_server("COL", dataset, reactive(input$tabset), country_meta$COL)
  country_tab_server("VNM", dataset, reactive(input$tabset), country_meta$VNM)
  country_tab_server("LKA", dataset, reactive(input$tabset), country_meta$LKA)
  country_tab_server("IND", dataset, reactive(input$tabset), country_meta$IND)
  country_tab_server("NGA", dataset, reactive(input$tabset), country_meta$NGA)
  country_tab_server("BGD", dataset, reactive(input$tabset), country_meta$BGD)
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
