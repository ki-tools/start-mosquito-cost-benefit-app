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

app_theme <- bslib::bs_theme(
  bootswatch = "cosmo",
  # bootswatch = "minty",
  # bootswatch = "yeti",
  # bootswatch = "cerulean",
  # base_font = bslib::font_google("Poppins"),
  # base_font = bslib::font_google("Open Sans"),
  # base_font = bslib::font_google("Lato"),
  base_font = bslib::font_google("Montserrat"),
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
    class = "nav-item border-bottom",
    role = "presentation",
    tags$button(
      class = paste0("nav-link w-100 text-start fw-normal fs-6", ifelse(active, " active", "")),
      id = paste0("pills-", id, "-tab"),
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

app_header <- function(title) {
  tags$header(
    class = "py-3 px-2 mb-3 border-bottom",
    style = "background: #dddddd;",
    tags$div(
      class = "container-fluid d-flex justify-content-between",
      tags$div(
        class = "fs-4 fw-bold",
        title
      ),
      tags$div(
        class = "d-flex",
        tags$a(href = "https://www.washington.edu/research/research-centers/start-center/", target = "_blank",
          tags$img(src = "START_306_logo.png", height = "40px")
        )
      )
    )
  )
}

sidebar_menu <- function(...) {

}

# front end interface
ui <- bslib::page_fill(
  theme = app_theme,
  app_header("The Benefits and Costs of Scaling Up Mosquito Release Technologies for Dengue Prevention"),
  tags$div(
    class = "d-flex",
    tags$div(
      class = "overflow-auto",
      style = "min-width: 200px; height: calc(100vh - 100px);",
      tags$ul(
        class = "nav nav-pills flex-column",
        id = "pills-tab",
        role = "tablist",
        app_nav_item("userguide", "User Guide", active = TRUE),
        app_nav_item("datasources", "Data Sources, Assumptions, & Cautions"),
        app_nav_item("BGD", "Bangladesh"),
        app_nav_item("BRA", "Brazil"),
        app_nav_item("COL", "Colombia"),
        app_nav_item("IND", "India"),
        app_nav_item("IDN", "Indonesia"),
        app_nav_item("MEX", "Mexico"),
        app_nav_item("NGA", "Nigeria"),
        app_nav_item("LKA", "Sri Lanka"),
        app_nav_item("VNM", "Vietnam")
      )
    ),
    tags$div(
      class = "tab-content",
      id = "pills-tabContent",
      app_nav_content("userguide", active = TRUE, user_guide_tab()),
      app_nav_content("datasources", data_assumption_caution_tab()),
      app_nav_content("BGD", country_tab_ui("BGD", country_meta$BGD)),
      app_nav_content("BRA", country_tab_ui("BRA", country_meta$BRA)),
      app_nav_content("COL", country_tab_ui("COL", country_meta$COL)),
      app_nav_content("IND", country_tab_ui("IND", country_meta$IND)),
      app_nav_content("IDN", country_tab_ui("IDN", country_meta$IDN)),
      app_nav_content("MEX", country_tab_ui("MEX", country_meta$MEX)),
      app_nav_content("NGA", country_tab_ui("NGA", country_meta$NGA)),
      app_nav_content("LKA", country_tab_ui("LKA", country_meta$LKA)),
      app_nav_content("VNM", country_tab_ui("VNM", country_meta$VNM))
    )
  )
  # div(
  #   style = "background: 'red';",
  #   div(
  #     bslib::navs_pill_list(
  #       id = "tabset",
  #       user_guide_tab(), # R/user_guide_tab.R
  #       data_assumption_caution_tab(), # R/data_assump_caut.R
  #       country_tab_ui("Bangladesh", "BGD", country_meta$BGD),
  #       country_tab_ui("Brazil", "BRA", country_meta$BRA),
  #       country_tab_ui("Colombia", "COL", country_meta$COL),
  #       country_tab_ui("India", "IND", country_meta$IND),
  #       country_tab_ui("Indonesia", "IDN", country_meta$IDN),
  #       country_tab_ui("Mexico", "MEX", country_meta$MEX),
  #       country_tab_ui("Nigeria", "NGA", country_meta$NGA),
  #       country_tab_ui("Sri Lanka", "LKA", country_meta$LKA),
  #       country_tab_ui("Vietnam", "VNM", country_meta$VNM)
  #     )
  #   ),
  #   mainPanel(
  #   )
  # )
)

# back end logic
# input$tabset is a variable containing the currently-selected tab
server <- function(input, output, session) {
  # country_tab_server("IDN", dataset, reactive(input$tabset), country_meta$IDN)
  # country_tab_server("BRA", dataset, reactive(input$tabset), country_meta$BRA)
  # country_tab_server("MEX", dataset, reactive(input$tabset), country_meta$MEX)
  # country_tab_server("COL", dataset, reactive(input$tabset), country_meta$COL)
  # country_tab_server("VNM", dataset, reactive(input$tabset), country_meta$VNM)
  # country_tab_server("LKA", dataset, reactive(input$tabset), country_meta$LKA)

  # country_tab_server("IND", dataset, reactive(input$tabset), country_meta$IND)
  # country_tab_server("NGA", dataset, reactive(input$tabset), country_meta$NGA)
  # country_tab_server("BGD", dataset, reactive(input$tabset), country_meta$BGD)

}

# Run the application
shiny::shinyApp(ui = ui, server = server)
