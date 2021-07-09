#loading packages
library(shiny)
library(plotly)
library(leaflet)
library(shinythemes)
library(dplyr)
library(sf)

# loading datasets
dataset <- readRDS("data.rds")

# load meta data for countries
source("country_meta.R")

# the country tabs are handled by shiny modules
# country_tab_ui() and country_tab_server() code found in R/country_tab.R

# front end interface
ui <- navbarPage(id = "tabset",
  "The Benefits and Costs of Scaling Up Mosquito Release Technologies for Dengue & Malaria Prevention",
  windowTitle = "Cost Benefit Tool",
  theme = shinytheme("cerulean"),
  user_guide_tab(), # R/user_guide_tab.R
  data_assumption_caution_tab(), # R/data_assump_caut.R
  country_tab_ui("Brazil", "BRA", country_meta$BRA),
  country_tab_ui("Burkina Faso", "BF", country_meta$BF),
  country_tab_ui("Colombia", "COL", country_meta$COL),
  country_tab_ui("Indonesia", "IDN", country_meta$IDN),
  country_tab_ui("Mexico", "MEX", country_meta$MEX),
  country_tab_ui("Sri Lanka", "LKA", country_meta$LKA),
  country_tab_ui("Vietnam", "VNM", country_meta$VNM)
)

# back end logic
# input$tabset is a variable containing the currently-selected tab
server <- function(input, output, session) {
  country_tab_server("BF", dataset, reactive(input$tabset), country_meta$BF)
  country_tab_server("IDN", dataset, reactive(input$tabset), country_meta$IDN)
  country_tab_server("BRA", dataset, reactive(input$tabset), country_meta$BRA)
  country_tab_server("MEX", dataset, reactive(input$tabset), country_meta$MEX)
  country_tab_server("COL", dataset, reactive(input$tabset), country_meta$COL)
  country_tab_server("VNM", dataset, reactive(input$tabset), country_meta$VNM)
  country_tab_server("LKA", dataset, reactive(input$tabset), country_meta$LKA)
}

# Run the application
shinyApp(ui = ui, server = server)
