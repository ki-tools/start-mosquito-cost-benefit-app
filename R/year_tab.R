# this file contains a shiny module for each year tab
# reference: https://mastering-shiny.org/scaling-modules.html

tab_style <- "height: calc(100vh - 165px); overflow: auto;"

year_tab_ui <- function(id, country_id, country_meta) {
  ns <- shiny::NS(id)
  div(
    style = tab_style,
    h2("Key Indicators"),
    tags$p("The following section highlights key indicators, including the total program budget, number of dengue cases averted, and percentage of cases averted nationally. In the following map, please select a key indicator to be shown in the map."),
    tags$div(
      class = "row",
      tags$div(
        class = "container",
        style = "margin-bottom: 10px; width: calc(100vw - 590px); color: #FFFFFFAA; font-weight: 600; font-size: 24px;",
        tags$div(
          class = "row",
          tags$div(
            class = "col mx-1 py-2 text-center",
            style = "background: #e49444;",
            "Total Budget: ",
            textOutput(ns("totalbudget"), inline = TRUE)
          ),
          tags$div(
            class = "col mx-1 py-2 text-center",
            style = "background: #d1615d;",
            "Cases Averted: ",
            textOutput(ns("casesaverted"), inline = TRUE)
          ),
          tags$div(
            class = "col mx-1 py-2 text-center",
            style = "background: #85b6b2;",
            textOutput(ns("pctaverted"), inline = TRUE),
            "% Averted"
          )
        )
      )
    ),
    withSpinner(leafletOutput(ns("mymap"), height = "600px"))
  )
}

#' @param id country ID (used to access the appropriate data)
#' @param dataset
year_tab_server <- function(
  id, country_id, tab, rv, country_meta, primaryinputtype, submit
) {
  moduleServer(id, function(input, output, session) {
    output$mytext <- renderText({
      rv()$PREP
    })

    # selects current data based on the tab that is being viewed
    # dataset is a named list of datasets with name corresponding to tab value
    cur_dat <- reactive({
      # message("tab: ", tab())
      # message("id: ", country_id)
      if (tab() != country_id || is.null(rv()$POPDENSITY))
        return(NULL)

      path <- file.path("data", rv()$POPDENSITY, paste0(tab(), ".rds"))
      readRDS(path)
    }) %>%
      bindCache(tab(), rv()$POPDENSITY)

    # augment the current dataset with variables derived from user inputs
    cur_dat_aug <- eventReactive(submit(), {
      isolate({
        if (is.null(cur_dat()))
          return(NULL)

        if (primaryinputtype() == "1") {
          PLANNING <- as.numeric(rv()$PLANNING)
          PREP <- as.numeric(rv()$PREP)
          PROD <- as.numeric(rv()$PRODUCTION)
          DIST <- as.numeric(rv()$DISTRIBUTION)
          MONITOR <- as.numeric(rv()$MONITORING)
          RELEASE <- as.numeric(rv()$RELEASE)
        } else {
          PLANNING <- as.numeric(rv()$DEFINEWORKPLAN) + as.numeric(rv()$DETERMINERELEASE)
          PREP <- as.numeric(rv()$ENROLCOMMUNITY) # + as.numeric(rv()$PREP_PLAN)
          PROD <- as.numeric(rv()$PRODUCTION_FACILITY) + as.numeric(rv()$MOSLINECREATION) + as.numeric(rv()$MOSPROD) + as.numeric(rv()$QUALITYMANAGEMENT)
          DIST <- as.numeric(rv()$DELIVEREGGS)
          RELEASE <- as.numeric(rv()$EGGDEPLOYMENT) + as.numeric(rv()$QUALITYASSURANCE)
          MONITOR <- as.numeric(rv()$ADAPTIVEMANAGEMENT) + as.numeric(rv()$MEASURECOMMUNITY) + as.numeric(rv()$MONITORINGWOLBACHIA)
        }

        tot_cost <- PLANNING + PREP + PROD + DIST + MONITOR + RELEASE

        yr <- paste0("year", id)
        yrdat <- CONSTANTS[[yr]]

        d <- cur_dat()

        newdat <- tibble(
          tot_cases = d$totpop * yrdat$popgrowth * d$totdeng * yrdat$multiplier * rv()$EFFECTIVENESS,
          tot_dalys = tot_cases * country_meta$daly_per_case * yrdat$multiplier,
          tot_hosp = tot_cases * country_meta$pct_trt_hosp * yrdat$multiplier,
          tot_amb = tot_cases * country_meta$pct_trt_amb * yrdat$multiplier,
          tot_nonmed = tot_cases * country_meta$pct_trt_nonmedical * yrdat$multiplier,
          area_cov_int = d$areatsqkm * rv()$AREACOV,
          pop_cov_int = d$tarpop * yrdat$popgrowth * rv()$AREACOV,
          tot_cost_int = tot_cost * rv()$AREACOV * yrdat$costs,
          cost_per_person = tot_cost_int / pop_cov_int,
          cases_avert = pop_cov_int * yrdat$multiplier * d$tardeng,
          dalys_avert = cases_avert * country_meta$daly_per_case,
          hosp_avert = cases_avert * country_meta$pct_trt_hosp,
          amb_avert = cases_avert * country_meta$pct_trt_amb,
          nonmed_avert = cases_avert * country_meta$pct_trt_nonmedical,
          cost_per_case = tot_cost_int / cases_avert,
          cost_per_daly = tot_cost_int / dalys_avert,
          direct_hosp_cost = hosp_avert * country_meta$cost_per_hosp,
          direct_amb_cost = amb_avert * country_meta$cost_per_amb,
          direct_nonmed_cost = nonmed_avert * country_meta$cost_per_nonmedical,
          indirect_hosp_cost = hosp_avert * country_meta$indirectcost_per_hosp,
          indirect_amb_cost = amb_avert * country_meta$indirectcost_per_amb,
          indirect_nonmed_cost = nonmed_avert * country_meta$indirectcost_per_nonmedical
        )

        dat <- bind_cols(d, newdat) # need d first so it remains an "sf" object

        # update leaflet output
        cost <- dat$cost_per_person
        cost <- ifelse(is.infinite(cost), NA, cost)
        # mybins <- c(0, 10, 20, 50, 100, 500, Inf)
        # mybins <- c(0, 2, 5, 10, 20, 50, 100, 500, Inf)
        mybins <- c(0, 5, 50, 100, 500, 1000, 5000, 10000, Inf)
        pal1 <- colorBin(
          palette = "viridis",
          domain = cost,
          na.color = "transparent",
          bins = mybins)
        # pal1 <- colorNumeric("Reds", cost)
        leafletProxy("mymap", data = dat) %>%
          # remove old legend
          clearControls() %>%
          # remove previously plotted polygons
          clearShapes() %>%
          # add polygons with colors based on cost per case averted
          addPolygons(
            weight = 0.3,
            stroke = TRUE,
            color = "black",
            fillColor = ~pal1(cost),
            smoothFactor = 0.2,
            fillOpacity = 0.75,
            label = maptooltips(dat), # function defined below
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px", 
              direction = "auto"
            )
            # popup = mappopup(dat),
            # color = ~pal1(cost)
          ) %>%
          addLegend(
            position = "bottomleft",
            pal = pal1,
            values = ~cost,
            title = "Cost per case averted",
            opacity = 0.9)

        dat
      })
    }, ignoreNULL = FALSE)

    output$mymap <- renderLeaflet(make_map(cur_dat_aug(),
      country_meta))


    totalbudget <- reactive({
      if (is.null(cur_dat_aug())) {
        return(NULL)
      }
      res <- sum(cur_dat_aug()$tot_cost_int)
      res <- prettyNum(round(res / 1e6, 1), big.mark = ",", scientific = FALSE)
      paste0("$", format(res, trim = TRUE), "M")
    })

    casesavertednum <- reactive({
      if (is.null(cur_dat_aug())) {
        return(NULL)
      }
      # TODO: check why there are NAs
      sum(cur_dat_aug()$cases_avert, na.rm = TRUE)
    })

    casesaverted <- reactive({
      if (is.null(casesavertednum())) {
        return(NULL)
      }
      res <- casesavertednum()
      res <- prettyNum(round(res / 1e6, 1), big.mark = ",", scientific = FALSE)
      paste0(format(res, trim = TRUE), "M")
    })

    pctaverted <- reactive({
      if (is.null(casesavertednum()) || is.null(cur_dat_aug())) {
        return(NULL)
      }
      round(100 * casesavertednum() / sum(cur_dat_aug()$tot_cases, na.rm = TRUE), 1)
    })

    output$totalbudget <- renderText(totalbudget())
    output$casesaverted <- renderText(casesaverted())
    output$pctaverted <- renderText(pctaverted())
  })
}

datatable <- function(dat, columns, id, pagesize = 10) {
  if (is.null(dat))
    return(NULL)
  htmltools::browsable(
    tagList(
      reactable(
        dat,
        searchable = TRUE,
        defaultPageSize = pagesize,
        elementId = paste0(id, "-table"),
        theme = reactableTheme(
          style = list(
            fontFamily = "PT Mono, Segoe UI, Helvetica, Arial, sans-serif"
          )
        ),
        columns = columns,
        compact = TRUE
      ),
      tags$button(
        class = "float-right",
        tagList(fontawesome::fa("download"), "Download as CSV"),
        onclick = paste0(
          "Reactable.downloadDataCSV('",
          id, "-table', '", id, ".csv')")
      )
    )
  )
}

# create leaflet map
make_map <- function(data, country_meta) {
  if (is.null(data))
    return(NULL)

  cost <- data$cost_per_person
  cost <- ifelse(is.infinite(cost), NA, cost)

  # mybins <- c(0, 10, 20, 50, 100, 500, Inf)
  # mybins <- c(0, 2, 5, 10, 20, 50, 100, 500, Inf)
  mybins <- c(0, 5, 50, 100, 500, 1000, 5000, 10000, Inf)
  pal1 <- colorBin(
    palette = "viridis",
    domain = cost,
    na.color = "transparent",
    bins = mybins)

  # pal1 <- colorNumeric("viridis", cost)
  leaflet(data) %>%
    addTiles() %>%
    setView(
      lat = country_meta$lat,
      lng = country_meta$lng,
      zoom = country_meta$zoom
    ) %>%
    addPolygons(
      # fillColor = ~mypalette(data$km_target),
      fillColor = ~pal1(cost),
      stroke = TRUE,
      fillOpacity = 0.75,
      color = "black",
      weight = 0.3,
      # label = mytext,
      label = maptooltips(data),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      # pal = mypalette,
      pal = pal1,
      # values = ~data$km_target,
      values = ~cost,
      opacity = 0.9,
      # title = "Target Area (KM2)",
      title = "Cost per case averted",
      position = "bottomleft") %>%
    addProviderTiles(providers$CartoDB.Positron)
}

format_big <- function(x)
  format(round(as.numeric(x)), big.mark = ",")

# tooltips for updated leaflet map
maptooltips <- function(dat) {
  paste0(
    "Name: ", dat$name, "<br/>",
    "Target Population: ",  format_big(dat$tarpop), "<br/>",
    # "Incidence: ", round(dat$prev_inc_m, 3), "<br/>",
    "Total Area: ",  format_big(dat$areasqkm), "<br/>",
    "Target Area: ",  format_big(dat$areatsqkm), "<br/>",
    "Total Cost of Intervention (User Generated): ", "$",
    format_big(dat$tot_cost_int), "<br/>",
    "Cost per Person (User Generated): ", "$",
    format_big(dat$cost_per_person), "<br/>",
    "Cost per Case (User Generated): ", "$",
    format_big(dat$cost_per_case), "<br/>",
    "Cost per DALY (User Generated): ", "$",
    format_big(dat$cost_per_daly), "<br/>"
  ) %>%
  lapply(htmltools::HTML)
}
