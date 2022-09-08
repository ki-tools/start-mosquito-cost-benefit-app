# this file contains a shiny module for each year tab
# reference: https://mastering-shiny.org/scaling-modules.html

tab_style <- "height: calc(100vh - 165px); overflow-x: hidden; overflow-y: auto;"
var_lookup <- readr::read_csv("var_lookup.csv", show_col_types = FALSE)

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
    tags$div(
      style = "position: relative",
      withSpinner(leafletOutput(ns("mymap"), height = "550px")),
      tags$div(style = "background: rgba(255,255,255,0.75); position: absolute; right: 0; top: 0; padding-left: 15px; padding-right: 15px; padding-top: 10px; margin-right: 5px; margin-top: 5px; border-radius: 5px",
        selectInput(ns("mapvariable"), "Variable:", map_var_choices,
          selected = "cost_per_case", width = "400px")
      )
    ),
    tags$h2("Pre-Intervention Overview"),
    tags$p("The following table presents details related to selecting a location and population for implementation. Specifically, it presents pre-implementation/baseline area, target area (based on selected population density), total population, target population (based on selected population density), dengue incidence, cases, DALYs, hospitalized cases, ambulatory cases, and not-medically attended cases."),
    withSpinner(uiOutput(ns("preinterventiontable"))),
    tags$h2("Intervention Overview"),
    tags$p("The following table shows the impact of the mosquito release technology in health benefits including cases, DALYs, hospitalized cases, ambulatory cases, and not-medically attended cases that are averted."),
    withSpinner(uiOutput(ns("interventiontable"))),
    tags$h2("Intervention Health Benefits"),
    tags$p("The following table shows the impact of the mosquito release technology in health benefits including cases, DALYs, hospitalized cases, ambulatory cases, and not-medically attended cases that are averted."),
    withSpinner(uiOutput(ns("healthbenefitstable"))),
    tags$h2("Intervention Economic Benefits"),
    tags$p("The following table presents the impact of mosquito release technology in economic benefits including the cost per case, cost per DALY, direct hospitalization costs, direct ambulatory costs, direct non-medical costs, indirect hospitalized costs, indirect hospitalized costs, indirect ambulatory costs, and indirect non-medical costs."),
    withSpinner(uiOutput(ns("economicbenefitstable")))
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
          tot_cost_int = tot_cost * area_cov_int * yrdat$costs,
          cost_per_person = tot_cost_int / pop_cov_int,
          cases_avert = pop_cov_int * yrdat$multiplier * d$totdeng,
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
        plot_var <- dat[[input$mapvariable]]
        plot_var <- ifelse(is.infinite(plot_var), NA, plot_var)
        mybins <- get_bins(plot_var)

        pal1 <- colorBin(
          palette = "viridis",
          domain = plot_var,
          na.color = "transparent",
          bins = mybins)
        # pal1 <- colorNumeric("Reds", plot_var)
        leafletProxy("mymap", data = dat) %>%
          # remove old legend
          clearControls() %>%
          # remove previously plotted polygons
          clearShapes() %>%
          # add polygons with colors based on plot_var per case averted
          addPolygons(
            weight = 0.3,
            stroke = TRUE,
            color = "black",
            fillColor = ~pal1(plot_var),
            smoothFactor = 0.2,
            fillOpacity = 0.75,
            label = maptooltips(dat), # function defined below
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px", 
              direction = "auto"
            )
            # popup = mappopup(dat),
            # color = ~pal1(plot_var)
          ) %>%
          addLegend(
            position = "bottomleft",
            pal = pal1,
            values = ~plot_var,
            title = "Cost per case averted",
            opacity = 0.9)

        dat
      })
    }, ignoreNULL = FALSE)

    output$mymap <- renderLeaflet(make_map(cur_dat_aug(), input$mapvariable,
      country_meta))

    output$interventiontable <- renderUI({
      datatable(
        cur_dat_aug(),
        vars = c("name", "gaul_code",
          var_lookup$name[var_lookup$group == "Intervention"]),
        id = "interventiontable"
      )
    })

    output$preinterventiontable <- renderUI({
      datatable(
        cur_dat_aug(),

        vars = c("name", "gaul_code", "areasqkm", "areatsqkm",
          "totpop", "tarpop", "totdeng",
          var_lookup$name[var_lookup$group == "Pre-Intervention"]),
        id = "preinterventiontable"
      )
    })

    output$healthbenefitstable <- renderUI({
      datatable(
        cur_dat_aug(),

        vars = c("name", "gaul_code",
          var_lookup$name[var_lookup$group == "Health Benefits"]),
        id = "healthbenefitstable"
      )
    })

    output$economicbenefitstable <- renderUI({
      datatable(
        cur_dat_aug(),

        vars = c("name", "gaul_code",
          var_lookup$name[var_lookup$group == "Economic Benefits"]),
        id = "economicbenefitstable"
      )
    })

    totalbudget <- reactive({
      if (is.null(cur_dat_aug()))
        return(NULL)
      number_format(sum(cur_dat_aug()$tot_cost_int), dollars = TRUE)
    })

    casesavertednum <- reactive({
      if (is.null(cur_dat_aug()))
        return(NULL)
      # TODO: check why there are NAs
      sum(cur_dat_aug()$cases_avert, na.rm = TRUE)
    })

    casesaverted <- reactive({
      if (is.null(casesavertednum()))
        return(NULL)
      number_format(casesavertednum(), dollars = FALSE)
    })

    pctaverted <- reactive({
      if (is.null(casesavertednum()) || is.null(cur_dat_aug()))
        return(NULL)
      round(100 * casesavertednum() / sum(cur_dat_aug()$tot_cases, na.rm = TRUE), 1)
    })

    output$totalbudget <- renderText(totalbudget())
    output$casesaverted <- renderText(casesaverted())
    output$pctaverted <- renderText(pctaverted())
  })
}


map_var_choices <- list()
for (grp in setdiff(unique(var_lookup$group), "General")) {
  idx <- var_lookup$group == grp
  map_var_choices[[grp]] <-
    structure(as.list(var_lookup$name[idx]), names = var_lookup$desc[idx])
}

number_format <- function(x, dollars = FALSE) {
  res <- ifelse(x > 999999999,
    paste0(prettyNum(round((x / 1000000000), 2),
      big.mark = ",", scientific = FALSE), "B"),
    ifelse(x > 999999,
      paste0(prettyNum(round((x / 1000000), 1),
        big.mark = ",", scientific = FALSE), "M"),
      ifelse(x > 999,
        paste0(prettyNum(round((x / 1000), 1),
          big.mark = ",", scientific = FALSE), "K"),
        prettyNum(round(x))
      )
    )
  )
  if (dollars) {
    return(paste0("$", res))
  } else {
    return(res)
  }
}

datatable <- function(dat, vars, id, pagesize = 15) {
  if (is.null(dat))
    return(NULL)

  columns <- list()
  for (vr in vars) {
    cur_row <- var_lookup[var_lookup$name == vr, ]
    if (cur_row$type == "none") {
      fmt <- colFormat()
    } else if (cur_row$type == "number") {
      fmt <- colFormat(digits = 2, separators = TRUE)
    } else if (cur_row$type == "currency") {
      fmt <- colFormat(digits = 2, currency = "USD", separators = TRUE)
    }
    columns[[vr]] <- colDef(name = cur_row$desc, format = fmt, minWidth = cur_row$minwidth,
      sticky = if (vr == "name") "left" else NULL,
      style = "border-right: 1px solid lightgray")
  }

  htmltools::browsable(
    tagList(
      reactable(
        as_tibble(dat)[, vars],
        searchable = TRUE,
        defaultPageSize = pagesize,
        elementId = paste0(id, "-table"),
        theme = reactableTheme(
          style = list(
            fontFamily = "PT Mono, Segoe UI, Helvetica, Arial, sans-serif"
          ),
          headerStyle = list(
            borderRight = "1px solid lightgray",
            background = "#efefef",
            paddingTop = "3px"
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

get_bins <- function(x) {
  # calculate bins by roughly equal number of observations in each
  tmp <- sort(x[!is.na(x)])
  tmp2 <- tmp[seq(1, length(tmp), length = 8)]
  unique(unlist(lapply(seq_along(tmp2), function(ii) {
    if (ii == 1)
      return(pretty(tmp2[ii])[1])
    pretty(tmp2[ii])[2]
  })))
}

# create leaflet map
make_map <- function(data, variable, country_meta) {
  if (is.null(data))
    return(NULL)

  plot_var <- data[[variable]]
  plot_var <- ifelse(is.infinite(plot_var), NA, plot_var)

  # mybins <- c(0, 10, 20, 50, 100, 500, Inf)
  # mybins <- c(0, 2, 5, 10, 20, 50, 100, 500, Inf)
  mybins <- get_bins(plot_var)
  pal1 <- colorBin(
    palette = "viridis",
    domain = plot_var,
    na.color = "transparent",
    bins = mybins)

  # pal1 <- colorNumeric("viridis", plot_var)
  leaflet(data) %>%
    addTiles() %>%
    setView(
      lat = country_meta$lat,
      lng = country_meta$lng,
      zoom = country_meta$zoom
    ) %>%
    addPolygons(
      # fillColor = ~mypalette(data$km_target),
      fillColor = ~pal1(plot_var),
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
      values = ~plot_var,
      opacity = 0.9,
      # title = "Target Area (KM2)",
      title = var_lookup$desc[var_lookup$name == variable],
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
