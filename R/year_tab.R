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
  id, country_id, dataset, tab, rv, country_meta, primaryinputtype, submit
) {
  moduleServer(id, function(input, output, session) {
    adm_names <- c(
      country_meta$admin1_name,
      country_meta$admin2_name,
      country_meta$admin3_name
    )
    first_admin <- head(adm_names, 1)
    last_admin <- tail(adm_names, 1)

    output$mytext <- renderText({
      rv()$PREP
    })

    # selects current data based on the tab that is being viewed
    # dataset is a named list of datasets with name corresponding to tab value
    cur_dat <- reactive({
      # message("tab: ", tab())
      # message("id: ", id)
      if (tab() != country_id || is.null(rv()$POPDENSITY))
        return(NULL)
      res <- dataset[[tab()]][[rv()$POPDENSITY]]
      # rename administrative entities
      nms <- names(res)
      names(res)[nms == "adm1_name"] <- country_meta$admin1_name
      names(res)[nms == "adm2_name"] <- country_meta$admin2_name
      # if there is no ADM3_Name, this won't do anything
      names(res)[nms == "adm3_name"] <- country_meta$admin3_name
      res
    })

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

        update_data_from_inputs(
          cur_dat(),
          PLANNING = PLANNING,
          PREP = PREP,
          PROD = PROD,
          DIST = DIST,
          MONITOR = MONITOR,
          RELEASE = RELEASE,
          EFF = as.numeric(rv()$EFFECTIVENESS) / 100,
          AREACOV = as.numeric(rv()$AREACOV) / 100,
          PCT_AMB = as.numeric(rv()$PCT_AMB) / 100,
          PCT_HOSP = as.numeric(rv()$PCT_HOSP) / 100,
          MORT_RATE = as.numeric(rv()$MORT_RATE) / 100,
          # PREV_RATE = country_meta$prevalence,
          COST_AMB = as.numeric(rv()$COST_AMB),
          COST_HOSP = as.numeric(rv()$COST_HOSP),
          COST_DEATH = as.numeric(rv()$COST_DEATH),
          country_meta
        )
      })
    }, ignoreNULL = FALSE)

    output$mymap <- renderLeaflet(make_map(cur_dat_aug(),
      country_meta, last_admin))

    # update leaflet plot when user updates inputs
    observeEvent(submit(), {
      dat <- cur_dat_aug()
      if (!is.null(dat)) {
        cost <- dat$cost_per_case_avert
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
            label = maptooltips(dat, last_admin), # function defined below
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
      }
    }, ignoreNULL = FALSE)

    totalbudget <- reactive({
      if (is.null(cur_dat_aug())) {
        return(NULL)
      }
      res <- sum(cur_dat_aug()$tot_ann_cost_target)
      res <- prettyNum(round(res / 1e6, 1), big.mark = ",", scientific = FALSE)
      paste0("$", format(res, trim = TRUE), "M")
    })

    casesavertednum <- reactive({
      if (is.null(cur_dat_aug())) {
        return(NULL)
      }
      # TODO: check why there are NAs
      sum(cur_dat_aug()$case_target_area, na.rm = TRUE)
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
      if (is.null(casesavertednum())) {
        return(NULL)
      }
      round(100 * casesavertednum() / country_meta$total_national_cases, 1)
    })

    output$totalbudget <- renderText(totalbudget())
    output$casesaverted <- renderText(casesaverted())
    output$pctaverted <- renderText(pctaverted())

    # # reactive data frames used in outputs
    # outputs <- reactive({
    #   if (is.null(cur_dat_aug()))
    #     return(NULL)
    #   cur_dat_aug() %>%
    #     as.data.frame() %>%
    #     select(any_of(c(adm_names,
    #       "km_target", "x_pdmean", "cost_per_pers_cov")))
    # })

    # keyindicators <- reactive({
    #   if (is.null(cur_dat_aug()))
    #     return(NULL)
    #   cur_dat_aug() %>%
    #     as.data.frame() %>%
    #     select(any_of(c(adm_names, "km_target",
    #       "tot_ann_cost_target", "cost_per_pers_cov",
    #       "cost_per_case_avert",  "cost_per_daly_avert")))
    # })



    # healthoutcomes <- reactive({
    #   if (is.null(cur_dat_aug()))
    #     return(NULL)
    #   cur_dat_aug() %>%
    #     as.data.frame() %>%
    #     select(any_of(c(adm_names, "prev_inc_m",
    #       "case_target_area", "cost_per_case_avert",
    #       "daly_target_area",  "cost_per_daly_avert")))
    # })

    # healthsystemoutcomes <- reactive({
    #   if (is.null(cur_dat_aug()))
    #     return(NULL)
    #   cur_dat_aug() %>%
    #     as.data.frame() %>%
    #     select(any_of(c(adm_names, "amb_cases", "hosp_cases",
    #       "amb_cost_avert", "hosp_cost_avert",
    #       "tot_health_sys_cost_avert")))
    # })

    # economicoutcomes <- reactive({
    #   if (is.null(cur_dat_aug()))
    #     return(NULL)
    #   cur_dat_aug() %>%
    #     as.data.frame() %>%
    #     select(any_of(c(adm_names, "death_target_area",
    #       "cost_per_death_avert", "econ_loss_death")))
    # })

    # # make_map() defined later in this file
    # output$mymap <- renderLeaflet(make_map(cur_dat_aug(),
    #   country_meta, last_admin))

    # # output$outputs <- shiny::renderDataTable(outputs(), options = dt_opts10)

    # output$outputs <- shiny::renderUI({
    #   datatable(
    #     outputs(),
    #     id = "outputs",
    #     columns = list(
    #       km_target = colDef(name = "Target Area (KM2)",
    #         format = colFormat(separators = TRUE, digits = 1)),
    #       cost_per_pers_cov = colDef(name = "Cost per person",
    #         format = colFormat(separators = TRUE, currency = "USD")),
    #       x_pdmean = colDef(name = "Population in Target Area",
    #         format = colFormat(separators = TRUE, digits = 1))
    #     )
    #   )
    # })

    # output$keyindicators <- shiny::renderUI({
    #   datatable(
    #     keyindicators(),
    #     id = "keyindicators",
    #     columns = list(
    #       km_target = colDef(name = "Target Area (KM2)",
    #         format = colFormat(separators = TRUE, digits = 1)),
    #       tot_ann_cost_target = colDef(name = "Total",
    #         format = colFormat(separators = TRUE, currency = "USD")),
    #       cost_per_pers_cov = colDef(name = "Per person",
    #         format = colFormat(separators = TRUE, currency = "USD"),
    #         width = 120),
    #       cost_per_case_avert = colDef(name = "Per case",
    #         format = colFormat(separators = TRUE, currency = "USD"),
    #         width = 120),
    #       cost_per_daly_avert = colDef(name = "Per DALY",
    #         format = colFormat(separators = TRUE, currency = "USD"),
    #         width = 120)
    #     )
    #   )
    # })

    # # output$keyindicatorsplot <- renderPlotly({
    # #   if (is.null(keyindicators()))
    # #     return(NULL)
    # #   ggplot(data = keyindicators() %>% filter(`Per case` < 1000),
    # #     aes_string(x = "`Per DALY`", y = "`Per case`",
    # #       color = first_admin, label = last_admin)) +
    # #     geom_point(na.rm = TRUE) +
    # #     # ggthemes::scale_color_tableau() +
    # #     # xlim(0, 10) +
    # #     # ylim(0, 10) +
    # #     theme_classic() +
    # #     labs(
    # #       y = "Cost Per Case Averted (USD)",
    # #       x = "Cost per DALY Averted (USD)") +
    # #     ggtitle(paste("Key Cost Indicators Colored by", first_admin,
    # #       "(with cost per case < 1000)"))
    # # })

    # # output$keyindicatorsplot2 <- renderPlotly({
    # #   if (is.null(keyindicators()))
    # #     return(NULL)
    # #   keyindicators() %>%
    # #     arrange(-Total) %>%
    # #     mutate(rank = seq_len(n())) %>%
    # #   ggplot(
    # #     aes_string(x = "rank", y = "Total", color = first_admin,
    # #       label = last_admin)) +
    # #     geom_point(na.rm = TRUE) +
    # #     # ggthemes::scale_color_tableau() +
    # #     # ylim(0, 100000000) +
    # #     theme_classic() +
    # #     labs(
    # #       y = "Total Annual Cost for Target Area (USD)",
    # #       x = paste(first_admin, "Rank")) +
    # #     ggtitle(paste("Total Annual Program Cost Colored by", first_admin)) +
    # #     theme(axis.text.x = element_text(angle = 45))
    # # })

    # output$healthoutcomesdata <- shiny::renderUI({
    #   datatable(
    #     healthoutcomes(),
    #     id = "healthoutcomes",
    #     columns = list(
    #       prev_inc_m = colDef(name = "Incidence", width = 150),
    #       case_target_area = colDef(name = "Cases",
    #         format = colFormat(separators = TRUE, digits = 0),
    #         width = 120),
    #       cost_per_case_avert = colDef(name = "Cost per case",
    #         format = colFormat(separators = TRUE, currency = "USD"),
    #         width = 150),
    #       daly_target_area = colDef(name = "DALYs",
    #         format = colFormat(separators = TRUE, digits = 0),
    #         width = 130),
    #       cost_per_daly_avert = colDef(name = "Cost per DALY",
    #         format = colFormat(separators = TRUE, currency = "USD"),
    #         width = 150)
    #     )
    #   )
    # })

    # output$totalbudget <- renderText(totalbudget())
    # output$casesaverted <- renderText(casesaverted())
    # output$pctaverted <- renderText(pctaverted())

    # # output$healthoutcomesdataplot <- renderPlotly({
    # #   if (is.null(healthoutcomes()))
    # #     return(NULL)
    # #   healthoutcomes() %>%
    # #     arrange(desc(.data$"Cost per case")) %>%
    # #     mutate(rank = seq_len(n())) %>%
    # #   ggplot(
    # #     aes_string(x = "rank", y = "`Cost per case`",
    # #       color = first_admin, label = last_admin)) +
    # #     geom_point(na.rm = TRUE) +
    # #     # ggthemes::scale_color_tableau() +
    # #     # ylim(0, 10) +
    # #     theme_classic() +
    # #     labs(
    # #       y = "Cost Per Case Averted (USD)",
    # #       x = paste(last_admin, "Rank"),
    # #       title = paste0("Cost Per Case Averted Colored by", first_admin)) +
    # #     theme(axis.text.x = element_text(angle = 45))
    # # })

    # # output$healthoutcomesdataplot2 <- renderPlotly({
    # #   if (is.null(healthoutcomes()))
    # #     return(NULL)
    # #   healthoutcomes() %>%
    # #     arrange(desc(.data[["Cases"]])) %>%
    # #     mutate(rank = seq_len(n())) %>%
    # #   ggplot(
    # #     aes_string(x = "rank", y = "Cases", color = first_admin,
    # #       label = last_admin)) +
    # #     geom_point(na.rm = TRUE) +
    # #     # ggthemes::scale_color_tableau() +
    # #     # ylim(0, 10000) +
    # #     theme_classic() +
    # #     labs(
    # #       y = "Total Cases in Target Area",
    # #       x = paste(last_admin, "Rank")) +
    # #     ggtitle("Total Cases in Target Area") +
    # #     theme(axis.text.x = element_text(angle = 45))
    # # })

    # output$healthsystemoutcomesdata <- shiny::renderUI({
    #   datatable(
    #     healthsystemoutcomes(),
    #     id = "healthsystemoutcomes",
    #     columns = list(
    #       amb_cases = colDef(name = "Ambulatory Cases",
    #         format = colFormat(separators = TRUE, digits = 0)),
    #       hosp_cases = colDef(name = "Hospitalized Cases",
    #         format = colFormat(separators = TRUE, digits = 0)),
    #       amb_cost_avert = colDef(name = "Ambulatory Costs",
    #         format = colFormat(separators = TRUE, currency = "USD")),
    #       hosp_cost_avert = colDef(name = "Hospital Costs",
    #         format = colFormat(separators = TRUE, currency = "USD")),
    #       tot_health_sys_cost_avert = colDef(name = "Total",
    #         format = colFormat(separators = TRUE, currency = "USD"))
    #     )
    #   )
    # })

    # # output$healthsystemoutcomesdataplot <- renderPlotly({
    # #   if (is.null(healthsystemoutcomes()))
    # #     return(NULL)
    # #   healthsystemoutcomes() %>%
    # #     arrange(desc(.data$"Ambulatory Costs")) %>%
    # #     mutate(rank = seq_len(n())) %>%
    # #   ggplot(
    # #     aes_string(x = "rank", y = "`Ambulatory Costs`",
    # #       color = first_admin, label = last_admin)) +
    # #     geom_point(na.rm = TRUE) +
    # #     # ggthemes::scale_color_tableau() +
    # #     # ylim(0, 600000) +
    # #     theme_classic() +
    # #     labs(
    # #       y = "Total Ambulatory Costs in Target Area",
    # #       x = paste(last_admin, "Rank")) +
    # #     ggtitle("Ambulatory Costs Averted in Target Area") +
    # #     theme(axis.text.x = element_text(angle = 45))
    # # })

    # # output$healthsystemoutcomesdataplot2 <- renderPlotly({
    # #   if (is.null(healthsystemoutcomes()))
    # #     return(NULL)
    # #   healthsystemoutcomes() %>%
    # #     arrange(desc(.data$"Hospital Costs")) %>%
    # #     mutate(rank = seq_len(n())) %>%
    # #   ggplot(
    # #     aes_string(x = "rank", y = "`Hospital Costs`",
    # #       color = first_admin, label = last_admin)) +
    # #     geom_point(na.rm = TRUE) +
    # #     # ggthemes::scale_color_tableau() +
    # #     # ylim(0, 600000) +
    # #     theme_classic() +
    # #     labs(
    # #       y = "Total Hospitalized Costs in Target Area",
    # #       x = paste(last_admin, "Rank")) +
    # #     ggtitle("Hospitalized Costs Averted in Target Area") +
    # #     theme(axis.text.x = element_text(angle = 45))
    # # })

    # output$economicoutcomesdata <- shiny::renderUI({
    #   datatable(
    #     economicoutcomes(),
    #     id = "economicoutcomes",
    #     columns = list(
    #       cost_per_death_avert = colDef(name = "Cost per death averted",
    #         format = colFormat(separators = TRUE, currency = "USD")),
    #       death_target_area = colDef(name = "Deaths",
    #         format = colFormat(separators = TRUE, digits = 0)),
    #       econ_loss_death = colDef(name = "Economic losses",
    #         format = colFormat(separators = TRUE, currency = "USD"))
    #     )
    #   )
    # })

    # # output$economicoutcomesdataplot <- renderPlotly({
    # #   if (is.null(economicoutcomes()))
    # #     return(NULL)
    # #   economicoutcomes() %>%
    # #     arrange(desc(.data$"Economic losses")) %>%
    # #     mutate(rank = seq_len(n())) %>%
    # #   ggplot(
    # #     aes_string(x = "rank", y = "`Economic losses`",
    # #       color = first_admin, label = last_admin)) +
    # #     geom_point(na.rm = TRUE) +
    # #     # ggthemes::scale_color_tableau() +
    # #     # ylim(0, 1000000) +
    # #     theme_classic() +
    # #     labs(
    # #       y = "Total Economic Losses",
    # #       x = paste(last_admin, "Rank")) +
    # #     ggtitle(paste0("Economic Losses Averted in Target Areas of ",
    # #       last_admin, "s")) +
    # #     theme(axis.text.x = element_text(angle = 45))
    # # })



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

# update data frame with user inputs
update_data_from_inputs <- function(
  dat, PLANNING, PREP, PROD, DIST, MONITOR, RELEASE, EFF, AREACOV,
  PCT_AMB, PCT_HOSP, MORT_RATE, COST_AMB, COST_HOSP,
  COST_DEATH, country_meta
) {
  if (is.null(dat))
    return(NULL)

  # total cost per kilometer squared, which corresponds to user input
  dat$tot_ann_cost_km <- PLANNING + PREP + PROD + DIST + MONITOR + RELEASE
  dat$eff <- EFF

  dat$case_target_area <- dat$x_pdmean * dat$incidence
  dat$prev_inc_m <- dat$incidence
  # dat$case_target_area <- dat$x_pdmean * PREV_RATE
  # dat$prev_inc_m <- PREV_RATE

  dat$death_target_area <- dat$case_target_area * MORT_RATE
  dat$daly_target_area <- dat$case_target_area * country_meta$daly_per_case

  ## building other variables related to what the user puts in
  dat$tot_ann_cost_target <- dat$tot_ann_cost_km * dat$km_target * AREACOV
  dat$cost_per_pers_cov <- dat$tot_ann_cost_target / dat$x_pdmean
  dat$cost_per_case_avert <- dat$tot_ann_cost_target /
    (dat$case_target_area * dat$eff)
  dat$cost_per_death_avert <- dat$tot_ann_cost_target /
    (dat$death_target_area * dat$eff)
  dat$cost_per_daly_avert <- dat$tot_ann_cost_target /
    (dat$daly_target_area * dat$eff)

  dat$amb_cases <- dat$case_target_area * PCT_AMB
  dat$hosp_cases <- dat$case_target_area * PCT_HOSP
  dat$amb_cost_avert <- (dat$case_target_area * PCT_AMB) * COST_AMB
  dat$hosp_cost_avert <- (dat$case_target_area * PCT_HOSP) * COST_HOSP
  dat$tot_health_sys_cost_avert <- dat$hosp_cost_avert + dat$amb_cost_avert
  dat$econ_loss_death <- dat$death_target_area * COST_DEATH

  dat
}

# create leaflet map
make_map <- function(data, country_meta, last_admin) {
  if (is.null(data))
    return(NULL)

  cost <- data$cost_per_case_avert
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
      label = maptooltips(data, last_admin),
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
maptooltips <- function(dat, last_admin) {
  paste0(
    last_admin, ": ", dat[[last_admin]], "<br/>",
    "Target Population: ",  format_big(dat$x_pdmean), "<br/>",
    # "Incidence: ", round(dat$prev_inc_m, 3), "<br/>",
    "Total Area: ",  format_big(dat$area_km2), "<br/>",
    "Target Area: ",  format_big(dat$km_target), "<br/>",
    "Total Program Cost (User Generated): ", "$",
    format_big(dat$tot_ann_cost_target), "<br/>",
    "Cost per Person (User Generated): ", "$",
    format_big(dat$cost_per_pers_cov), "<br/>",
    "Cost per Case (User Generated): ", "$",
    format_big(dat$cost_per_case_avert), "<br/>",
    "Cost per DALY (User Generated): ", "$",
    format_big(dat$cost_per_daly_avert), "<br/>",
    "Cost per death (User Generated): ", "$",
    format_big(dat$cost_per_death_avert), "<br/>"
  ) %>%
  lapply(htmltools::HTML)
}
