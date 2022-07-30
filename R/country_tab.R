# this file contains a shiny module for each country tab
# reference: https://mastering-shiny.org/scaling-modules.html
library(shinycssloaders)

tab_style <- "height: calc(100vh - 154px); overflow: auto;"

#' @param id country ID (used to set the tab ID)
country_tab_ui <- function(id, country_meta) {
  ns <- shiny::NS(id)
  div(
    class = "px-3",
    div(class = "row",
      div(
        class = "col",
        style = "height: calc(100vh - 112px); max-width: 320px; background: #ededed; margin-left: 12px; padding-top: 10px; padding-bottom: 15px;",
        class = "border-0 overflow-auto",
        h3(strong("Primary Cost Inputs", align = "left"), class = "mt-0"),
        strong("Please enter the estimated cost for each program phase:"),
        br(), br(),
        em("All values are in US dollars per kilometer squared."),
        br(), br(),
        em("Suggested ranges for each phase are included below."),
        br(), br(),
        numericInput(ns("PLANNING"), span("Planning Cost:",
          em(style = "font-weight: normal;", "($1,800-$2,900)")),
          value = country_meta$planning, min = 0, max = 5000),
        numericInput(ns("PREP"), span("Preparation Cost:",
          em(style = "font-weight: normal;", "($5,700-$8,000)")),
          value = country_meta$prep, min = 0, max = 12000),
        numericInput(ns("PRODUCTION"), span("Production Cost:",
          em(style = "font-weight: normal;", "($9,400-$13,300)")),
          value = country_meta$production, min = 0, max = 20000),
        numericInput(ns("DISTRIBUTION"), span("Distribution Cost:",
          em(style = "font-weight: normal;", "($1,400-$2,200)")),
          value = country_meta$distribution, min = 0, max = 5000),
        numericInput(ns("RELEASE"), span("Release Cost:",
          em(style = "font-weight: normal;", "($3,200-$4,400)")),
          value = country_meta$release, min = 0, max = 10000),
        numericInput(ns("MONITORING"), span("Monitoring Cost:",
          em(style = "font-weight: normal;", "($5,000-$7,000)")),
          value = country_meta$monitoring, min = 0, max = 10000),
        h3(strong("Secondary Inputs", align = "left")),
        sliderInput(ns("EFFECTIVENESS"), "Effectiveness:",
          min = 0, max = 100, value = 77),
        sliderInput(ns("AREACOV"), "Area Coverage:",
            min = 0, max = 100,
            value = 80),
        sliderInput(ns("PCT_AMB"), "Percent Treated in Ambulatory Setting:",
            min = 0, max = 100,
            value = country_meta$pct_trt_amb * 100),
        sliderInput(ns("PCT_HOSP"), "Percent Treated in Hospital Setting:",
            min = 0, max = 100,
            value = country_meta$pct_trt_hosp * 100),
        numericInput(ns("MORT_RATE"), "Mortality Rate (%):",
            min = 0, max = 100,
            value = country_meta$mortality * 100),
        conditionalPanel(
          condition = "input.tabset != 'BF'",
          numericInput(ns("PREV_RATE"), "Prevalence Rate (%):",
            min = 0, max = 100,
            value = country_meta$prevalence * 100),
        ),
        numericInput(ns("COST_AMB"), "Cost per Ambulatory Visit:",
          value = country_meta$cost_per_amb_case, min = 0, max = 1000000),
        numericInput(ns("COST_HOSP"), "Cost per Hospitalized Visit:",
          value = country_meta$cost_per_hosp_case, min = 0, max = 1000000),
        numericInput(ns("COST_DEATH"), "Cost per Death:",
          value = country_meta$cost_per_child_fat, min = 0, max = 1000000),
        actionButton(ns("submit"), "Submit")
      ),
      div(
        class = "col",

        tabsetPanel(id = "country_tabset",
          tabPanel("Map of Relevant Program Areas",
            style = tab_style,
            h2("Relevant Mosquito Release Program Areas"),
            ifelse(id == "BF",
              "The following map shows areas where mosquito release programs
              for malaria control may be most useful based on high prevalence.
              The colors indicate the cost per disease case averted by the
              intervention.",
              "The following map shows areas where mosquito release programs
              for dengue control may be most useful based on population
              density. The colors indicate the cost per disease case averted
              by the intervention."
            ),
            withSpinner(leafletOutput(ns("mymap"), height = "700px")),
            h2("Coverage Indicators"),
            "The following table shows each geography's coverage outputs:
            area covered by the program, people covered by the program, and
            cost per person covered.",
            withSpinner(dataTableOutput(ns("outputs")))
          ),
          tabPanel("Key Cost Indicators",
            style = tab_style,
            h2("Key Cost Indicators"),
            "The following table shows each geography's target area
            (in kilometers squared), total cost to cover the target area, cost
            per person covered by the intervention, cost per case averted by
            the intervention and cost per DALY averted by the intervention.",
            withSpinner(dataTableOutput(ns("keyindicators"))),
            h2("Cost Per Case Averted vs. Cost Per DALY Averted"),
            "The following plot shows cost per case vs. cost per DALY
            by geography",
            withSpinner(plotlyOutput(ns("keyindicatorsplot"))),
            h2("Total Annual Cost"),
            "The following plot shows the total annual cost by geography",
            withSpinner(plotlyOutput(ns("keyindicatorsplot2")))
          ),
          tabPanel("Health Outcomes",
            style = tab_style,
            h2("Health Outcomes"),
            "The following table shows each geography's disease prevalence,
            total cases, cost per case averted, total DALYs, cost per DALY
            averted.",
            withSpinner(dataTableOutput(ns("healthoutcomesdata"))),
            h2("Cost Per Case Averted"),
            "The following plot shows each geography's cost per case averted.",
            withSpinner(plotlyOutput(ns("healthoutcomesdataplot"))),
            h2("Total Cases in Target Area"),
            "The following plot shows the total cases in each target area.",
            withSpinner(plotlyOutput(ns("healthoutcomesdataplot2")))
          ),
          tabPanel("Health System Costs",
            style = tab_style,
            h2("Health System Costs Averted"),
            "The following table shows each geography's total ambulatory cases
            and costs averted, hospitalized cases and costs averted, and total
            health system costs averted with a successful mosquito release
            intervention.",
            withSpinner(dataTableOutput(ns("healthsystemoutcomesdata"))),
            h2("Ambulatory Costs Averted"),
            "The following plot shows total ambulatory costs averted in each
            geography.",
            withSpinner(plotlyOutput(ns("healthsystemoutcomesdataplot"))),
            h2("Hospital Costs Averted"),
            "The following plot shows total hospital costs averted in each
            geography.",
            withSpinner(plotlyOutput(ns("healthsystemoutcomesdataplot2")))
          ),
          tabPanel("Economic Costs",
            style = tab_style,
            h2("Economic Costs Averted"),
            "The following table shows each geography's total deaths, cost per
            death averted, and economic losses due to disease fatalities.",
            withSpinner(dataTableOutput(ns("economicoutcomesdata"))),
            withSpinner(plotlyOutput(ns("economicoutcomesdataplot")))
          )
        )
      )
    )
  )
}

#' @param id country ID (used to access the appropriate data)
#' @param dataset
country_tab_server <- function(id, dataset, tab, country_meta) {
  moduleServer(id, function(input, output, session) {
    adm_names <- c(
      country_meta$admin1_name,
      country_meta$admin2_name,
      country_meta$admin3_name
    )
    first_admin <- head(adm_names, 1)
    last_admin <- tail(adm_names, 1)

    dt_opts5 <- list(
      searching = TRUE,
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20))

    dt_opts10 <- list(
      searching = TRUE,
      pageLength = 10,
      lengthMenu = c(5, 10, 15, 20))

    # selects current data based on the tab that is being viewed
    # dataset is a named list of datasets with name corresponding to tab value
    cur_dat <- reactive({
      if (tab() != id)
        return(NULL)
      res <- dataset[[tab()]]
      # rename administrative entities
      nms <- names(res)
      names(res)[nms == "adm1_name"] <- country_meta$admin1_name
      names(res)[nms == "adm2_name"] <- country_meta$admin2_name
      # if there is no ADM3_Name, this won't do anything
      names(res)[nms == "adm3_name"] <- country_meta$admin3_name
      res
    })

    # augment the current dataset with variables derived from user inputs
    cur_dat_aug <- eventReactive(list(input$submit, cur_dat()), {
      if (is.null(cur_dat()))
        return(NULL)
      update_data_from_inputs(
        cur_dat(),
        PLANNING = as.numeric(input$PLANNING),
        PREP = as.numeric(input$PREP),
        PROD = as.numeric(input$PRODUCTION),
        DIST = as.numeric(input$DISTRIBUTION),
        MONITOR = as.numeric(input$MONITORING),
        RELEASE = as.numeric(input$RELEASE),
        EFF = as.numeric(input$EFFECTIVENESS) / 100,
        AREACOV = as.numeric(input$AREACOV) / 100,
        PCT_AMB = as.numeric(input$PCT_AMB) / 100,
        PCT_HOSP = as.numeric(input$PCT_HOSP) / 100,
        MORT_RATE = as.numeric(input$MORT_RATE) / 100,
        PREV_RATE = as.numeric(input$PREV_RATE) / 100,
        COST_AMB = as.numeric(input$COST_AMB),
        COST_HOSP = as.numeric(input$COST_HOSP),
        COST_DEATH = as.numeric(input$COST_DEATH),
        country_meta
      )
    })

    # reactive data frames used in outputs
    outputs <- reactive({
      if (is.null(cur_dat_aug()))
        return(NULL)
      cur_dat_aug() %>%
        as.data.frame() %>%
        select(any_of(c(adm_names,
          "km_target", "x_pdmean", "cost_per_pers_cov"))) %>%
        mutate(across(any_of(c("km_target", "x_pdmean",
          "cost_per_pers_cov")), round, 1)) %>%
        rename(
          "Target Area (KM2)" = km_target,
          "Cost per person" = cost_per_pers_cov,
          "Population in Target Area" = x_pdmean
        )
    })

    keyindicators <- reactive({
      if (is.null(cur_dat_aug()))
        return(NULL)
      cur_dat_aug() %>%
        as.data.frame() %>%
        select(any_of(c(adm_names, "km_target",
          "tot_ann_cost_target", "cost_per_pers_cov",
          "cost_per_case_avert",  "cost_per_daly_avert"))) %>%
        mutate(across(any_of(c("km_target", "tot_ann_cost_target",
          "cost_per_pers_cov", "cost_per_case_avert",
          "cost_per_daly_avert")), round, 0)) %>%
        rename(
          "Target Area (KM2)" = km_target,
          "Total" = tot_ann_cost_target,
          "Per person" = cost_per_pers_cov,
          "Per case" = cost_per_case_avert,
          "Per DALY" = cost_per_daly_avert
        )
    })

    healthoutcomes <- reactive({
      if (is.null(cur_dat_aug()))
        return(NULL)
      cur_dat_aug() %>%
        as.data.frame() %>%
        select(any_of(c(adm_names, "prev_inc_m",
          "case_target_area", "cost_per_case_avert",
          "daly_target_area",  "cost_per_daly_avert"))) %>%
        mutate(across(any_of(c("Cases", "cost_per_case_avert", "DALYs",
          "cost_per_daly_avert")), round, 0)) %>%
        rename(
          Prevalence = prev_inc_m,
          Cases = case_target_area,
          "Cost per case" = cost_per_case_avert,
          DALYs = daly_target_area,
          "Cost per DALY" = cost_per_daly_avert
        )
    })

    healthsystemoutcomes <- reactive({
      if (is.null(cur_dat_aug()))
        return(NULL)
      cur_dat_aug() %>%
        as.data.frame() %>%
        select(any_of(c(adm_names, "amb_cases", "hosp_cases",
          "amb_cost_avert", "hosp_cost_avert",
          "tot_health_sys_cost_avert"))) %>%
        mutate(across(any_of(c("amb_cases", "hosp_cases", "amb_cost_avert",
          "hosp_cost_avert", "tot_health_sys_cost_avert")), round, 0)) %>%
        rename(
          "Ambulatory Cases" = amb_cases,
          "Hospitalized Cases" = hosp_cases,
          "Ambulatory Costs" = amb_cost_avert,
          "Hospital Costs" = hosp_cost_avert,
          "Total" = "tot_health_sys_cost_avert"
        )
    })

    economicoutcomes <- reactive({
      if (is.null(cur_dat_aug()))
        return(NULL)
      cur_dat_aug() %>%
        as.data.frame() %>%
        select(any_of(c(adm_names, "death_target_area",
          "cost_per_death_avert", "econ_loss_death"))) %>%
        mutate(across(any_of(c("death_target_area", "cost_per_death_avert",
          "econ_loss_death")), round, 0)) %>%
        rename(
          "Cost per death averted" = cost_per_death_avert,
          "Deaths" = death_target_area,
          "Economic losses" = econ_loss_death
        )
    })

    # make_map() defined later in this file
    output$mymap <- renderLeaflet(make_map(cur_dat_aug(),
      country_meta, last_admin))

    output$outputs <- shiny::renderDataTable(outputs(), options = dt_opts10)

    output$keyindicators <- shiny::renderDataTable(keyindicators(),
      options = dt_opts5)

    output$keyindicatorsplot <- renderPlotly({
      if (is.null(keyindicators()))
        return(NULL)
      ggplot(data = keyindicators() %>% filter(`Per case` < 1000),
        aes_string(x = "`Per DALY`", y = "`Per case`",
          color = first_admin, label = last_admin)) +
        geom_point(na.rm = TRUE) +
        # ggthemes::scale_color_tableau() +
        # xlim(0, 10) +
        # ylim(0, 10) +
        theme_classic() +
        labs(
          y = "Cost Per Case Averted (USD)",
          x = "Cost per DALY Averted (USD)") +
        ggtitle(paste("Key Cost Indicators Colored by", first_admin,
          "(with cost per case < 1000)"))
    })

    output$keyindicatorsplot2 <- renderPlotly({
      if (is.null(keyindicators()))
        return(NULL)
      keyindicators() %>%
        arrange(-Total) %>%
        mutate(rank = seq_len(n())) %>%
      ggplot(
        aes_string(x = "rank", y = "Total", color = first_admin,
          label = last_admin)) +
        geom_point(na.rm = TRUE) +
        # ggthemes::scale_color_tableau() +
        # ylim(0, 100000000) +
        theme_classic() +
        labs(
          y = "Total Annual Cost for Target Area (USD)",
          x = paste(first_admin, "Rank")) +
        ggtitle(paste("Total Annual Program Cost Colored by", first_admin)) +
        theme(axis.text.x = element_text(angle = 45))
    })

    output$healthoutcomesdata <- renderDataTable(healthoutcomes(),
      options = dt_opts5)

    output$healthoutcomesdataplot <- renderPlotly({
      if (is.null(healthoutcomes()))
        return(NULL)
      healthoutcomes() %>%
        arrange(desc(.data$"Cost per case")) %>%
        mutate(rank = seq_len(n())) %>%
      ggplot(
        aes_string(x = "rank", y = "`Cost per case`",
          color = first_admin, label = last_admin)) +
        geom_point(na.rm = TRUE) +
        # ggthemes::scale_color_tableau() +
        # ylim(0, 10) +
        theme_classic() +
        labs(
          y = "Cost Per Case Averted (USD)",
          x = paste(last_admin, "Rank"),
          title = paste0("Cost Per Case Averted Colored by", first_admin)) +
        theme(axis.text.x = element_text(angle = 45))
    })

    output$healthoutcomesdataplot2 <- renderPlotly({
      if (is.null(healthoutcomes()))
        return(NULL)
      healthoutcomes() %>%
        arrange(desc(.data[["Cases"]])) %>%
        mutate(rank = seq_len(n())) %>%
      ggplot(
        aes_string(x = "rank", y = "Cases", color = first_admin,
          label = last_admin)) +
        geom_point(na.rm = TRUE) +
        # ggthemes::scale_color_tableau() +
        # ylim(0, 10000) +
        theme_classic() +
        labs(
          y = "Total Cases in Target Area",
          x = paste(last_admin, "Rank")) +
        ggtitle("Total Cases in Target Area") +
        theme(axis.text.x = element_text(angle = 45))
    })

    output$healthsystemoutcomesdata <- renderDataTable(healthsystemoutcomes(),
      options = dt_opts5)

    output$healthsystemoutcomesdataplot <- renderPlotly({
      if (is.null(healthsystemoutcomes()))
        return(NULL)
      healthsystemoutcomes() %>%
        arrange(desc(.data$"Ambulatory Costs")) %>%
        mutate(rank = seq_len(n())) %>%
      ggplot(
        aes_string(x = "rank", y = "`Ambulatory Costs`",
          color = first_admin, label = last_admin)) +
        geom_point(na.rm = TRUE) +
        # ggthemes::scale_color_tableau() +
        # ylim(0, 600000) +
        theme_classic() +
        labs(
          y = "Total Ambulatory Costs in Target Area",
          x = paste(last_admin, "Rank")) +
        ggtitle("Ambulatory Costs Averted in Target Area") +
        theme(axis.text.x = element_text(angle = 45))
    })

    output$healthsystemoutcomesdataplot2 <- renderPlotly({
      if (is.null(healthsystemoutcomes()))
        return(NULL)
      healthsystemoutcomes() %>%
        arrange(desc(.data$"Hospital Costs")) %>%
        mutate(rank = seq_len(n())) %>%
      ggplot(
        aes_string(x = "rank", y = "`Hospital Costs`",
          color = first_admin, label = last_admin)) +
        geom_point(na.rm = TRUE) +
        # ggthemes::scale_color_tableau() +
        # ylim(0, 600000) +
        theme_classic() +
        labs(
          y = "Total Hospitalized Costs in Target Area",
          x = paste(last_admin, "Rank")) +
        ggtitle("Hospitalized Costs Averted in Target Area") +
        theme(axis.text.x = element_text(angle = 45))
    })

    output$economicoutcomesdata <- renderDataTable(economicoutcomes(),
      options = dt_opts5)

    output$economicoutcomesdataplot <- renderPlotly({
      if (is.null(economicoutcomes()))
        return(NULL)
      economicoutcomes() %>%
        arrange(desc(.data$"Economic losses")) %>%
        mutate(rank = seq_len(n())) %>%
      ggplot(
        aes_string(x = "rank", y = "`Economic losses`",
          color = first_admin, label = last_admin)) +
        geom_point(na.rm = TRUE) +
        # ggthemes::scale_color_tableau() +
        # ylim(0, 1000000) +
        theme_classic() +
        labs(
          y = "Total Economic Losses",
          x = paste(last_admin, "Rank")) +
        ggtitle(paste0("Economic Losses Averted in Target Areas of ",
          last_admin, "s")) +
        theme(axis.text.x = element_text(angle = 45))
    })

    # update leaflet plot when user updates inputs
    observeEvent(input$submit, {
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
  })
}

# update data frame with user inputs
update_data_from_inputs <- function(
  dat, PLANNING, PREP, PROD, DIST, MONITOR, RELEASE, EFF, AREACOV,
  PCT_AMB, PCT_HOSP, MORT_RATE, PREV_RATE, COST_AMB, COST_HOSP,
  COST_DEATH, country_meta
) {
  if (is.null(dat))
    return(NULL)

  # total cost per kilometer squared, which corresponds to user input
  dat$tot_ann_cost_km <- PLANNING + PREP + PROD + DIST + MONITOR + RELEASE
  dat$eff <- EFF

  dat$case_target_area <- dat$x_pdmean * PREV_RATE
  dat$prev_inc_m <- PREV_RATE
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
      position = "bottomleft")
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
