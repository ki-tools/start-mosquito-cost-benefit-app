# this file contains a shiny module for each country tab
# reference: https://mastering-shiny.org/scaling-modules.html
library(shinycssloaders)

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

tab_style <- "height: calc(100vh - 220px); overflow: auto;"

#' @param id country ID (used to set the tab ID)
country_tab_ui <- function(id, country_meta) {
  ns <- shiny::NS(id)
  div(
    class = "px-3",
    div(class = "row",
      div(
        class = "col",
        style = "height: calc(100vh - 140px); max-width: 320px; background: #ededed; margin-left: 12px; padding-top: 10px; padding-bottom: 15px;",
        class = "border-0 overflow-auto",
        h3(strong("Primary Cost Inputs", align = "left"), class = "mt-0"),
        tags$p(strong("Please enter the estimated cost for each program phase:")),
        tags$p(em("All values are in US dollars per km\u00B2. Suggested ranges for each phase are included.")),
        shinyWidgets::radioGroupButtons(
          width = "100%",
          individual = TRUE,
          # justified = TRUE,
          inputId = ns("primaryinputtype"),
          label = "Primary input specification: ",
          choices = c("Activity-based" = 1, "Phase-based" = 2),
          selected = "1"
        ),
        conditionalPanel(
          condition = paste0("input['", ns("primaryinputtype"), "'] === '1'"),
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
            value = country_meta$monitoring, min = 0, max = 10000)
        ),
        conditionalPanel(
          condition = paste0("input['", ns("primaryinputtype"), "'] === '2'"),
          h5(strong("Planning")),
          numericInput(ns("PLANNING_PLAN"),
            "Define workplan and budget:",
            value = country_meta$planning_plan, min = 0, max = 5000),
          numericInput(ns("PLANNING_METHOD"),
            "Determine release methodology:",
            value = country_meta$planning_method, min = 0, max = 5000),
          h5(strong("Preparation")),
          numericInput(ns("PREP_PLAN"),
            "Complete release and monitoring plan:",
            value = country_meta$prep_plan, min = 0, max = 12000),
          numericInput(ns("PREP_ENROLL"),
            "Enroll community participation:",
            value = country_meta$prep_enroll, min = 0, max = 12000),
          h5(strong("Production")),
          numericInput(ns("PRODUCTION_FACILITY"),
            "Facility setup:",
            value = country_meta$production_facility, min = 0, max = 20000),
          numericInput(ns("PRODUCTION_LINE"),
            "Mosquito line creation:",
            value = country_meta$production_line, min = 0, max = 20000),
          numericInput(ns("PRODUCTION_PROD"),
            "Mosquito production:",
            value = country_meta$production_prod, min = 0, max = 20000),
          numericInput(ns("PRODUCTION_QUALITY"),
            "Quality management and control:",
            value = country_meta$production_quality, min = 0, max = 20000),
          h5(strong("Distribution")),
          numericInput(ns("DISTRIBUTION_DELIVER"),
            "Deliver eggs/adults to distribution points:",
            value = country_meta$distribution_deliver, min = 0, max = 5000),
          h5(strong("Release")),
          numericInput(ns("RELEASE_DEPLOY"),
            "Egg or adult deployment:",
            value = country_meta$release_deploy, min = 0, max = 10000),
          numericInput(ns("RELEASE_QUALITY"),
            "Quality assurance:",
            value = country_meta$release_quality, min = 0, max = 10000),
          h5(strong("Monitoring")),
          numericInput(ns("MONITORING_MANAGE"),
            "Adaptive management:",
            value = country_meta$monitoring_manage, min = 0, max = 10000),
          numericInput(ns("MONITORING_SENTIMENT"),
            "Measure community sentiment:",
            value = country_meta$monitoring_sentiment, min = 0, max = 10000),
          numericInput(ns("MONITORING_FIELD"),
            "Monitoring Wolbachia frequency in the field:",
            value = country_meta$monitoring_field, min = 0, max = 10000)
        ),
        h3(strong("Secondary Inputs", align = "left")),
        sliderInput(ns("EFFECTIVENESS"), "Effectiveness:",
          min = 0, max = 100, value = 77),
        radioButtons(ns("POPDENSITY"),
          "Population density:",
          c(
            "\u2265 1000 people per km\u00B2" = "1000",
            "\u2265 750 people per km\u00B2" = "750",
            "\u2265 500 people per km\u00B2" = "500",
            "\u2265 250 people per km\u00B2" = "250",
            "\u2265 0 people per km\u00B2" = "0"
          )
        ),
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
        # conditionalPanel(
        #   condition = "input.tabset != 'BF'",
        #   numericInput(ns("PREV_RATE"), "Prevalence Rate (%):",
        #     min = 0, max = 100,
        #     value = country_meta$prevalence * 100),
        # ),
        numericInput(ns("COST_AMB"), "Cost per Ambulatory Visit:",
          value = country_meta$cost_per_amb_case, min = 0, max = 1000000),
        numericInput(ns("COST_HOSP"), "Cost per Hospitalized Visit:",
          value = country_meta$cost_per_hosp_case, min = 0, max = 1000000),
        numericInput(ns("COST_DEATH"), "Cost per Death:",
          value = country_meta$cost_per_child_fat, min = 0, max = 1000000),
        tags$div(
          class = "d-flex flex-row-reverse",
          style = "position: absolute; bottom: 0px; background: #bababa; width: 319px; margin-left: -11px; z-index: 9000; padding-left: 10px; padding-right: 10px; padding-top: 10px; padding-bottom: 5px; box-shadow: inset 0px 6px 5px -4px #ededed;",
          actionButton(ns("submit"), class = "disabled", "Submit"),
          tags$div(
            id = ns("submitText1"),
            style = "font-size: 15px; font-weight: 600; line-height: 18px; color: #d1615d;",
            "Update inputs to enable 'Submit' button."),
          tags$div(
            id = ns("submitText2"),
            style = "font-size: 15px; font-weight: 600; line-height: 18px; display: none; color: #d1615d;",
          "Click 'Submit' to see updated output.")
        )
      ),
      div(
        class = "col",
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
        tabsetPanel(id = "country_tabset",
          tabPanel("Map of Relevant Program Areas",
            style = tab_style,
            h2("Relevant Mosquito Release Program Areas"),
            tags$p("The following map shows areas where mosquito release
            programs for dengue control may be most useful based on population
            density. The colors indicate the cost per disease case averted
            by the intervention."),
            withSpinner(leafletOutput(ns("mymap"), height = "600px")),
            h2("Coverage Indicators"),
            tags$p("The following table shows each geography's coverage outputs:
            area covered by the program, people covered by the program, and
            cost per person covered."),
            withSpinner(uiOutput(ns("outputs")))
          ),
          tabPanel("Key Cost Indicators",
            style = tab_style,
            h2("Key Cost Indicators"),
            tags$p("The following table shows each geography's target area
            (in kilometers squared), total cost to cover the target area, cost
            per person covered by the intervention, cost per case averted by
            the intervention and cost per DALY averted by the intervention."),
            withSpinner(uiOutput(ns("keyindicators"))),
            # h2("Cost Per Case Averted vs. Cost Per DALY Averted"),
            # "The following plot shows cost per case vs. cost per DALY
            # by geography",
            # withSpinner(plotlyOutput(ns("keyindicatorsplot"))),
            # h2("Total Annual Cost"),
            # "The following plot shows the total annual cost by geography",
            # withSpinner(plotlyOutput(ns("keyindicatorsplot2")))
          ),
          tabPanel("Health Outcomes",
            style = tab_style,
            h2("Health Outcomes"),
            tags$p("The following table shows each geography's disease,
            prevalence total cases, cost per case averted, total DALYs,
            cost per DALY averted."),
            withSpinner(uiOutput(ns("healthoutcomesdata"))),
            # h2("Cost Per Case Averted"),
            # "The following plot shows each geography's cost per case averted.",
            # withSpinner(plotlyOutput(ns("healthoutcomesdataplot"))),
            # h2("Total Cases in Target Area"),
            # "The following plot shows the total cases in each target area.",
            # withSpinner(plotlyOutput(ns("healthoutcomesdataplot2")))
          ),
          tabPanel("Health System Costs",
            style = tab_style,
            h2("Health System Costs Averted"),
            tags$p("The following table shows each geography's total cases
            ambulatory and costs averted, hospitalized cases and costs
            averted, and total health system costs averted with a successful
            intervention mosquito release."),
            withSpinner(uiOutput(ns("healthsystemoutcomesdata"))),
            # h2("Ambulatory Costs Averted"),
            # "The following plot shows total ambulatory costs averted in each
            # geography.",
            # withSpinner(plotlyOutput(ns("healthsystemoutcomesdataplot"))),
            # h2("Hospital Costs Averted"),
            # "The following plot shows total hospital costs averted in each
            # geography.",
            # withSpinner(plotlyOutput(ns("healthsystemoutcomesdataplot2")))
          ),
          tabPanel("Economic Costs",
            style = tab_style,
            h2("Economic Costs Averted"),
            tags$p("The following table shows each geography's total deaths,
            cost per death averted, and economic losses due to disease
            fatalities."),
            withSpinner(uiOutput(ns("economicoutcomesdata"))),
            # withSpinner(plotlyOutput(ns("economicoutcomesdataplot")))
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

    # enable submit button when any input changes
    rvs <- reactiveValues(input_updated = FALSE)

    observeEvent(input$PLANNING, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$PREP, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$PRODUCTION, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$DISTRIBUTION, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$RELEASE, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$MONITORING, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$EFFECTIVENESS, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$AREACOV, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$PCT_AMB, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$PCT_HOSP, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$MORT_RATE, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$COST_AMB, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$COST_HOSP, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$COST_DEATH, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$POPDENSITY, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$PLANNING_PLAN, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$PLANNING_METHOD, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$PREP_PLAN, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$PREP_ENROLL, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$PRODUCTION_FACILITY, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$PRODUCTION_LINE, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$PRODUCTION_PROD, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$PRODUCTION_QUALITY, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$DISTRIBUTION_DELIVER, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$RELEASE_DEPLOY, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$RELEASE_QUALITY, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$MONITORING_MANAGE, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$MONITORING_SENTIMENT, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$MONITORING_FIELD, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)
    observeEvent(input$primaryinputtype, { rvs$input_updated <- TRUE },
      ignoreInit = TRUE)

    observeEvent(input$submit, { rvs$input_updated <- FALSE })

    observe({
      if (rvs$input_updated == FALSE) {
        shinyjs::disable("submit")
      } else {
        shinyjs::enable("submit")
      }
    })

    observe({
      if (rvs$input_updated == FALSE) {
        shinyjs::show("submitText1")
      } else {
        shinyjs::hide("submitText1")
      }
    })

    observe({
      if (rvs$input_updated == FALSE) {
        shinyjs::hide("submitText2")
      } else {
        shinyjs::show("submitText2")
      }
    })

    # selects current data based on the tab that is being viewed
    # dataset is a named list of datasets with name corresponding to tab value
    cur_dat <- reactive({
      # message("tab: ", tab())
      # message("id: ", id)
      if (tab() != id || is.null(input$POPDENSITY))
        return(NULL)
      res <- dataset[[tab()]][[input$POPDENSITY]]
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

      if (input$primaryinputtype == "1") {
        PLANNING <- as.numeric(input$PLANNING)
        PREP <- as.numeric(input$PREP)
        PROD <- as.numeric(input$PRODUCTION)
        DIST <- as.numeric(input$DISTRIBUTION)
        MONITOR <- as.numeric(input$MONITORING)
        RELEASE <- as.numeric(input$RELEASE)
      } else {
        PLANNING <- as.numeric(input$PLANNING_PLAN) + as.numeric(input$PLANNING_METHOD)
        PREP <- as.numeric(input$PREP_PLAN) + as.numeric(input$PREP_ENROLL)
        PROD <- as.numeric(input$PRODUCTION_FACILITY) + as.numeric(input$PRODUCTION_LINE) + as.numeric(input$PRODUCTION_PROD) + as.numeric(input$PRODUCTION_QUALITY)
        DIST <- as.numeric(input$DISTRIBUTION_DELIVER)
        RELEASE <- as.numeric(input$RELEASE_DEPLOY) + as.numeric(input$RELEASE_QUALITY)
        MONITOR <- as.numeric(input$MONITORING_MANAGE) + as.numeric(input$MONITORING_SENTIMENT) + as.numeric(input$MONITORING_FIELD)
      }

      update_data_from_inputs(
        cur_dat(),
        PLANNING = PLANNING,
        PREP = PREP,
        PROD = PROD,
        DIST = DIST,
        MONITOR = MONITOR,
        RELEASE = RELEASE,
        EFF = as.numeric(input$EFFECTIVENESS) / 100,
        AREACOV = as.numeric(input$AREACOV) / 100,
        PCT_AMB = as.numeric(input$PCT_AMB) / 100,
        PCT_HOSP = as.numeric(input$PCT_HOSP) / 100,
        MORT_RATE = as.numeric(input$MORT_RATE) / 100,
        # PREV_RATE = country_meta$prevalence,
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
          "km_target", "x_pdmean", "cost_per_pers_cov")))
    })

    keyindicators <- reactive({
      if (is.null(cur_dat_aug()))
        return(NULL)
      cur_dat_aug() %>%
        as.data.frame() %>%
        select(any_of(c(adm_names, "km_target",
          "tot_ann_cost_target", "cost_per_pers_cov",
          "cost_per_case_avert",  "cost_per_daly_avert")))
    })

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

    healthoutcomes <- reactive({
      if (is.null(cur_dat_aug()))
        return(NULL)
      cur_dat_aug() %>%
        as.data.frame() %>%
        select(any_of(c(adm_names, "prev_inc_m",
          "case_target_area", "cost_per_case_avert",
          "daly_target_area",  "cost_per_daly_avert")))
    })

    healthsystemoutcomes <- reactive({
      if (is.null(cur_dat_aug()))
        return(NULL)
      cur_dat_aug() %>%
        as.data.frame() %>%
        select(any_of(c(adm_names, "amb_cases", "hosp_cases",
          "amb_cost_avert", "hosp_cost_avert",
          "tot_health_sys_cost_avert")))
    })

    economicoutcomes <- reactive({
      if (is.null(cur_dat_aug()))
        return(NULL)
      cur_dat_aug() %>%
        as.data.frame() %>%
        select(any_of(c(adm_names, "death_target_area",
          "cost_per_death_avert", "econ_loss_death")))
    })

    # make_map() defined later in this file
    output$mymap <- renderLeaflet(make_map(cur_dat_aug(),
      country_meta, last_admin))

    # output$outputs <- shiny::renderDataTable(outputs(), options = dt_opts10)

    output$outputs <- shiny::renderUI({
      datatable(
        outputs(),
        id = "outputs",
        columns = list(
          km_target = colDef(name = "Target Area (KM2)",
            format = colFormat(separators = TRUE, digits = 1)),
          cost_per_pers_cov = colDef(name = "Cost per person",
            format = colFormat(separators = TRUE, currency = "USD")),
          x_pdmean = colDef(name = "Population in Target Area",
            format = colFormat(separators = TRUE, digits = 1))
        )
      )
    })

    output$keyindicators <- shiny::renderUI({
      datatable(
        keyindicators(),
        id = "keyindicators",
        columns = list(
          km_target = colDef(name = "Target Area (KM2)",
            format = colFormat(separators = TRUE, digits = 1)),
          tot_ann_cost_target = colDef(name = "Total",
            format = colFormat(separators = TRUE, currency = "USD")),
          cost_per_pers_cov = colDef(name = "Per person",
            format = colFormat(separators = TRUE, currency = "USD"),
            width = 120),
          cost_per_case_avert = colDef(name = "Per case",
            format = colFormat(separators = TRUE, currency = "USD"),
            width = 120),
          cost_per_daly_avert = colDef(name = "Per DALY",
            format = colFormat(separators = TRUE, currency = "USD"),
            width = 120)
        )
      )
    })

    # output$keyindicatorsplot <- renderPlotly({
    #   if (is.null(keyindicators()))
    #     return(NULL)
    #   ggplot(data = keyindicators() %>% filter(`Per case` < 1000),
    #     aes_string(x = "`Per DALY`", y = "`Per case`",
    #       color = first_admin, label = last_admin)) +
    #     geom_point(na.rm = TRUE) +
    #     # ggthemes::scale_color_tableau() +
    #     # xlim(0, 10) +
    #     # ylim(0, 10) +
    #     theme_classic() +
    #     labs(
    #       y = "Cost Per Case Averted (USD)",
    #       x = "Cost per DALY Averted (USD)") +
    #     ggtitle(paste("Key Cost Indicators Colored by", first_admin,
    #       "(with cost per case < 1000)"))
    # })

    # output$keyindicatorsplot2 <- renderPlotly({
    #   if (is.null(keyindicators()))
    #     return(NULL)
    #   keyindicators() %>%
    #     arrange(-Total) %>%
    #     mutate(rank = seq_len(n())) %>%
    #   ggplot(
    #     aes_string(x = "rank", y = "Total", color = first_admin,
    #       label = last_admin)) +
    #     geom_point(na.rm = TRUE) +
    #     # ggthemes::scale_color_tableau() +
    #     # ylim(0, 100000000) +
    #     theme_classic() +
    #     labs(
    #       y = "Total Annual Cost for Target Area (USD)",
    #       x = paste(first_admin, "Rank")) +
    #     ggtitle(paste("Total Annual Program Cost Colored by", first_admin)) +
    #     theme(axis.text.x = element_text(angle = 45))
    # })

    output$healthoutcomesdata <- shiny::renderUI({
      datatable(
        healthoutcomes(),
        id = "healthoutcomes",
        columns = list(
          prev_inc_m = colDef(name = "Incidence", width = 150),
          case_target_area = colDef(name = "Cases",
            format = colFormat(separators = TRUE, digits = 0),
            width = 120),
          cost_per_case_avert = colDef(name = "Cost per case",
            format = colFormat(separators = TRUE, currency = "USD"),
            width = 150),
          daly_target_area = colDef(name = "DALYs",
            format = colFormat(separators = TRUE, digits = 0),
            width = 130),
          cost_per_daly_avert = colDef(name = "Cost per DALY",
            format = colFormat(separators = TRUE, currency = "USD"),
            width = 150)
        )
      )
    })

    output$totalbudget <- renderText(totalbudget())
    output$casesaverted <- renderText(casesaverted())
    output$pctaverted <- renderText(pctaverted())

    # output$healthoutcomesdataplot <- renderPlotly({
    #   if (is.null(healthoutcomes()))
    #     return(NULL)
    #   healthoutcomes() %>%
    #     arrange(desc(.data$"Cost per case")) %>%
    #     mutate(rank = seq_len(n())) %>%
    #   ggplot(
    #     aes_string(x = "rank", y = "`Cost per case`",
    #       color = first_admin, label = last_admin)) +
    #     geom_point(na.rm = TRUE) +
    #     # ggthemes::scale_color_tableau() +
    #     # ylim(0, 10) +
    #     theme_classic() +
    #     labs(
    #       y = "Cost Per Case Averted (USD)",
    #       x = paste(last_admin, "Rank"),
    #       title = paste0("Cost Per Case Averted Colored by", first_admin)) +
    #     theme(axis.text.x = element_text(angle = 45))
    # })

    # output$healthoutcomesdataplot2 <- renderPlotly({
    #   if (is.null(healthoutcomes()))
    #     return(NULL)
    #   healthoutcomes() %>%
    #     arrange(desc(.data[["Cases"]])) %>%
    #     mutate(rank = seq_len(n())) %>%
    #   ggplot(
    #     aes_string(x = "rank", y = "Cases", color = first_admin,
    #       label = last_admin)) +
    #     geom_point(na.rm = TRUE) +
    #     # ggthemes::scale_color_tableau() +
    #     # ylim(0, 10000) +
    #     theme_classic() +
    #     labs(
    #       y = "Total Cases in Target Area",
    #       x = paste(last_admin, "Rank")) +
    #     ggtitle("Total Cases in Target Area") +
    #     theme(axis.text.x = element_text(angle = 45))
    # })

    output$healthsystemoutcomesdata <- shiny::renderUI({
      datatable(
        healthsystemoutcomes(),
        id = "healthsystemoutcomes",
        columns = list(
          amb_cases = colDef(name = "Ambulatory Cases",
            format = colFormat(separators = TRUE, digits = 0)),
          hosp_cases = colDef(name = "Hospitalized Cases",
            format = colFormat(separators = TRUE, digits = 0)),
          amb_cost_avert = colDef(name = "Ambulatory Costs",
            format = colFormat(separators = TRUE, currency = "USD")),
          hosp_cost_avert = colDef(name = "Hospital Costs",
            format = colFormat(separators = TRUE, currency = "USD")),
          tot_health_sys_cost_avert = colDef(name = "Total",
            format = colFormat(separators = TRUE, currency = "USD"))
        )
      )
    })

    # output$healthsystemoutcomesdataplot <- renderPlotly({
    #   if (is.null(healthsystemoutcomes()))
    #     return(NULL)
    #   healthsystemoutcomes() %>%
    #     arrange(desc(.data$"Ambulatory Costs")) %>%
    #     mutate(rank = seq_len(n())) %>%
    #   ggplot(
    #     aes_string(x = "rank", y = "`Ambulatory Costs`",
    #       color = first_admin, label = last_admin)) +
    #     geom_point(na.rm = TRUE) +
    #     # ggthemes::scale_color_tableau() +
    #     # ylim(0, 600000) +
    #     theme_classic() +
    #     labs(
    #       y = "Total Ambulatory Costs in Target Area",
    #       x = paste(last_admin, "Rank")) +
    #     ggtitle("Ambulatory Costs Averted in Target Area") +
    #     theme(axis.text.x = element_text(angle = 45))
    # })

    # output$healthsystemoutcomesdataplot2 <- renderPlotly({
    #   if (is.null(healthsystemoutcomes()))
    #     return(NULL)
    #   healthsystemoutcomes() %>%
    #     arrange(desc(.data$"Hospital Costs")) %>%
    #     mutate(rank = seq_len(n())) %>%
    #   ggplot(
    #     aes_string(x = "rank", y = "`Hospital Costs`",
    #       color = first_admin, label = last_admin)) +
    #     geom_point(na.rm = TRUE) +
    #     # ggthemes::scale_color_tableau() +
    #     # ylim(0, 600000) +
    #     theme_classic() +
    #     labs(
    #       y = "Total Hospitalized Costs in Target Area",
    #       x = paste(last_admin, "Rank")) +
    #     ggtitle("Hospitalized Costs Averted in Target Area") +
    #     theme(axis.text.x = element_text(angle = 45))
    # })

    output$economicoutcomesdata <- shiny::renderUI({
      datatable(
        economicoutcomes(),
        id = "economicoutcomes",
        columns = list(
          cost_per_death_avert = colDef(name = "Cost per death averted",
            format = colFormat(separators = TRUE, currency = "USD")),
          death_target_area = colDef(name = "Deaths",
            format = colFormat(separators = TRUE, digits = 0)),
          econ_loss_death = colDef(name = "Economic losses",
            format = colFormat(separators = TRUE, currency = "USD"))
        )
      )
    })

    # output$economicoutcomesdataplot <- renderPlotly({
    #   if (is.null(economicoutcomes()))
    #     return(NULL)
    #   economicoutcomes() %>%
    #     arrange(desc(.data$"Economic losses")) %>%
    #     mutate(rank = seq_len(n())) %>%
    #   ggplot(
    #     aes_string(x = "rank", y = "`Economic losses`",
    #       color = first_admin, label = last_admin)) +
    #     geom_point(na.rm = TRUE) +
    #     # ggthemes::scale_color_tableau() +
    #     # ylim(0, 1000000) +
    #     theme_classic() +
    #     labs(
    #       y = "Total Economic Losses",
    #       x = paste(last_admin, "Rank")) +
    #     ggtitle(paste0("Economic Losses Averted in Target Areas of ",
    #       last_admin, "s")) +
    #     theme(axis.text.x = element_text(angle = 45))
    # })

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
