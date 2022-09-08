# this file contains a shiny module for each country tab
# reference: https://mastering-shiny.org/scaling-modules.html

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

tab_style <- "height: calc(100vh - 165px); overflow: auto;"

range_txt <- function(rng) {
  paste0(
    "($",
    format(rng[1], big.mark = ","),
    "-$",
    format(rng[2], big.mark = ","),
    ")"
  )
}

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
          choices = c("Phase-based" = 1, "Activity-based" = 2),
          selected = "1"
        ),
        conditionalPanel(
          condition = paste0("input['", ns("primaryinputtype"), "'] === '1'"),
          numericInput(ns("PLANNING"), span("Planning Cost:",
            em(style = "font-weight: normal;",
              range_txt(CONSTANTS$planning_range))),
            value = CONSTANTS$planning,
            min = CONSTANTS$planning_range[1],
            max = CONSTANTS$planning_range[2]
          ),
          numericInput(ns("PREP"),
            span("Preparation Cost:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$prep_range))),
            value = CONSTANTS$prep,
            min = CONSTANTS$prep_range[1],
            max = CONSTANTS$prep_range[2]
          ),
          numericInput(ns("PRODUCTION"),
            span("Production Cost:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$production_range))),
            value = CONSTANTS$production,
            min = CONSTANTS$production_range[1],
            max = CONSTANTS$production_range[2]
          ),
          numericInput(ns("DISTRIBUTION"),
            span("Distribution Cost:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$distribution_range))),
            value = CONSTANTS$distribution,
            min = CONSTANTS$distribution_range[1],
            max = CONSTANTS$distribution_range[2]
          ),
          numericInput(ns("RELEASE"),
            span("Release Cost:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$release_range))),
            value = CONSTANTS$release,
            min = CONSTANTS$release_range[1],
            max = CONSTANTS$release_range[2]
          ),
          numericInput(ns("MONITORING"),
            span("Monitoring Cost:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$monitoring_range))),
            value = CONSTANTS$monitoring,
            min = CONSTANTS$monitoring_range[1],
            max = CONSTANTS$monitoring_range[2]
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("primaryinputtype"), "'] === '2'"),
          h5(strong("Planning")),
          numericInput(ns("DEFINEWORKPLAN"),
            span("Define workplan and budget:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$defineworkplan_range))),
            value = CONSTANTS$defineworkplan,
            min = CONSTANTS$defineworkplan_range[1],
            max = CONSTANTS$defineworkplan_range[2]
          ),
          numericInput(ns("DETERMINERELEASE"),
            span("Determine release methodology:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$determinerelease_range))),
            value = CONSTANTS$determinerelease,
            min = CONSTANTS$determinerelease_range[1],
            max = CONSTANTS$determinerelease_range[2]
          ),
          h5(strong("Preparation")),
          # numericInput(ns("PREP_PLAN"),
          #   "Complete release and monitoring plan:",
          #   value = 1000,
          #   min = 0,
          #   max = 12000
          # ), # TODO: make sure this is really not needed anymore
          numericInput(ns("ENROLCOMMUNITY"),
            span("Enroll community participation:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$enrolcommunity_range))),
            value = CONSTANTS$enrolcommunity,
            min = CONSTANTS$enrolcommunity_range[1],
            max = CONSTANTS$enrolcommunity_range[2]
          ),
          h5(strong("Production")),
          numericInput(ns("PRODUCTION_FACILITY"),
            span("Facility setup:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$facilitysetup_range))),
            value = CONSTANTS$facilitysetup,
            min = CONSTANTS$facilitysetup_range[1],
            max = CONSTANTS$facilitysetup_range[2]
          ),
          numericInput(ns("MOSLINECREATION"),
            span("Mosquito line creation:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$moslinecreation_range))),
            value = CONSTANTS$moslinecreation,
            min = CONSTANTS$moslinecreation_range[1],
            max = CONSTANTS$moslinecreation_range[2]
          ),
          numericInput(ns("MOSPROD"),
            span("Mosquito production:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$mosprod_range))),
            value = CONSTANTS$mosprod,
            min = CONSTANTS$mosprod_range[1],
            max = CONSTANTS$mosprod_range[2]
          ),
          numericInput(ns("QUALITYMANAGEMENT"),
            span("Quality management and control:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$qualitymanagement_range))),
            value = CONSTANTS$qualitymanagement,
            min = CONSTANTS$qualitymanagement_range[1],
            max = CONSTANTS$qualitymanagement_range[2]
          ),
          h5(strong("Distribution")),
          numericInput(ns("DELIVEREGGS"),
            span("Deliver eggs/adults to distribution points:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$delivereggs_range))),
            value = CONSTANTS$delivereggs,
            min = CONSTANTS$delivereggs_range[1],
            max = CONSTANTS$delivereggs_range[2]
          ),
          h5(strong("Release")),
          numericInput(ns("EGGDEPLOYMENT"),
            span("Egg or adult deployment:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$eggdeployment_range))),
            value = CONSTANTS$eggdeployment,
            min = CONSTANTS$eggdeployment_range[1],
            max = CONSTANTS$eggdeployment_range[2]
          ),
          numericInput(ns("QUALITYASSURANCE"),
            span("Quality assurance:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$qualityassurance_range))),
            value = CONSTANTS$qualityassurance,
            min = CONSTANTS$qualityassurance_range[1],
            max = CONSTANTS$qualityassurance_range[2]
          ),
          h5(strong("Monitoring")),
          numericInput(ns("ADAPTIVEMANAGEMENT"),
            span("Adaptive management:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$adaptivemanagement_range))),
            value = CONSTANTS$adaptivemanagement,
            min = CONSTANTS$adaptivemanagement_range[1],
            max = CONSTANTS$adaptivemanagement_range[2]
          ),
          numericInput(ns("MEASURECOMMUNITY"),
            span("Measure community sentiment:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$measurecommunity_range))),
            value = CONSTANTS$measurecommunity,
            min = CONSTANTS$measurecommunity_range[1],
            max = CONSTANTS$measurecommunity_range[2]
          ),
          numericInput(ns("MONITORINGWOLBACHIA"),
            span("Monitoring Wolbachia frequency in the field:",
              em(style = "font-weight: normal;",
                range_txt(CONSTANTS$monitoringwolbachia_range))),
            value = CONSTANTS$monitoringwolbachia,
            min = CONSTANTS$monitoringwolbachia_range[1],
            max = CONSTANTS$monitoringwolbachia_range[2]
          )
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
          value = country_meta$cost_per_amb, min = 0, max = 1000000),
        numericInput(ns("COST_HOSP"), "Cost per Hospitalized Visit:",
          value = country_meta$cost_per_hosp, min = 0, max = 1000000),
        # numericInput(ns("COST_DEATH"), "Cost per Death:",
        #   value = country_meta$cost_per_child_fat, min = 0, max = 1000000),
        tags$div(
          class = "d-flex flex-row-reverse",
          style = "position: absolute; bottom: 0px; background: #bababa; width: 319px; margin-left: -11px; z-index: 9000; padding-left: 8px; padding-right: 8px; padding-top: 8px; padding-bottom: 8px;",
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
        style = "overflow: hidden; max-width: calc(100vw - 590px);",
        tabsetPanel(id = "country_tabset",
          selected = "5-Year",
          tabPanel("1-Year",
            style = tab_style,
            year_tab_ui(ns("1"), id, country_meta)
          ),
          tabPanel("5-Year",
            style = tab_style,
            year_tab_ui(ns("5"), id, country_meta)
          ),
          tabPanel("10-Year",
            style = tab_style,
            year_tab_ui(ns("10"), id, country_meta)
          ),
          tabPanel("20-Year",
            style = tab_style,
            year_tab_ui(ns("20"), id, country_meta)
          )
        )
      )
    )
  )
}

#' @param id country ID (used to access the appropriate data)
country_tab_server <- function(id, tab, country_meta) {
  moduleServer(id, function(input, output, session) {
    # enable submit button when any input changes
    rvs <- reactiveValues(input_updated = FALSE)

    observeEvent(
      {
        input$PLANNING
        input$PREP
        input$PRODUCTION
        input$DISTRIBUTION
        input$RELEASE
        input$MONITORING
        input$EFFECTIVENESS
        input$AREACOV
        input$PCT_AMB
        input$PCT_HOSP
        input$MORT_RATE
        input$COST_AMB
        input$COST_HOSP
        input$COST_DEATH
        input$POPDENSITY
        input$DEFINEWORKPLAN
        input$DETERMINERELEASE
        input$ENROLCOMMUNITY
        input$PRODUCTION_FACILITY
        input$MOSLINECREATION
        input$MOSPROD
        input$QUALITYMANAGEMENT
        input$DELIVEREGGS
        input$EGGDEPLOYMENT
        input$QUALITYASSURANCE
        input$ADAPTIVEMANAGEMENT
        input$MEASURECOMMUNITY
        input$MONITORINGWOLBACHIA
        input$primaryinputtype
      },
      {
        rvs$input_updated <- TRUE
      },
      ignoreInit = TRUE
    )

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

    inpts <- reactive(list(
      PLANNING = input$PLANNING,
      PREP = input$PREP,
      PRODUCTION = input$PRODUCTION,
      DISTRIBUTION = input$DISTRIBUTION,
      RELEASE = input$RELEASE,
      MONITORING = input$MONITORING,
      EFFECTIVENESS = input$EFFECTIVENESS,
      AREACOV = input$AREACOV,
      PCT_AMB = input$PCT_AMB,
      PCT_HOSP = input$PCT_HOSP,
      MORT_RATE = input$MORT_RATE,
      COST_AMB = input$COST_AMB,
      COST_HOSP = input$COST_HOSP,
      COST_DEATH = input$COST_DEATH,
      POPDENSITY = input$POPDENSITY,
      DEFINEWORKPLAN = input$DEFINEWORKPLAN,
      DETERMINERELEASE = input$DETERMINERELEASE,
      ENROLCOMMUNITY = input$ENROLCOMMUNITY,
      PREP_PLAN = input$PREP_PLAN,
      PRODUCTION_FACILITY = input$PRODUCTION_FACILITY,
      MOSLINECREATION = input$MOSLINECREATION,
      MOSPROD = input$MOSPROD,
      QUALITYMANAGEMENT = input$QUALITYMANAGEMENT,
      DELIVEREGGS = input$DELIVEREGGS,
      EGGDEPLOYMENT = input$EGGDEPLOYMENT,
      QUALITYASSURANCE = input$QUALITYASSURANCE,
      ADAPTIVEMANAGEMENT = input$ADAPTIVEMANAGEMENT,
      MEASURECOMMUNITY = input$MEASURECOMMUNITY,
      MONITORINGWOLBACHIA = input$MONITORINGWOLBACHIA
    ))

    primaryinputtype <- reactive(input$primaryinputtype)
    submit <- reactive(input$submit)

    year_tab_server("1", id, tab, inpts, country_meta,
      primaryinputtype, submit)
    year_tab_server("5", id, tab, inpts, country_meta,
      primaryinputtype, submit)
    year_tab_server("10", id, tab, inpts, country_meta,
      primaryinputtype, submit)
    year_tab_server("20", id, tab, inpts, country_meta,
      primaryinputtype, submit)
  })
}
