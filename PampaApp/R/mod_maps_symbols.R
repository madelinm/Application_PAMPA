#' maps_symbols UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom leaflet leafletOutput
mod_maps_symbols_ui <- function(id){
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shiny::h3("Maps with symbols", align = "center"),
      shiny::br(),
      shinyWidgets::radioGroupButtons(ns("maps_symbols_aggregation"), "Choose an aggregation",
        choices = c(
          "species" = "espece",
          "species groups" = "unitobs"
        ),
        justified = TRUE
      ),
      shiny::selectInput(ns("maps_symbols_factSpatial"), "Select a spatial grouping factor",
        choices = c()
      ),
      shiny::selectInput(ns("maps_symbols_factSpatialSel"), "Select categories for the spatial grouping factor (all by default)",
        choices = c(), multiple = TRUE
      ),
      shiny::radioButtons(ns("maps_symbols_metric_table"), "Choose a metric table",
        choices = c(
          "... / station / size class" = "unitSpSz",
          "... / station" = "unitSp")
      ),
      shiny::selectInput(ns("maps_symbols_metric"), "Select a metric",
        choices = c()
      ),
      shiny::radioButtons(ns("maps_symbols_type_fact"), "Subset species per...",
        choices = c(
          "... station characteristic" = "unitobs",
          "... species characteristic" = "refesp")
      ),
      shiny::selectInput(ns("maps_symbols_factGraph"), "select the factor for the graphic separation",
        choices = c()
      ),
      shiny::selectInput(ns("maps_symbols_factGraphSel"), "Select categories of the factor for the  graphic separation (all by default)",
        choices = c(), multiple = TRUE
      ),
      shiny::div(
        shiny::actionButton(ns("maps_symbols_launch_button"), "Launch Maps"),
        align = "center"
      )
    ),
    shiny::mainPanel(width = 9,
      shiny::h3("Maps", align = "center"),
      leaflet::leafletOutput(ns("maps_symbols"), width = "100%", height = "750px")
    )
  )
}

#' maps_symbols Server Functions
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinyjs reset
#' @importFrom PAMPA maps.f
#' @importFrom leaflet renderLeaflet
mod_maps_symbols_server <- function(id, load_file){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    params <- shiny::reactiveValues(
      aggregation = NULL,
      fact_spatial = NULL,
      fact_spatial_sel = NULL,
      metric_table = NULL,
      metric = NULL,
      type_fact = NULL,
      fact_graph = NULL,
      fact_graph_sel = NULL
    )

    next_step <- shiny::reactive(
      switch(input$maps_symbols_aggregation,
        "espece" = "spSymbols.esp",
        "unitobs" = "spSymbols.unitobs")
    )

    shiny::observeEvent(load_file(), {
      shinyjs::reset(id = "maps_symbols_factSpatialSel")
      shinyjs::reset(id = "maps_symbols_factGraphSel")
      output$maps_symbols <- NULL
    })

    shiny::observeEvent({
        input$maps_symbols_aggregation
      }, {
        if (input$maps_symbols_aggregation == "espece"){
          if (load_file() != 0 && (PAMPA:::is.benthos.f() | nrow(get("unitSpSz", envir = .GlobalEnv)) == 0)){
            shiny::updateRadioButtons(inputId = "maps_symbols_metric_table",
              choices = c(
                "... / station / species" = "unitSp")
            )
          } else{
            shiny::updateRadioButtons(inputId = "maps_symbols_metric_table",
              choices = c(
                "... / station / species / size classe" = "unitSpSz",
                "... / station / species" = "unitSp"),
              selected = "unitSp"
            )
          }
          shiny::updateRadioButtons(inputId = "maps_symbols_type_fact",
            label = "Generate one plot per...",
            choices = c(
              "... station characteristic" = "unitobs",
              "... species characteristic" = "refesp"),
            selected = "refesp"
          )
        } else{
          if (load_file() != 0 && (PAMPA:::is.benthos.f() | nrow(get("unitSpSz", envir = .GlobalEnv)) == 0)){
            shiny::updateRadioButtons(inputId = "maps_symbols_metric_table",
              choices = c(
                "... / station" = "unitSp",
                "... of biodiversity (/ station)" = "unit"),
              selected = "unitSp"
            )
          } else{
            shiny::updateRadioButtons(inputId = "maps_symbols_metric_table",
              choices = c(
                "... / station / size classe" = "unitSpSz",
                "... / station" = "unitSp",
                "... of biodiversity (/ station)" = "unit"),
              selected = "unitSp"
            )
          }
          shiny::updateRadioButtons(inputId = "maps_symbols_type_fact",
            label = "Subset species per...",
            choices = c(
              "... species characteristic" = "refesp")
          )
        }
    })

    shiny::observeEvent(load_file(), {
      if (load_file() != 0){
        choices <- PAMPA:::champsRefspa.f(dataEnv = .GlobalEnv)
        shiny::updateSelectInput(inputId = "maps_symbols_factSpatial", choices = choices)
      }

    })

    shiny::observeEvent(
      {
        input$maps_symbols_factSpatial
      }, {
        if(input$maps_symbols_factSpatial != "" & input$maps_symbols_factSpatial != "NA"){
          choices <- unique(PAMPA:::selectModalitesSpatiales.f(tableMetrique = input$maps_symbols_metric_table,
            facts = input$maps_symbols_factSpatial, selections = append(list(NA), NA),
            metrique = input$maps_symbols_metric, nextStep = next_step(),
            dataEnv = .GlobalEnv)[, input$maps_symbols_factSpatial])
          choices <- sort(as.character(choices))
          shiny::updateSelectInput(inputId = "maps_symbols_factSpatialSel", choices = choices)
        }
      }
    )

    shiny::observeEvent(
      {
        input$maps_symbols_metric_table
        load_file()
      }, {
        if (load_file() != 0){
          choices <- PAMPA:::MetricsField.aliases(input$maps_symbols_metric_table, "maps_symbols", .GlobalEnv)
          shiny::updateSelectInput(inputId = "maps_symbols_metric", choices = c("", choices))
        }
    })

    shiny::observeEvent(
      {
        input$maps_symbols_type_fact
        load_file()
      }, {
        if (load_file() != 0){
          choices <- switch(input$maps_symbols_type_fact,
            "unitobs" = PAMPA:::UnitobsFields.aliases(dataEnv = .GlobalEnv, ordered = TRUE,
              tableMetrique = input$maps_symbols_metric_table),
            "refesp" = PAMPA:::spRefFields.aliases(site = getOption("P.MPA"), dataEnv = .GlobalEnv,
              ordered = TRUE, tableMetrique = input$maps_symbols_metric_table)
          )
          shiny::updateSelectInput(inputId = "maps_symbols_factGraph", choices = c("", NA, choices))
        }
    })

    shiny::observeEvent(input$maps_symbols_factGraph, {
      if(input$maps_symbols_factGraph != "" & input$maps_symbols_factGraph != "NA"){
        choices <- unique(PAMPA:::selectModalites.f(tableMetrique = input$maps_symbols_metric_table,
          facts = input$maps_symbols_factGraph, selections = append(list(NA), NA),
          metrique = input$maps_symbols_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$maps_symbols_factGraph])
        choices <- sort(as.character(choices))
        shiny::updateSelectInput(inputId = "maps_symbols_factGraphSel", choices = c("", choices))
      } else{
        shiny::updateSelectInput(inputId = "maps_symbols_factGraphSel", choices = c())
      }
    })

    shiny::observeEvent(input$maps_symbols_launch_button, {
      shiny::showModal(shiny::modalDialog("Creation of graphics...", footer = NULL))

      params$aggregation <- input$maps_symbols_aggregation
      params$fact_spatial <- input$maps_symbols_factSpatial
      params$fact_spatial_sel <- if (!is.null(input$maps_symbols_factSpatialSel)){
        input$maps_symbols_factSpatialSel
      } else{
        NA
      }
      params$metric_table <- input$maps_symbols_metric_table
      params$metric <- input$maps_symbols_metric
      params$type_fact <- input$maps_symbols_type_fact
      params$fact_graph <- if (!is.null(input$maps_symbols_factGraph) && input$maps_symbols_factGraph != "NA"){
        input$maps_symbols_factGraph
      } else{
        ""
      }
      params$fact_graph_sel <- if (!is.null(input$maps_symbols_factGraphSel)){
        input$maps_symbols_factGraphSel
      } else{
        NA
      }

      map <- PAMPA::maps.f(
        agregation = params$aggregation,
        graphType = "symboles",
        metrique = params$metric,
        factSpatial = params$fact_spatial,
        factSpatialSel = params$fact_spatial_sel,
        factGraph = params$fact_graph,
        factGraphSel = params$fact_graph_sel,
        tableMetrique = params$metric_table,
        dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
      )

      output$maps_symbols <- leaflet::renderLeaflet({
        map@map
      })

      shiny::removeModal()
    })

  })
}

## To be copied in the UI
# mod_maps_symbols_ui("maps_symbols_1")

## To be copied in the server
# mod_maps_symbols_server("maps_symbols_1")
