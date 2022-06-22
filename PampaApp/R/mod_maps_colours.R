#' maps_colours UI Function
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
mod_maps_colours_ui <- function(id){
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shiny::h3("Maps with coloured polygons", align = "center"),
      shiny::br(),
      shinyWidgets::radioGroupButtons(ns("maps_colours_aggregation"), "Choose an aggregation",
        choices = c(
          "species" = "espece",
          "species groups" = "unitobs"
        ),
        justified = TRUE
      ),
      shiny::selectInput(ns("maps_colours_factSpatial"), "Select a spatial grouping factor",
        choices = c()
      ),
      shiny::selectInput(ns("maps_colours_factSpatialSel"), "Select categories for the spatial grouping factor (all by default)",
        choices = c(), multiple = TRUE
      ),
      shiny::radioButtons(ns("maps_colours_metric_table"), "Choose a metric table",
        choices = c(
          "... / station / size class" = "unitSpSz",
          "... / station" = "unitSp")
      ),
      shiny::selectInput(ns("maps_colours_metric"), "Select a metric",
        choices = c()
      ),
      shiny::radioButtons(ns("maps_colours_type_fact"), "Subset species per...",
        choices = c(
          "... station characteristic" = "unitobs",
          "... species characteristic" = "refesp")
      ),
      shiny::selectInput(ns("maps_colours_factGraph"), "select the factor for the graphic separation",
        choices = c()
      ),
      shiny::selectInput(ns("maps_colours_factGraphSel"), "Select categories of the factor for the  graphic separation (all by default)",
        choices = c(), multiple = TRUE
      ),
      shiny::div(
        shiny::actionButton(ns("maps_colours_launch_button"), "Launch Maps"),
        align = "center"
      )
    ),
    shiny::mainPanel(width = 9,
      shiny::h3("Maps", align = "center"),
      leaflet::leafletOutput(ns("maps_colours"), width = "100%", height = "750px")
    )
  )
}

#' maps_colours Server Functions
#'
#' @noRd
#'
#' @import shiny
#' @importFrom PAMPA maps.f
#' @importFrom leaflet renderLeaflet
mod_maps_colours_server <- function(id, load_file){
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
      switch(input$maps_colours_aggregation,
        "espece" = "spSymbols.esp",
        "unitobs" = "spSymbols.unitobs")
    )

    shiny::observeEvent({
      input$maps_colours_aggregation
    }, {
      if (input$maps_colours_aggregation == "espece"){
        if (load_file() != 0 && (PAMPA:::is.benthos.f() | nrow(get("unitSpSz", envir = .GlobalEnv)) == 0)){
          shiny::updateRadioButtons(inputId = "maps_colours_metric_table",
            choices = c(
              "... / station / species" = "unitSp")
          )
        } else{
          shiny::updateRadioButtons(inputId = "maps_colours_metric_table",
            choices = c(
              "... / station / species / size classe" = "unitSpSz",
              "... / station / species" = "unitSp"),
            selected = "unitSp"
          )
        }
        shiny::updateRadioButtons(inputId = "maps_colours_type_fact",
          label = "Generate one plot per...",
          choices = c(
            "... station characteristic" = "unitobs",
            "... species characteristic" = "refesp"),
          selected = "refesp"
        )
      } else{
        if (load_file() != 0 && (PAMPA:::is.benthos.f() | nrow(get("unitSpSz", envir = .GlobalEnv)) == 0)){
          shiny::updateRadioButtons(inputId = "maps_colours_metric_table",
            choices = c(
              "... / station" = "unitSp",
              "... of biodiversity (/ station)" = "unit"),
            selected = "unitSp"
          )
        } else{
          shiny::updateRadioButtons(inputId = "maps_colours_metric_table",
            choices = c(
              "... / station / size classe" = "unitSpSz",
              "... / station" = "unitSp",
              "... of biodiversity (/ station)" = "unit"),
            selected = "unitSp"
          )
        }
        shiny::updateRadioButtons(inputId = "maps_colours_type_fact",
          label = "Subset species per...",
          choices = c(
            "... species characteristic" = "refesp")
        )
      }
    })

    shiny::observeEvent(load_file(), {
      if (load_file() != 0){
        choices <- PAMPA:::champsRefspa.f(dataEnv = .GlobalEnv)
        shiny::updateSelectInput(inputId = "maps_colours_factSpatial", choices = choices)
      }
    })

    shiny::observeEvent(
    {
      input$maps_colours_factSpatial
    }, {
      if (input$maps_colours_factSpatial != "" & input$maps_colours_factSpatial != "NA"){
        choices <- unique(PAMPA:::selectModalitesSpatiales.f(tableMetrique = input$maps_colours_metric_table,
          facts = input$maps_colours_factSpatial, selections = append(list(NA), NA),
          metrique = input$maps_colours_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv)[, input$maps_colours_factSpatial])
        choices <- sort(as.character(choices))
        shiny::updateSelectInput(inputId = "maps_colours_factSpatialSel", choices = choices)
      }
    })

    shiny::observeEvent(
    {
      input$maps_colours_metric_table
      load_file()
    }, {
      if (load_file() != 0){
        choices <- PAMPA:::MetricsField.aliases(input$maps_colours_metric_table, "maps_colours", .GlobalEnv)
        shiny::updateSelectInput(inputId = "maps_colours_metric", choices = c("", choices))
      }
    })

    shiny::observeEvent(
    {
      input$maps_colours_type_fact
      load_file()
    }, {
      if (load_file() != 0){
        choices <- switch(input$maps_colours_type_fact,
          "unitobs" = PAMPA:::UnitobsFields.aliases(dataEnv = .GlobalEnv, ordered = TRUE,
            tableMetrique = input$maps_colours_metric_table),
          "refesp" = PAMPA:::spRefFields.aliases(site = getOption("P.MPA"), dataEnv = .GlobalEnv,
            ordered = TRUE, tableMetrique = input$maps_colours_metric_table)
        )
        shiny::updateSelectInput(inputId = "maps_colours_factGraph", choices = c("", NA, choices))
      }
    })

    shiny::observeEvent(input$maps_colours_factGraph, {
      if(input$maps_colours_factGraph != "" & input$maps_colours_factGraph != "NA"){
        choices <- unique(PAMPA:::selectModalites.f(tableMetrique = input$maps_colours_metric_table,
          facts = input$maps_colours_factGraph, selections = append(list(NA), NA),
          metrique = input$maps_colours_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$maps_colours_factGraph])
        choices <- sort(as.character(choices))
        shiny::updateSelectInput(inputId = "maps_colours_factGraphSel", choices = c("", choices))
      } else{
        shiny::updateSelectInput(inputId = "maps_colours_factGraphSel", choices = c())
      }
    })

    shiny::observeEvent(input$maps_colours_launch_button, {
      shiny::showModal(shiny::modalDialog("Creation of map...", footer = NULL))

      params$aggregation <- input$maps_colours_aggregation
      params$fact_spatial <- input$maps_colours_factSpatial
      params$fact_spatial_sel <- if (!is.null(input$maps_colours_factSpatialSel)){
        input$maps_colours_factSpatialSel
      } else{
        NA
      }
      params$metric_table <- input$maps_colours_metric_table
      params$metric <- input$maps_colours_metric
      params$type_fact <- input$maps_colours_type_fact
      params$fact_graph <- if (!is.null(input$maps_colours_factGraph) && input$maps_colours_factGraph != "NA"){
        input$maps_colours_factGraph
      } else{
        ""
      }
      params$fact_graph_sel <- if (!is.null(input$maps_colours_factGraphSel)){
        input$maps_colours_factGraphSel
      } else{
        NA
      }

      map <- PAMPA::maps.f(
        agregation = params$aggregation,
        graphType = "couleurs",
        metrique = params$metric,
        factSpatial = params$fact_spatial,
        factSpatialSel = params$fact_spatial_sel,
        factGraph = params$fact_graph,
        factGraphSel = params$fact_graph_sel,
        tableMetrique = params$metric_table,
        dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
      )

      output$maps_colours <- leaflet::renderLeaflet({
        map@map
      })

      shiny::removeModal()
    })
  })
}

## To be copied in the UI
# mod_maps_colours_ui("maps_colours_1")

## To be copied in the server
# mod_maps_colours_server("maps_colours_1")
