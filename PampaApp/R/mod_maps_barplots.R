#' maps_barplots UI Function
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
mod_maps_barplots_ui <- function(id){
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shiny::h3("Maps with barplots", align = "center"),
      shiny::br(),
      shinyWidgets::radioGroupButtons(ns("maps_barplots_aggregation"), "Choose an aggregation",
        choices = c(
          "species" = "espece",
          "species groups" = "unitobs"
        ),
        justified = TRUE
      ),
      shiny::selectInput(ns("maps_barplots_factSpatial"), "Select a spatial grouping factor",
        choices = c()
      ),
      shiny::selectInput(ns("maps_barplots_factSpatialSel"), "Select categories for the spatial grouping factor (all by default)",
        choices = c(), multiple = TRUE
      ),
      shiny::radioButtons(ns("maps_barplots_metric_table"), "Choose a metric table",
        choices = c(
          "... / station / size class" = "unitSpSz",
          "... / station" = "unitSp")
      ),
      shiny::selectInput(ns("maps_barplots_metric"), "Select a metric",
        choices = c()
      ),
      shiny::radioButtons(ns("maps_barplots_type_fact"), "Subset species per...",
        choices = c(
          "... station characteristic" = "unitobs",
          "... species characteristic" = "refesp")
      ),
      shiny::selectInput(ns("maps_barplots_factGraph"), "select the factor for the graphic separation",
        choices = c()
      ),
      shiny::selectInput(ns("maps_barplots_factGraphSel"), "Select categories of the factor for the  graphic separation (all by default)",
        choices = c(), multiple = TRUE
      ),
      shiny::selectInput(ns("maps_barplots_listFact"), "Select explanatory factor(s) for plotting",
        choices = c(), multiple = TRUE
      ),
      shiny::uiOutput(ns("maps_barplots_listFactSel")),
      shiny::div(
        shiny::actionButton(ns("maps_barplots_launch_button"), "Launch Maps"),
        align = "center"
      )
    ),
    shiny::mainPanel(width = 9,
      shiny::h3("Maps", align = "center"),
      leaflet::leafletOutput(ns("maps_barplots"), width = "100%", height = "750px")
    )
  )
}

#' maps_barplots Server Functions
#'
#' @noRd
#'
#' @import shiny
#' @importFrom PAMPA maps.f
#' @importFrom leaflet renderLeaflet
mod_maps_barplots_server <- function(id, load_file){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    length_listFact <- shiny::reactive({
      length(input$maps_barplots_listFact)
    })

    params <- shiny::reactiveValues(
      aggregation = NULL,
      fact_spatial = NULL,
      fact_spatial_sel = NULL,
      metric_table = NULL,
      metric = NULL,
      type_fact = NULL,
      fact_graph = NULL,
      fact_graph_sel = NULL,
      list_fact = NULL,
      list_fact_sel = NULL
    )

    next_step <- shiny::reactive(
      switch(input$maps_barplots_aggregation,
        "espece" = "spBarBoxplot.esp",
        "unitobs" = "spBarBoxplot.unitobs")
    )

    shiny::observeEvent(input$maps_barplots_aggregation, {
      if (input$maps_barplots_aggregation == "espece"){
        if (load_file() != 0 && (!PAMPA:::is.benthos.f() | nrow(get("unitSpSz", envir = .GlobalEnv)) == 0)){
          shiny::updateRadioButtons(inputId = "maps_barplots_metric_table",
            choices = c(
              "... / station / species" = "unitSp")
          )
        } else{
          shiny::updateRadioButtons(inputId = "maps_barplots_metric_table",
            choices = c(
              "... / station / species / size classe" = "unitSpSz",
              "... / station / species" = "unitSp"),
            selected = "unitSp"
          )
        }
        shiny::updateRadioButtons(inputId = "maps_barplots_type_fact",
          label = "Generate one plot per...",
          choices = c(
            "... station characteristic" = "unitobs",
            "... species characteristic" = "refesp"),
          selected = "refesp"
        )
      } else{
        if (load_file() != 0 && (!PAMPA:::is.benthos.f() | nrow(get("unitSpSz", envir = .GlobalEnv)) == 0)){
          shiny::updateRadioButtons(inputId = "maps_barplots_metric_table",
            choices = c(
              "... / station" = "unitSp",
              "... of biodiversity (/ station)" = "unit"),
            selected = "unitSp"
          )
        } else{
          shiny::updateRadioButtons(inputId = "maps_barplots_metric_table",
            choices = c(
              "... / station / size classe" = "unitSpSz",
              "... / station" = "unitSp",
              "... of biodiversity (/ station)" = "unit"),
            selected = "unitSp"
          )
        }
        shiny::updateRadioButtons(inputId = "maps_barplots_type_fact",
          label = "Subset species per...",
          choices = c(
            "... species characteristic" = "refesp")
        )
      }
    })

    output$maps_barplots_listFactSel <- shiny::renderUI({
      nb_input <- length_listFact()
      ids <- sapply(1:nb_input, function(i){
        paste("maps_barplots_listFactSel_", i, sep = "")
      })
      labels_input <- sapply(1:nb_input, function(i){
        paste("Choose categories for ", input$maps_barplots_listFact[i], " (all by default)", sep = "")
      })
      output_listFactSel <- shiny::tagList()
      if (nb_input > 0){
        for (i in 1:nb_input){
          choices <- unique(PAMPA:::selectModalites.f(tableMetrique = input$maps_barplots_metric_table,
            facts = input$maps_barplots_listFact[i], selections = append(list(NA), NA),
            metrique = input$maps_barplots_metric, nextStep = next_step(), dataEnv = .GlobalEnv,
            level = 1)[, input$maps_barplots_listFact[i]])
          choices <- as.character(choices)
          output_listFactSel[[i]] <- shiny::tagList()
          output_listFactSel[[i]][[1]] <- shiny::selectInput(ns(ids[i]), labels_input[i],
            choices = c("", choices), multiple = TRUE)
        }
      }
      output_listFactSel
    })

    shiny::observeEvent(load_file(), {
      if (load_file() != 0){
        choices <- PAMPA:::champsRefspa.f(dataEnv = .GlobalEnv)
        shiny::updateSelectInput(inputId = "maps_barplots_factSpatial", choices = choices)

        choices <- PAMPA:::refTablesFields.aliases(nomTable = input$maps_barplots_metric_table, dataEnv = .GlobalEnv)
        shiny::updateSelectInput(inputId = "maps_barplots_listFact", choices = c("", choices))
      }
    })

    shiny::observeEvent(
    {
      input$maps_barplots_factSpatial
    }, {
      if(input$maps_barplots_factSpatial != "" & input$maps_barplots_factSpatial != "NA"){
        choices <- unique(PAMPA:::selectModalitesSpatiales.f(tableMetrique = input$maps_barplots_metric_table,
          facts = input$maps_barplots_factSpatial, selections = append(list(NA), NA),
          metrique = input$maps_barplots_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv)[, input$maps_barplots_factSpatial])
        choices <- sort(as.character(choices))
        shiny::updateSelectInput(inputId = "maps_barplots_factSpatialSel", choices = choices)
      }
    })

    shiny::observeEvent(
    {
      input$maps_barplots_metric_table
      load_file()
    }, {
      if (load_file() != 0){
        choices <- PAMPA:::MetricsField.aliases(input$maps_barplots_metric_table, "maps_barplots", .GlobalEnv)
        shiny::updateSelectInput(inputId = "maps_barplots_metric", choices = c("", choices))
      }
    })

    shiny::observeEvent(
    {
      input$maps_barplots_type_fact
      load_file()
    }, {
      if (load_file() != 0){
        choices <- switch(input$maps_barplots_type_fact,
          "unitobs" = PAMPA:::UnitobsFields.aliases(dataEnv = .GlobalEnv, ordered = TRUE,
            tableMetrique = input$maps_barplots_metric_table),
          "refesp" = PAMPA:::spRefFields.aliases(site = getOption("P.MPA"), dataEnv = .GlobalEnv,
            ordered = TRUE, tableMetrique = input$maps_barplots_metric_table)
        )
        shiny::updateSelectInput(inputId = "maps_barplots_factGraph", choices = c("", NA, choices))
      }
    })

    shiny::observeEvent(input$maps_barplots_factGraph, {
      if(input$maps_barplots_factGraph != "" & input$maps_barplots_factGraph != "NA"){
        choices <- unique(PAMPA:::selectModalites.f(tableMetrique = input$maps_barplots_metric_table,
          facts = input$maps_barplots_factGraph, selections = append(list(NA), NA),
          metrique = input$maps_barplots_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$maps_barplots_factGraph])
        choices <- sort(as.character(choices))
        shiny::updateSelectInput(inputId = "maps_barplots_factGraphSel", choices = c("", choices))
      } else{
        shiny::updateSelectInput(inputId = "maps_barplots_factGraphSel", choices = c())
      }
    })

    shiny::observeEvent(input$maps_barplots_launch_button, {
      shiny::showModal(shiny::modalDialog("Creation of graphics...", footer = NULL))

      params$aggregation <- input$maps_barplots_aggregation
      params$fact_spatial <- input$maps_barplots_factSpatial
      params$fact_spatial_sel <- if (!is.null(input$maps_barplots_factSpatialSel)){
        input$maps_barplots_factSpatialSel
      } else{
        NA
      }
      params$metric_table <- input$maps_barplots_metric_table
      params$metric <- input$maps_barplots_metric
      params$type_fact <- input$maps_barplots_type_fact
      params$fact_graph <- if (!is.null(input$maps_barplots_factGraph) && input$maps_barplots_factGraph != "NA"){
        input$maps_barplots_factGraph
      } else{
        ""
      }
      params$fact_graph_sel <- if (!is.null(input$maps_barplots_factGraphSel)){
        input$maps_barplots_factGraphSel
      } else{
        NA
      }
      params$list_fact <- input$maps_barplots_listFact
      params$list_fact_sel <- lapply(1:length_listFact(), function(i){
        id <- paste("maps_barplots_listFactSel_", i, sep = "")
        if (!is.null(input[[id]])) input[[id]] else NA
      })

      map <- PAMPA::maps.f(
        agregation = params$aggregation,
        graphType = "barplot",
        metrique = params$metric,
        factSpatial = params$fact_spatial,
        factSpatialSel = params$fact_spatial_sel,
        factGraph = params$fact_graph,
        factGraphSel = params$fact_graph_sel,
        listFact = params$list_fact,
        listFactSel = params$list_fact_sel,
        tableMetrique = params$metric_table,
        dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
      )

      output$maps_barplots <- leaflet::renderLeaflet({
        map
      })

      shiny::removeModal()
    })
  })
}

## To be copied in the UI
# mod_maps_barplots_ui("maps_barplots_1")

## To be copied in the server
# mod_maps_barplots_server("maps_barplots_1")
