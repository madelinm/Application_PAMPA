#' maps_boxplots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_maps_boxplots_ui <- function(id){
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shiny::h3("Maps with boxplots", align = "center"),
      shiny::br(),
      shinyWidgets::radioGroupButtons(ns("maps_boxplots_aggregation"), "Choose an aggregation",
        choices = c(
          "species" = "espece",
          "species groups" = "unitobs"
        ),
        justified = TRUE
      ),
      shiny::selectInput(ns("maps_boxplots_factSpatial"), "Select a spatial grouping factor",
        choices = c()
      ),
      shiny::selectInput(ns("maps_boxplots_factSpatialSel"), "Select categories for the spatial grouping factor (all by default)",
        choices = c(), multiple = TRUE
      ),
      shiny::radioButtons(ns("maps_boxplots_metric_table"), "Choose a metric table",
        choices = c(
          "... / station / size class" = "unitSpSz",
          "... / station" = "unitSp")
      ),
      shiny::selectInput(ns("maps_boxplots_metric"), "Select a metric",
        choices = c()
      ),
      shiny::radioButtons(ns("maps_boxplots_type_fact"), "Subset species per...",
        choices = c(
          "... station characteristic" = "unitobs",
          "... species characteristic" = "refesp")
      ),
      shiny::selectInput(ns("maps_boxplots_factGraph"), "select the factor for the graphic separation",
        choices = c()
      ),
      shiny::selectInput(ns("maps_boxplots_factGraphSel"), "Select categories of the factor for the  graphic separation (all by default)",
        choices = c(), multiple = TRUE
      ),
      shiny::selectInput(ns("maps_boxplots_listFact"), "Select explanatory factor(s) for plotting",
        choices = c(), multiple = TRUE
      ),
      shiny::uiOutput(ns("maps_boxplots_listFactSel")),
      shiny::div(
        shiny::actionButton(ns("maps_boxplots_launch_button"), "Launch Maps"),
        align = "center"
      )
    ),
    shiny::mainPanel(width = 9,
      shiny::h3("Maps", align = "center"),
      leaflet::leafletOutput(ns("maps_boxplots"), width = "100%", height = "750px")
#      mapview::mapviewOutput(ns("maps_boxplots"))
    )
  )
}

#' maps_boxplots Server Functions
#'
#' @noRd
mod_maps_boxplots_server <- function(id, load_file){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    length_listFact <- shiny::reactive({
      length(input$maps_boxplots_listFact)
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
      switch(input$maps_boxplots_aggregation,
        "espece" = "spBarBoxplot.esp",
        "unitobs" = "spBarBoxplot.unitobs")
    )

    shiny::observeEvent(input$maps_boxplots_aggregation, {
      if (input$maps_boxplots_aggregation == "espece"){
        if (load_file() != 0 && (!PAMPA:::is.benthos.f() | nrow(get("unitSpSz", envir = .GlobalEnv)) == 0)){
          shiny::updateRadioButtons(inputId = "maps_boxplots_metric_table",
            choices = c(
              "... / station / species" = "unitSp")
          )
        } else{
          shiny::updateRadioButtons(inputId = "maps_boxplots_metric_table",
            choices = c(
              "... / station / species / size classe" = "unitSpSz",
              "... / station / species" = "unitSp"),
            selected = "unitSp"
          )
        }
        shiny::updateRadioButtons(inputId = "maps_boxplots_type_fact",
          label = "Generate one plot per...",
          choices = c(
            "... station characteristic" = "unitobs",
            "... species characteristic" = "refesp"),
          selected = "refesp"
        )
      } else{
        if (load_file() != 0 && (!PAMPA:::is.benthos.f() | nrow(get("unitSpSz", envir = .GlobalEnv)) == 0)){
          shiny::updateRadioButtons(inputId = "maps_boxplots_metric_table",
            choices = c(
              "... / station" = "unitSp",
              "... of biodiversity (/ station)" = "unit"),
            selected = "unitSp"
          )
        } else{
          shiny::updateRadioButtons(inputId = "maps_boxplots_metric_table",
            choices = c(
              "... / station / size classe" = "unitSpSz",
              "... / station" = "unitSp",
              "... of biodiversity (/ station)" = "unit"),
            selected = "unitSp"
          )
        }
        shiny::updateRadioButtons(inputId = "maps_boxplots_type_fact",
          label = "Subset species per...",
          choices = c(
            "... species characteristic" = "refesp")
        )
      }
    })

    output$maps_boxplots_listFactSel <- shiny::renderUI({
      nb_input <- length_listFact()
      ids <- sapply(1:nb_input, function(i){
        paste("maps_boxplots_listFactSel_", i, sep = "")
      })
      labels_input <- sapply(1:nb_input, function(i){
        paste("Choose categories for ", input$maps_boxplots_listFact[i], " (all by default)", sep = "")
      })
      output_listFactSel <- shiny::tagList()
      if (nb_input > 0){
        for (i in 1:nb_input){
          choices <- unique(PAMPA:::selectModalites.f(tableMetrique = input$maps_boxplots_metric_table,
            facts = input$maps_boxplots_listFact[i], selections = append(list(NA), NA),
            metrique = input$maps_boxplots_metric, nextStep = next_step(), dataEnv = .GlobalEnv,
            level = 1)[, input$maps_boxplots_listFact[i]])
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
        shiny::updateSelectInput(inputId = "maps_boxplots_factSpatial", choices = choices)

        choices <- PAMPA:::refTablesFields.aliases(nomTable = input$maps_boxplots_metric_table, dataEnv = .GlobalEnv)
        shiny::updateSelectInput(inputId = "maps_boxplots_listFact", choices = c("", choices))
      }
    })

    shiny::observeEvent(
    {
      input$maps_boxplots_factSpatial
    }, {
      if(input$maps_boxplots_factSpatial != "" & input$maps_boxplots_factSpatial != "NA"){
        choices <- unique(selectModalitesSpatiales.f(tableMetrique = input$maps_boxplots_metric_table,
          facts = input$maps_boxplots_factSpatial, selections = append(list(NA), NA),
          metrique = input$maps_boxplots_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv)[, input$maps_boxplots_factSpatial])
        choices <- sort(as.character(choices))
        shiny::updateSelectInput(inputId = "maps_boxplots_factSpatialSel", choices = choices)
      }
    })

    shiny::observeEvent(
    {
      input$maps_boxplots_metric_table
      load_file()
    }, {
      if (load_file() != 0){
        choices <- PAMPA:::MetricsField.aliases(input$maps_boxplots_metric_table, "maps_boxplots", .GlobalEnv)
        shiny::updateSelectInput(inputId = "maps_boxplots_metric", choices = c("", choices))
      }
    })

    shiny::observeEvent(
    {
      input$maps_boxplots_type_fact
      load_file()
    }, {
      if (load_file() != 0){
        choices <- switch(input$maps_boxplots_type_fact,
          "unitobs" = PAMPA:::UnitobsFields.aliases(dataEnv = .GlobalEnv, ordered = TRUE,
            tableMetrique = input$maps_boxplots_metric_table),
          "refesp" = PAMPA:::spRefFields.aliases(site = getOption("P.MPA"), dataEnv = .GlobalEnv,
            ordered = TRUE, tableMetrique = input$maps_boxplots_metric_table)
        )
        shiny::updateSelectInput(inputId = "maps_boxplots_factGraph", choices = c("", NA, choices))
      }
    })

    shiny::observeEvent(input$maps_boxplots_factGraph, {
      if(input$maps_boxplots_factGraph != "" & input$maps_boxplots_factGraph != "NA"){
        choices <- unique(PAMPA:::selectModalites.f(tableMetrique = input$maps_boxplots_metric_table,
          facts = input$maps_boxplots_factGraph, selections = append(list(NA), NA),
          metrique = input$maps_boxplots_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$maps_boxplots_factGraph])
        choices <- sort(as.character(choices))
        shiny::updateSelectInput(inputId = "maps_boxplots_factGraphSel", choices = c("", NA, choices))
      } else{
        shiny::updateSelectInput(inputId = "maps_boxplots_factGraphSel", choices = c())
      }
    })

    shiny::observeEvent(input$maps_boxplots_launch_button, {
      shiny::showModal(shiny::modalDialog("Creation of graphics...", footer = NULL))

      params$aggregation <- input$maps_boxplots_aggregation
      params$fact_spatial <- input$maps_boxplots_factSpatial
      params$fact_spatial_sel <- if (!is.null(input$maps_boxplots_factSpatialSel) && input$maps_boxplots_factSpatialSel != "NA"){
        input$maps_boxplots_factSpatialSel
      } else{
        NA
      }
      params$metric_table <- input$maps_boxplots_metric_table
      params$metric <- input$maps_boxplots_metric
      params$type_fact <- input$maps_boxplots_type_fact
      params$fact_graph <- if (!is.null(input$maps_boxplots_factGraph) && input$maps_boxplots_factGraph != "NA"){
        input$maps_boxplots_factGraph
      } else{
        ""
      }
      params$fact_graph_sel <- if (!is.null(input$maps_boxplots_factGraphSel) && input$maps_boxplots_factGraphSel != "NA"){
        input$maps_boxplots_factGraphSel
      } else{
        NA
      }
      params$list_fact <- input$maps_boxplots_listFact
      params$list_fact_sel <- lapply(1:length_listFact(), function(i){
        id <- paste("maps_boxplots_listFactSel_", i, sep = "")
        if (!is.null(input[[id]])) input[[id]] else NA
      })

      map <- PAMPA::maps.f(
        agregation = params$aggregation,
        graphType = "boxplot",
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

      output$maps_boxplots <- leaflet::renderLeaflet({
        map@map
      })

#      output$maps_boxplots <- mapview::renderMapview(map)

      shiny::removeModal()
    })
  })
}

## To be copied in the UI
# mod_maps_boxplots_ui("maps_boxplots_1")

## To be copied in the server
# mod_maps_boxplots_server("maps_boxplots_1")
