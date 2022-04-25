#' boxplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyWidgets
mod_boxplot_ui <- function(id){
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shiny::h3("Boxplot", align = "center"),
      shiny::br(),
      shinyWidgets::radioGroupButtons(ns("boxplot_aggregation"), "Choose an aggregation",
        choices = c("espece", "unitobs"),
        justified = TRUE
      ),
      shiny::radioButtons(ns("boxplot_metric_table"), "Choose a metric table",
        choices = c(
          "... / station / size classe" = "unitSpSz",
          "... / station" = "unitSp")
      ),
      shiny::selectInput(ns("boxplot_metric"), "Select a metric",
        choices = c()
      ),
      shiny::radioButtons(ns("boxplot_type_fact"), "Subset species per...",
        choices = c(
          "... station characteristics" = "unitobs",
          "... species characteristics" = "refesp")
      ),
      shiny::selectInput(ns("boxplot_factGraph"), "Select the factor for the graphic separation",
        choices = c()
      ),
      shiny::selectInput(ns("boxplot_factGraphSel"), "Select modalities of the factor for the  graphic separation",
        choices = c(), multiple = TRUE
      ),
      shiny::selectInput(ns("boxplot_listFact"), "Select explanatory factor(s) for plotting",
        choices = c(), multiple = TRUE
      ),
      shiny::uiOutput(ns("boxplot_listFactSel")),
      shiny::div(
        shiny::actionButton(ns("boxplot_launch_button"), "Launch Graphics"),
        align = "center"
      ),
      shiny::div(
        mod_boxplot_options_ui("boxplot_options_1"),
        mod_plot_options_ui("plot_options_2"),
        align = "center"
      )
    ),
    mainPanel(
      shiny::h3("Graphics", align = "center"),
      shiny::uiOutput(ns("graph_boxplot"))
    )
  )
}

#' boxplot Server Functions
#'
#' @noRd
mod_boxplot_server <- function(id, load_file){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    length_listFact <- shiny::reactive({
      length(input$boxplot_listFact)
    })

    length_factGraphSel <- shiny::reactive({
      if (!is.null(input$boxplot_factGraphSel)){
        length(input$boxplot_factGraphSel)
      } else{
        nextStep <- switch(input$boxplot_aggregation,
          "espece" = "boxplot.esp",
          "unitobs" = "boxplot.unitobs")
        sel <- unique(PAMPA:::selectModalites.f(tableMetrique = input$boxplot_metric_table,
          facts = input$boxplot_factGraph, selections = append(list(NA), NA),
          metrique = input$boxplot_metric, nextStep = nextStep,
          dataEnv = .GlobalEnv, level = 0)[, input$boxplot_factGraph])
        sel <- as.character(sel)
        length(sel)
      }
    })

    shiny::observeEvent({
        input$boxplot_aggregation
        load_file()
      }, {
        if (input$boxplot_aggregation == "espece"){
          if (load_file() != 0 && (PAMPA:::is.benthos.f() | nrow(get("unitSpSz", envir = .GlobalEnv)) == 0)){
            shiny::updateRadioButtons(inputId = "boxplot_metric_table",
              choices = c(
                "... / station / species" = "unitSp")
            )
          } else{
            shiny::updateRadioButtons(inputId = "boxplot_metric_table",
              choices = c(
                "... / station / species / size classe" = "unitSpSz",
                "... / station / species" = "unitSp"),
              selected = "unitSp"
            )
          }
          shiny::updateRadioButtons(inputId = "boxplot_type_fact",
            choices = c(
              "... station characteristics" = "unitobs",
              "... species characteristic" = "refesp"),
            selected = "refesp"
          )
        } else{
          if (load_file() != 0 && (PAMPA:::is.benthos.f() | nrow(get("unitSpSz", envir = .GlobalEnv)) == 0)){
            shiny::updateRadioButtons(inputId = "boxplot_metric_table",
              choices = c(
                "... / station" = "unitSp",
                "... of biodiversity (/ station)" = "unit"),
              selected = "unitSp"
            )
          } else{
            shiny::updateRadioButtons(inputId = "boxplot_metric_table",
              choices = c(
                "... / station / size classe" = "unitSpSz",
                "... / station" = "unitSp",
                "... of biodiversity (/ station)" = "unit"),
              selected = "unitSp"
            )
          }
          shiny::updateRadioButtons(inputId = "boxplot_type_fact",
            choices = c(
              "... species characteristic" = "refesp")
          )
        }
    })

    output$boxplot_listFactSel <- shiny::renderUI({
      nb_input <- length_listFact()
      ids <- sapply(1:nb_input, function(i){
        paste("boxplot_listFactSel_", i, sep = "")
      })
      labels_input <- sapply(1:nb_input, function(i){
        paste("Choose modalities for ", input$boxplot_listFact[i], sep = "")
      })
      output_listFactSel <- shiny::tagList()
      if (nb_input > 0){
        for (i in 1:nb_input){
          nextStep <- switch(input$boxplot_aggregation,
            "espece" = "boxplot.esp",
            "unitobs" = "boxplot.unitobs")
          choices <- unique(selectModalites.f(tableMetrique = input$boxplot_metric_table,
            facts = input$boxplot_listFact[i], selections = append(list(NA), NA),
            metrique = input$boxplot_metric, nextStep = nextStep, dataEnv = .GlobalEnv,
            level = 1)[, input$boxplot_listFact[i]])
          choices <- as.character(choices)
          output_listFactSel[[i]] <- shiny::tagList()
          output_listFactSel[[i]][[1]] <- shiny::selectInput(ns(ids[i]), labels_input[i],
            choices = c("", choices), multiple = TRUE)
        }
      }
      output_listFactSel
    })

    shiny::observeEvent(
      {
        input$boxplot_metric_table
        load_file()
      }, {
        if (load_file() != 0){
          choices <- PAMPA:::MetricsField.aliases(input$boxplot_metric_table, "boxplot", .GlobalEnv)
          shiny::updateSelectInput(inputId = "boxplot_metric", choices = c("", choices))

          choices <- PAMPA:::refTablesFields.aliases(nomTable = input$boxplot_metric_table, dataEnv = .GlobalEnv)
          shiny::updateSelectInput(inputId = "boxplot_listFact", choices = c("", choices))
        }
    })

    shiny::observeEvent(
      {
        input$boxplot_type_fact
        load_file()
      }, {
        if (load_file() != 0){
          choices <- switch(input$boxplot_type_fact,
            "unitobs" = PAMPA:::UnitobsFields.aliases(dataEnv = .GlobalEnv, ordered = TRUE,
              tableMetrique = input$boxplot_metric_table),
            "refesp" = spRefFields.aliases(site = getOption("P.MPA"), dataEnv = .GlobalEnv,
              ordered = TRUE, tableMetrique = input$boxplot_metric_table)
          )
          shiny::updateSelectInput(inputId = "boxplot_factGraph", choices = c("", "none", " ", choices))
        }
    })

    shiny::observeEvent(input$boxplot_factGraph, {
      if(input$boxplot_factGraph != ""){
        nextStep <- switch(input$boxplot_aggregation,
          "espece" = "boxplot.esp",
          "unitobs" = "boxplot.unitobs")
        choices <- unique(PAMPA:::selectModalites.f(tableMetrique = input$boxplot_metric_table,
          facts = input$boxplot_factGraph, selections = append(list(NA), NA),
          metrique = input$boxplot_metric, nextStep = nextStep,
          dataEnv = .GlobalEnv, level = 0)[, input$boxplot_factGraph])
        choices <- as.character(choices)
        shiny::updateSelectInput(inputId = "boxplot_factGraphSel", choices = c("", choices))
      }
    })

    shiny::observeEvent(input$boxplot_launch_button, {
      shiny::showModal(shiny::modalDialog("Creation of graphics...", footer = NULL))

      aggregation = input$boxplot_aggregation
      metric_table = input$boxplot_metric_table
      metric = input$boxplot_metric
      type_fact = input$boxplot_type_fact
      fact_graph = if (!is.null(input$boxplot_factGraph)) input$boxplot_factGraph else ""
      fact_graph_sel = if (!is.null(input$boxplot_factGraphSel)){
        input$boxplot_factGraphSel
      } else{
        nextStep <- switch(input$boxplot_aggregation,
          "espece" = "boxplot.esp",
          "unitobs" = "boxplot.unitobs")
        sel <- unique(PAMPA:::selectModalites.f(tableMetrique = input$boxplot_metric_table,
          facts = input$boxplot_factGraph, selections = append(list(NA), NA),
          metrique = input$boxplot_metric, nextStep = nextStep,
          dataEnv = .GlobalEnv, level = 0)[, input$boxplot_factGraph])
        sel <- as.character(sel)
      }
      list_fact = input$boxplot_listFact
      list_fact_sel = lapply(1:length_listFact(), function(i){
        id <- paste("boxplot_listFactSel_", i, sep = "")
        if (!is.null(input[[id]])) input[[id]] else NA
      })

      output$graph_boxplot <- shiny::renderUI({
        lapply(1:isolate(length_factGraphSel()), function(iFact){
          id <- paste("boxplot_", iFact, sep = "")
          shiny::plotOutput(outputId = id)

          output[[id]] <- shiny::renderPlot({
            PAMPA::boxplot_pampa.f(
              agregation = aggregation,
              metrique = metric,
              factGraph = fact_graph,
              factGraphSel = fact_graph_sel[iFact],
              listFact = list_fact,
              listFactSel = list_fact_sel,
              tableMetrique = metric_table,
              dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
            )
          })
        })
      })
      shiny::removeModal()
    })
  })
}

## To be copied in the UI
# mod_boxplot_ui("boxplot_1")

## To be copied in the server
# mod_boxplot_server("boxplot_1")
