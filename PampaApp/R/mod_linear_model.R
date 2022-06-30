#' linear_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinycssloaders withSpinner
mod_linear_model_ui <- function(id){
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shinyFeedback::useShinyFeedback(),
      shiny::h3("Linear models", align = "center"),
      shiny::br(),
      shinyWidgets::radioGroupButtons(ns("linear_model_aggregation"), "Choose an aggregation",
        choices = c(
          "species" = "espece",
          "species groups" = "unitobs"
        ),
        justified = TRUE
      ),
      shiny::radioButtons(ns("linear_model_metric_table"), "Choose a metric table",
        choices = c(
          "... / station / size class" = "unitSpSz",
          "... / station" = "unitSp")
      ),
      shiny::selectInput(ns("linear_model_metric"), "Select a metric",
        choices = c()
      ),
      shiny::radioButtons(ns("linear_model_type_fact"), "Subset species per...",
        choices = c(
          "... station characteristic" = "unitobs",
          "... species characteristic" = "refesp")
      ),
      shiny::selectInput(ns("linear_model_factGraph"), "Select the factor for the graphic separation",
        choices = c()
      ),
      shiny::selectInput(ns("linear_model_factGraphSel"), "Select categories of the factor for the graphic separation (all by default)",
        choices = c(), multiple = TRUE
      ),
      shiny::selectInput(ns("linear_model_listFact"), "Select explanatory factor(s) for plotting",
        choices = c(), multiple = TRUE
      ),
      shiny::uiOutput(ns("linear_model_listFactSel")),
      shiny::div(
        shiny::actionButton(ns("linear_model_launch_button"), "Launch Analysis"),
        align = "center"
      )
    ),
    shiny::mainPanel(width = 9,
      shiny::h3("Graphics", align = "center"),
      shinycssloaders::withSpinner(
        shiny::uiOutput(ns("stats_linear_model")),
        type = 3,
        color = "#CCCCCC",
        color.background = "#FFFFFF"
      )
    )
  )
}

#' linear_model Server Functions
#'
#' @noRd
#'
#' @import shiny
#' @importFrom PAMPA lm.f
#' @importFrom shinyjs reset
#' @importFrom shinyFeedback hideFeedback showFeedbackWarning showFeedbackDanger
mod_linear_model_server <- function(id, load_file){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    length_listFact <- shiny::reactive({
      length(input$linear_model_listFact)
    })

    length_factGraphSel <- shiny::reactive({
      if (!is.null(input$linear_model_factGraphSel) && input$linear_model_factGraphSel != "NA"){
        length(input$linear_model_factGraphSel)
      } else{
        sel <- unique(PAMPA:::selectModalites.f(tableMetrique = input$linear_model_metric_table,
          facts = input$linear_model_factGraph, selections = append(list(NA), NA),
          metrique = input$linear_model_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$linear_model_factGraph])
        sel <- sort(as.character(sel))
        length(sel)
      }
    })

    params <- shiny::reactiveValues(
      aggregation = NULL,
      metric_table = NULL,
      metric = NULL,
      type_fact = NULL,
      fact_graph = NULL,
      fact_graph_sel = NULL,
      list_fact = NULL,
      list_fact_sel = NULL
    )

    next_step <- shiny::reactive(
      switch(input$linear_model_aggregation,
        "espece" = "modele_lineaire",
        "unitobs" = "modele_lineaire.unitobs")
    )

    shiny::observeEvent(load_file(), {
      shinyjs::reset(id = "linear_model_factGraphSel")
    })

    shiny::observeEvent({
        input$linear_model_aggregation
        load_file()
      }, {
        if (input$linear_model_aggregation == "espece"){
          if (load_file() != 0 && (PAMPA:::is.benthos.f() | nrow(get("unitSpSz", envir = .GlobalEnv)) == 0)){
            shiny::updateRadioButtons(inputId = "linear_model_metric_table",
              choices = c(
                "... / station / species" = "unitSp")
            )
          } else{
            shiny::updateRadioButtons(inputId = "linear_model_metric_table",
              choices = c(
                "... / station / species / size class" = "unitSpSz",
                "... / station / species" = "unitSp"),
              selected = "unitSp"
            )
          }
          shiny::updateRadioButtons(inputId = "linear_model_type_fact",
            label = "Generate one plot per...",
            choices = c(
              "... station characteristic" = "unitobs",
              "... species characteristic" = "refesp"),
            selected = "refesp"
          )
        } else{
          if (load_file() != 0 && (PAMPA:::is.benthos.f() | nrow(get("unitSpSz", envir = .GlobalEnv)) == 0)){
            shiny::updateRadioButtons(inputId = "linear_model_metric_table",
              choices = c(
                "... / station" = "unitSp",
                "... of biodiversity (/station)" = "unit"),
              selected = "unitSp"
            )
          } else{
            shiny::updateRadioButtons(inputId = "linear_model_metric_table",
              choices = c(
                "... / station / size class" = "unitSpSz",
                "... / station" = "unitSp",
                "... of biodiversity (/station)" = "unit"),
              selected = "unitSp"
            )
          }
          shiny::updateRadioButtons(inputId = "linear_model_type_fact",
            label = "Subset species per...",
            choices = c(
              "... species characteristic" = "refesp")
          )
        }
    })

    output$linear_model_listFactSel <- shiny::renderUI({
      nb_input <- length_listFact()
      ids <- sapply(1:nb_input, function (i){
        paste("linear_model_listFactSel_", i, sep = "")
      })
      labels_input <- sapply(1:nb_input, function(i){
        paste("Choose categories for ", input$linear_model_listFact[i], "(all by default)", sep = "")
      })
      output_listFactSel <- shiny::tagList()
      if (nb_input > 0){
        for (i in 1:nb_input){
          choices <- unique(PAMPA:::selectModalites.f(tableMetrique = input$linear_model_metric_table,
            facts = input$linear_model_listFact[i], selections = append(list(NA), NA),
            metrique = input$linear_model_metric, nextStep = next_step(), dataEnv = .GlobalEnv,
            level = 1)[, input$linear_model_listFact[i]])
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
        input$linear_model_metric_table
        load_file()
      }, {
        if (load_file() != 0){
          choices <- PAMPA:::MetricsField.aliases(input$linear_model_metric_table, next_step(), .GlobalEnv)
          shiny::updateSelectInput(inputId = "linear_model_metric", choices = c("", choices))

          choices <- PAMPA:::refTablesFields.aliases(nomTable = input$linear_model_metric_table, dataEnv = .GlobalEnv)
          shiny::updateSelectInput(inputId = "linear_model_listFact", choices = c("", choices))
        }
    })

    shiny::observeEvent(
      {
        input$linear_model_type_fact
        load_file()
      }, {
        if (load_file() != 0){
          choices <- switch(input$linear_model_type_fact,
            "unitobs" = PAMPA:::UnitobsFields.aliases(dataEnv = .GlobalEnv, ordered = TRUE,
              tableMetrique = input$linear_model_metric_table),
            "refesp" = PAMPA:::spRefFields.aliases(site = getOption("P.MPA"), dataEnv = .GlobalEnv,
              ordered = TRUE, tableMetrique = input$linear_model_metric_table)
          )
          shiny::updateSelectInput(inputId = "linear_model_factGraph", choices = c("", NA, choices))
        }
    })

    shiny::observeEvent(input$linear_model_factGraph, {
      if (input$linear_model_factGraph != "" & input$linear_model_factGraph != "NA"){
        choices <- unique(PAMPA:::selectModalites.f(tableMetrique = input$linear_model_metric_table,
          facts = input$linear_model_factGraph, selections = append(list(NA), NA),
          metrique = input$linear_model_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$linear_model_factGraph])
        choices <- sort(as.character(choices))
        shiny::updateSelectInput(inputId = "linear_model_factGraphSel", choices = c("", choices))
      } else{
        shiny::updateSelectInput(inputId = "linear_model_factGraphSel", choices = c())
      }
    })

    shiny::observeEvent(input$linear_model_metric, {
      shinyFeedback::hideFeedback("linear_model_metric")
    })

    shiny::observeEvent(input$linear_model_listFact, {
      shinyFeedback::hideFeedback("linear_model_listFact")
      if (length_listFact() == 3){
        shinyFeedback::showFeedbackWarning("linear_model_listFact", text = "From 3 grouping factors, the results become difficult to exploit")
      }
      if (length_listFact() > 3){
        shinyFeedback::showFeedbackDanger("linear_model_listFact", text = "Too many grouping factors selected, the results would not be exploitable.")
      }
    })

    shiny::observeEvent(input$linear_model_launch_button, {
      error <- FALSE
      if (input$linear_model_metric == ""){
        shinyFeedback::showFeedbackDanger("linear_model_metric", text = "A metric is required.")
        error <- TRUE
      }
      if (is.null(input$linear_model_listFact)){
        shinyFeedback::showFeedbackDanger("linear_model_listFact", text = "Explanatory factor(s) are required.")
        error <- TRUE
      }
      if ( length_listFact() > 3){
        error <- TRUE
      }
      if (error){
        shiny::req(NULL)
      }

      shiny::showModal(shiny::modalDialog("Creation of graphics...", footer = NULL))

      params$aggregation <- input$linear_model_aggregation
      params$metric_table <- input$linear_model_metric_table
      params$metric <- input$linear_model_metric
      params$type_fact <- input$linear_model_type_fact
      params$fact_graph <- if (!is.null(input$linear_model_factGraph) && input$linear_model_factGraph != "NA"){
        input$linear_model_factGraph
      } else{
        ""
      }
      params$fact_graph_sel <- if (!is.null(input$linear_model_factGraphSel)){
        input$linear_model_factGraphSel
      } else if (params$fact_graph != ""){
        sel <- unique(PAMPA:::selectModalites.f(tableMetrique = input$linear_model_metric_table,
          facts = input$linear_model_factGraph, selections = append(list(NA), NA),
          metrique = input$linear_model_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$linear_model_factGraph])
        sel <- sort(as.character(sel))
      } else{
        NA
      }
      params$list_fact <- input$linear_model_listFact
      params$list_fact_sel <- lapply(1:length_listFact(), function(i){
        id <- paste("linear_model_listFactSel_", i, sep = "")
        if (!is.null(input[[id]])) input[[id]] else NA
      })

      if (params$aggregation == "espece" & params$fact_graph != ""){
        output$stats_linear_model <- shiny::renderUI({
          lapply(1:isolate(length_factGraphSel()), function(iFact){
            id <- paste("linear_model_", iFact, sep = "")
            shiny::plotOutput(outputId = id)

            output[[id]] <- shiny::renderPlot({
              PAMPA::lm.f(
                agregation = params$aggregation,
                metrique = params$metric,
                factAna = params$fact_graph,
                factAnaSel = params$fact_graph_sel[iFact],
                listFact = params$list_fact,
                listFactSel = params$list_fact_sel,
                tableMetrique = params$metric_table,
                new_window = FALSE,
                dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
              )
            })
          })
        })
      } else{
        output$stats_linear_model <- shiny::renderUI({
          id <- "linear_model"
          shiny::plotOutput(outputId = id)

          output[[id]] <- shiny::renderPlot({
            PAMPA::lm.f(
              agregation = params$aggregation,
              metrique = params$metric,
              factAna = params$fact_graph,
              factAnaSel = params$fact_graph_sel,
              listFact = params$list_fact,
              listFactSel = params$list_fact_sel,
              tableMetrique = params$metric_table,
              new_window = FALSE,
              dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
            )
          })
        })
      }
      shiny::removeModal()
    })
  })
}

## To be copied in the UI
# mod_linear_model_ui("linear_model_1")

## To be copied in the server
# mod_linear_model_server("linear_model_1")
