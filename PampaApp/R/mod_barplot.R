#' barplot UI Function
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
mod_barplot_ui <- function(id){
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shinyFeedback::useShinyFeedback(),
      shiny::h3("Barplot", align = "center"),
      shiny::h5("One value of metric per observation unit.", align = "center"),
      shiny::br(),
      shinyWidgets::radioGroupButtons(ns("barplot_aggregation"), "Choose an aggregation",
        choices = c(
          "species" = "espece",
          "species groups" = "unitobs"
        ),
        justified = TRUE
      ),
      shiny::radioButtons(ns("barplot_metric_table"), "Choose a metric table",
        choices = c(
          "... / station / size class" = "unitSpSz",
          "... / station" = "unitSp")
      ),
      shiny::selectInput(ns("barplot_metric"), "Select a metric",
        choices = c()
      ),
      shiny::radioButtons(ns("barplot_type_fact"), "Subset species per...",
        choices = c(
          "... station characteristic" = "unitobs",
          "... species characteristic" = "refesp")
      ),
      shiny::selectInput(ns("barplot_factGraph"), "Select the factor for the graphic separation",
        choices = c()
      ),
      shiny::selectInput(ns("barplot_factGraphSel"), "Select categories of the factor for the  graphic separation (all by default)",
        choices = c(), multiple = TRUE
      ),
      shiny::selectInput(ns("barplot_listFact"), "Select explanatory factor(s) for plotting",
        choices = c(), multiple = TRUE
      ),
      shiny::uiOutput(ns("barplot_listFactSel")),
      shiny::div(
        shiny::actionButton(ns("barplot_launch_button"), "Launch Graphics"),
        align = "center"
      ),
      shiny::div(
        mod_barplot_options_ui("barplot_options_1"),
        mod_plot_options_ui("plot_options_3"),
        align = "center"
      )
    ),
    shiny::mainPanel(width = 9,
      shiny::h3("Graphics", align = "center"),
      shiny::actionButton(ns("barplot_save_graphics"), "Save graphics"),
      shinycssloaders::withSpinner(
        shiny::uiOutput(ns("graph_barplot")),
        type = 3,
        color = "#CCCCCC",
        color.background = "#FFFFFF"
      )
    )
  )
}

#' barplot Server Functions
#'
#' @noRd
#'
#' @import shiny
#' @importFrom PAMPA barplot_pampa.f
#' @importFrom shinyjs reset
#' @importFrom shinyFeedback hideFeedback showFeedbackDanger
#' @importFrom R.utils setOption
mod_barplot_server <- function(id, load_file){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    length_listFact <- shiny::reactive({
      length(input$barplot_listFact)
    })

    length_factGraphSel <- shiny::reactive({
      if (!is.null(input$barplot_factGraphSel) && input$barplot_factGraphSel != "NA"){
        length(input$barplot_factGraphSel)
      } else{
        sel <- unique(PAMPA:::selectModalites.f(tableMetrique = input$barplot_metric_table,
          facts = input$barplot_factGraph, selections = append(list(NA), NA),
          metrique = input$barplot_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$barplot_factGraph])
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
      switch(input$barplot_aggregation,
        "espece" = "barplot.esp",
        "unitobs" = "barplot.unitobs")
    )

    shiny::observeEvent(load_file(), {
      shinyjs::reset(id = "barplot_factGraphSel")
      output$graph_barplot <- NULL
    })

    shiny::observeEvent({
        input$barplot_aggregation
        load_file()
      }, {
        if (input$barplot_aggregation == "espece"){
          if (load_file() != 0 && (PAMPA:::is.benthos.f() | nrow(get("unitSpSz", envir = .GlobalEnv)) == 0)){
            shiny::updateRadioButtons(inputId = "barplot_metric_table",
              choices = c(
                "... / station / species" = "unitSp")
            )
          } else{
            shiny::updateRadioButtons(inputId = "barplot_metric_table",
              choices = c(
                "... / station / species / size class" = "unitSpSz",
                "... / station / species" = "unitSp"),
              selected = "unitSp"
            )
          }
          shiny::updateRadioButtons(inputId = "barplot_type_fact",
            label = "Generate one plot per...",
            choices = c(
              "... station characteristic" = "unitobs",
              "... species characteristic" = "refesp"),
            selected = "refesp"
          )
        } else{
          if (load_file() != 0 && (PAMPA:::is.benthos.f() | nrow(get("unitSpSz", envir = .GlobalEnv)) == 0)){
            shiny::updateRadioButtons(inputId = "barplot_metric_table",
              choices = c(
                "... / station" = "unitSp",
                "... of biodiversity (/ station)" = "unit"),
              selected = "unitSp"
            )
          } else{
            shiny::updateRadioButtons(inputId = "barplot_metric_table",
              choices = c(
                "... / station / size class" = "unitSpSz",
                "... / station" = "unitSp",
                "... of biodiversity (/ station)" = "unit"),
              selected = "unitSp"
            )
          }
          shiny::updateRadioButtons(inputId = "barplot_type_fact",
            label = "Subset species per...",
            choices = c(
              "... species characteristic" = "refesp")
          )
        }
    })

    output$barplot_listFactSel <- shiny::renderUI({
      nb_input <- length_listFact()
      ids <- sapply(1:nb_input, function(i){
        paste("barplot_listFactSel_", i, sep = "")
      })
      labels_input <- sapply(1:nb_input, function(i){
        paste("Choose categories for ", input$barplot_listFact[i], " (all by default)", sep = "")
      })
      output_listFactSel <- shiny::tagList()
      if (nb_input > 0){
        for (i in 1:nb_input){
          choices <- unique(PAMPA:::selectModalites.f(tableMetrique = input$barplot_metric_table,
            facts = input$barplot_listFact[i], selections = append(list(NA), NA),
            metrique = input$barplot_metric, nextStep = next_step(), dataEnv = .GlobalEnv,
            level = 1)[, input$barplot_listFact[i]])
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
        input$barplot_metric_table
        load_file()
      }, {
        if (load_file() != 0){
          choices <- PAMPA:::MetricsField.aliases(input$barplot_metric_table, "barplot", .GlobalEnv)
          shiny::updateSelectInput(inputId = "barplot_metric", choices = c("", choices))

          choices <- PAMPA:::refTablesFields.aliases(nomTable = input$barplot_metric_table, dataEnv = .GlobalEnv)
          shiny::updateSelectInput(inputId = "barplot_listFact", choices = c("", choices))
        }
    })

    shiny::observeEvent(
      {
        input$barplot_type_fact
        load_file()
      }, {
        if (load_file() != 0){
          choices <- switch(input$barplot_type_fact,
            "unitobs" = PAMPA:::UnitobsFields.aliases(dataEnv = .GlobalEnv, ordered = TRUE,
              tableMetrique = input$barplot_metric_table),
            "refesp" = PAMPA:::spRefFields.aliases(site = getOption("P.MPA"), dataEnv = .GlobalEnv,
              ordered = TRUE, tableMetrique = input$barplot_metric_table)
          )
          shiny::updateSelectInput(inputId = "barplot_factGraph", choices = c("", NA, choices))
        }
    })

    shiny::observeEvent(input$barplot_factGraph, {
      if(input$barplot_factGraph != "" & input$barplot_factGraph != "NA"){
        choices <- unique(PAMPA:::selectModalites.f(tableMetrique = input$barplot_metric_table,
          facts = input$barplot_factGraph, selections = append(list(NA), NA),
          metrique = input$barplot_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$barplot_factGraph])
        choices <- sort(as.character(choices))
        shiny::updateSelectInput(inputId = "barplot_factGraphSel", choices = c("", choices))
      } else{
        shiny::updateSelectInput(inputId = "barplot_factGraphSel", choices = c())
      }
    })

    shiny::observeEvent(input$barplot_metric, {
      shinyFeedback::hideFeedback("barplot_metric")
    })

    shiny::observeEvent(input$barplot_listFact, {
      shinyFeedback::hideFeedback("barplot_listFact")
      if (length_listFact() > 2){
        shinyFeedback::showFeedbackDanger("barplot_listFact", text = "Cannot have more than 2 explanatory factors.")
      }
    })

    shiny::observeEvent(input$barplot_launch_button, {
      error <- FALSE
      if (input$barplot_metric == ""){
        shinyFeedback::showFeedbackDanger("barplot_metric", text = "A metric is required.")
        error <- TRUE
      }
      if (is.null(input$barplot_listFact)){
        shinyFeedback::showFeedbackDanger("barplot_listFact", text = "Explanatory factor(s) are required.")
        error <- TRUE
      }
      if (length_listFact() > 2){
        error <- TRUE
      }
      if (error){
        shiny::req(NULL)
      }

      shiny::showModal(shiny::modalDialog("Creation of graphics...", footer = NULL))

      params$aggregation <- input$barplot_aggregation
      params$metric_table <- input$barplot_metric_table
      params$metric <- input$barplot_metric
      params$type_fact <- input$barplot_type_fact
      params$fact_graph <- if (!is.null(input$barplot_factGraph) && input$barplot_factGraph != "NA"){
        input$barplot_factGraph
      } else{
        ""
      }
      params$fact_graph_sel <- if (!is.null(input$barplot_factGraphSel)){
        input$barplot_factGraphSel
      } else if (params$fact_graph != ""){
        sel <- unique(PAMPA:::selectModalites.f(tableMetrique = input$barplot_metric_table,
          facts = input$barplot_factGraph, selections = append(list(NA), NA),
          metrique = input$barplot_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$barplot_factGraph])
        sel <- sort(as.character(sel))
      } else{
        NA
      }
      params$list_fact <- input$barplot_listFact
      params$list_fact_sel <- lapply(1:length_listFact(), function(i){
        id <- paste("barplot_listFactSel_", i, sep = "")
        if (!is.null(input[[id]])) input[[id]] else NA
      })

      if (params$aggregation == "espece" & params$fact_graph != ""){
        output$graph_barplot <- shiny::renderUI({
          lapply(1:isolate(length_factGraphSel()), function(iFact){
            id <- paste("barplot_", iFact, sep = "")
            shiny::plotOutput(outputId = id)

            output[[id]] <- shiny::renderPlot({
              PAMPA::barplot_pampa.f(
                agregation = params$aggregation,
                metrique = params$metric,
                factGraph = params$fact_graph,
                factGraphSel = params$fact_graph_sel[iFact],
                listFact = params$list_fact,
                listFactSel = params$list_fact_sel,
                tableMetrique = params$metric_table,
                new_window = FALSE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
              )
            })
          })
        })
      } else{
        output$graph_barplot <- shiny::renderUI({
          id <- "barplot"
          shiny::plotOutput(outputId = id)

          output[[id]] <- shiny::renderPlot({
            PAMPA::barplot_pampa.f(
              agregation = params$aggregation,
              metrique = params$metric,
              factGraph = params$fact_graph,
              factGraphSel = params$fact_graph_sel,
              listFact = params$list_fact,
              listFactSel = params$list_fact_sel,
              tableMetrique = params$metric_table,
              new_window = FALSE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
            )
          })
        })
      }
      shiny::removeModal()
    })

    shiny::observeEvent(input$barplot_save_graphics, {
      shiny::showModal(shiny::modalDialog(
        shiny::checkboxGroupInput(ns("barplot_export_format"), "Choose the format for the file:",
          choices = c("pdf", "png", "wmf")),
        shiny::h5("The files will be saved at:"),
        shiny::code(get("filePathes", envir = .GlobalEnv)["results"],
          style = "color: #000000; background-color: #ffffff"),
        title = "Save graphics",
        footer = shiny::tagList(
          shiny::actionButton(ns("barplot_save"), "Save"),
          shiny::modalButton("Cancel")
        )
      ))
    })

    shiny::observeEvent(input$barplot_save, {
      if ("pdf" %in% input$barplot_export_format){
        R.utils::setOption("P.graphPDF", TRUE)
        PAMPA::barplot_pampa.f(
          agregation = params$aggregation,
          metrique = params$metric,
          factGraph = params$fact_graph,
          factGraphSel = params$fact_graph_sel,
          listFact = params$list_fact,
          listFactSel = params$list_fact_sel,
          tableMetrique = params$metric_table,
          new_window = TRUE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
        )
        R.utils::setOption("P.graphPDF", FALSE)
      }
      if ("png" %in% input$barplot_export_format){
        R.utils::setOption("P.graphPNG", TRUE)
        PAMPA::barplot_pampa.f(
          agregation = params$aggregation,
          metrique = params$metric,
          factGraph = params$fact_graph,
          factGraphSel = params$fact_graph_sel,
          listFact = params$list_fact,
          listFactSel = params$list_fact_sel,
          tableMetrique = params$metric_table,
          new_window = TRUE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
        )
        R.utils::setOption("P.graphPNG", FALSE)
      }
      if ("wmf" %in% input$barplot_export_format){
        R.utils::setOption("P.graphWMF", TRUE)
        PAMPA::barplot_pampa.f(
          agregation = params$aggregation,
          metrique = params$metric,
          factGraph = params$fact_graph,
          factGraphSel = params$fact_graph_sel,
          listFact = params$list_fact,
          listFactSel = params$list_fact_sel,
          tableMetrique = params$metric_table,
          new_window = TRUE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
        )
        R.utils::setOption("P.graphWMF", FALSE)
      }
      shiny::removeModal()
    })
  })
}

## To be copied in the UI
# mod_barplot_ui("barplot_1")

## To be copied in the server
# mod_barplot_server("barplot_1")
