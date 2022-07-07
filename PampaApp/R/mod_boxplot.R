#' boxplot UI Function
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
mod_boxplot_ui <- function(id){
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shinyFeedback::useShinyFeedback(),
      shiny::h3("Boxplot", align = "center"),
      shiny::h5("One value of metric per observation unit.", align = "center"),
      shiny::br(),
      shinyWidgets::radioGroupButtons(ns("boxplot_aggregation"), "Choose an aggregation",
        choices = c(
          "species" = "espece",
          "species groups" = "unitobs"
        ),
        justified = TRUE
      ),
      shiny::radioButtons(ns("boxplot_metric_table"), "Choose a metric table",
        choices = c(
          "... / station / size class" = "unitSpSz",
          "... / station" = "unitSp")
      ),
      shiny::selectInput(ns("boxplot_metric"), "Select a metric",
        choices = c()
      ),
      shiny::radioButtons(ns("boxplot_type_fact"), "Subset species per...",
        choices = c(
          "... station characteristic" = "unitobs",
          "... species characteristic" = "refesp")
      ),
      shiny::selectInput(ns("boxplot_factGraph"), "Select the factor for the graphic separation",
        choices = c()
      ),
      shiny::selectInput(ns("boxplot_factGraphSel"), "Select categories of the factor for the  graphic separation (all by default)",
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
    shiny::mainPanel(width = 9,
      shiny::h3("Graphics", align = "center"),
      shiny::actionButton(ns("boxplot_save_graphics"), "Save graphics"),
      shinycssloaders::withSpinner(
        shiny::uiOutput(ns("graph_boxplot")),
        type = 3,
        color = "#CCCCCC",
        color.background = "#FFFFFF"
      )
    )
  )
}

#' boxplot Server Functions
#'
#' @noRd
#'
#' @import shiny
#' @importFrom PAMPA boxplot_pampa.f
#' @importFrom shinyjs reset
#' @importFrom shinyFeedback hideFeedback showFeedbackDanger
#' @importFrom R.utils setOption
mod_boxplot_server <- function(id, load_file){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    length_listFact <- shiny::reactive({
      length(input$boxplot_listFact)
    })

    length_factGraphSel <- shiny::reactive({
      if (!is.null(input$boxplot_factGraphSel) && input$boxplot_factGraphSel != "NA"){
        length(input$boxplot_factGraphSel)
      } else{
        sel <- unique(PAMPA:::selectModalites.f(tableMetrique = input$boxplot_metric_table,
          facts = input$boxplot_factGraph, selections = append(list(NA), NA),
          metrique = input$boxplot_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$boxplot_factGraph])
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
      switch(input$boxplot_aggregation,
        "espece" = "boxplot.esp",
        "unitobs" = "boxplot.unitobs")
    )

    shiny::observeEvent(load_file(), {
      shinyjs::reset(id = "boxplot_factGraphSel")
      output$graph_boxplot <- NULL
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
                "... / station / species / size class" = "unitSpSz",
                "... / station / species" = "unitSp"),
              selected = "unitSp"
            )
          }
          shiny::updateRadioButtons(inputId = "boxplot_type_fact",
            label = "Generate one plot per...",
            choices = c(
              "... station characteristic" = "unitobs",
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
                "... / station / size class" = "unitSpSz",
                "... / station" = "unitSp",
                "... of biodiversity (/ station)" = "unit"),
              selected = "unitSp"
            )
          }
          shiny::updateRadioButtons(inputId = "boxplot_type_fact",
            label = "Subset species per...",
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
        paste("Choose categories for ", input$boxplot_listFact[i], " (all by default)", sep = "")
      })
      output_listFactSel <- shiny::tagList()
      if (nb_input > 0){
        for (i in 1:nb_input){
          choices <- unique(PAMPA:::selectModalites.f(tableMetrique = input$boxplot_metric_table,
            facts = input$boxplot_listFact[i], selections = append(list(NA), NA),
            metrique = input$boxplot_metric, nextStep = next_step(), dataEnv = .GlobalEnv,
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
            "refesp" = PAMPA:::spRefFields.aliases(site = getOption("P.MPA"), dataEnv = .GlobalEnv,
              ordered = TRUE, tableMetrique = input$boxplot_metric_table)
          )
          shiny::updateSelectInput(inputId = "boxplot_factGraph", choices = c("", NA, choices))
        }
    })

    shiny::observeEvent(input$boxplot_factGraph, {
      if(input$boxplot_factGraph != "" & input$boxplot_factGraph != "NA"){
        choices <- unique(PAMPA:::selectModalites.f(tableMetrique = input$boxplot_metric_table,
          facts = input$boxplot_factGraph, selections = append(list(NA), NA),
          metrique = input$boxplot_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$boxplot_factGraph])
        choices <- sort(as.character(choices))
        shiny::updateSelectInput(inputId = "boxplot_factGraphSel", choices = c("", choices))
      } else{
        shiny::updateSelectInput(inputId = "boxplot_factGraphSel", choices = c())
      }
    })

    shiny::observeEvent(input$boxplot_metric, {
      shinyFeedback::hideFeedback("boxplot_metric")
    })

    shiny::observeEvent(input$boxplot_listFact, {
      shinyFeedback::hideFeedback("boxplot_listFact")
    })

    shiny::observeEvent(input$boxplot_launch_button, {
      error <- FALSE
      if (input$boxplot_metric == ""){
        shinyFeedback::showFeedbackDanger("boxplot_metric", text = "A metric is required.")
        error <- TRUE
      }
      if (is.null(input$boxplot_listFact)){
        shinyFeedback::showFeedbackDanger("boxplot_listFact", text = "Explanatory factor(s) are required.")
        error <- TRUE
      }
      if (error){
        shiny::req(NULL)
      }

      shiny::showModal(shiny::modalDialog("Creation of graphics...", footer = NULL))

      params$aggregation <- input$boxplot_aggregation
      params$metric_table <- input$boxplot_metric_table
      params$metric <- input$boxplot_metric
      params$type_fact <- input$boxplot_type_fact
      params$fact_graph <- if (!is.null(input$boxplot_factGraph) && input$boxplot_factGraph != "NA"){
        input$boxplot_factGraph
      } else{
        ""
      }
      params$fact_graph_sel <- if (!is.null(input$boxplot_factGraphSel)){
        input$boxplot_factGraphSel
      } else if (params$fact_graph != ""){
        sel <- unique(PAMPA:::selectModalites.f(tableMetrique = input$boxplot_metric_table,
          facts = input$boxplot_factGraph, selections = append(list(NA), NA),
          metrique = input$boxplot_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$boxplot_factGraph])
        sel <- sort(as.character(sel))
      } else{
        NA
      }
      params$list_fact <- input$boxplot_listFact
      params$list_fact_sel <- lapply(1:length_listFact(), function(i){
        id <- paste("boxplot_listFactSel_", i, sep = "")
        if (!is.null(input[[id]])) input[[id]] else NA
      })

      if (params$aggregation == "espece" & params$fact_graph != ""){
        output$graph_boxplot <- shiny::renderUI({
          lapply(1:isolate(length_factGraphSel()), function(iFact){
            id <- paste("boxplot_", iFact, sep = "")
            shiny::plotOutput(outputId = id)

            output[[id]] <- shiny::renderPlot({
              PAMPA::boxplot_pampa.f(
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
        output$graph_boxplot <- shiny::renderUI({
          id <- "boxplot"
          shiny::plotOutput(outputId = id)

          output[[id]] <- shiny::renderPlot({
            PAMPA::boxplot_pampa.f(
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

    shiny::observeEvent(input$boxplot_save_graphics, {
      shiny::showModal(shiny::modalDialog(
        shiny::checkboxGroupInput(ns("boxplot_export_format"), "Choose the format for the file:",
          choices = c("pdf", "png", "wmf")),
        shiny::h5("The files will be saved at:"),
        shiny::code(get("filePathes", envir = .GlobalEnv)["results"],
          style = "color: #000000; background-color: #ffffff"),
        title = "Save graphics",
        footer = shiny::tagList(
          shiny::actionButton(ns("boxplot_save"), "Save"),
          shiny::modalButton("Cancel")
        )
      ))
    })

    shiny::observeEvent(input$boxplot_save, {
      if ("pdf" %in% input$boxplot_export_format){
        R.utils::setOption("P.graphPDF", TRUE)
        PAMPA::boxplot_pampa.f(
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
      if ("png" %in% input$boxplot_export_format){
        R.utils::setOption("P.graphPNG", TRUE)
        PAMPA::boxplot_pampa.f(
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
      if ("wmf" %in% input$boxplot_export_format){
        R.utils::setOption("P.graphWMF", TRUE)
        PAMPA::boxplot_pampa.f(
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
# mod_boxplot_ui("boxplot_1")

## To be copied in the server
# mod_boxplot_server("boxplot_1")
