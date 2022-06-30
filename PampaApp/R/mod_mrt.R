#' mrt UI Function
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
mod_mrt_ui <- function(id){
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shinyFeedback::useShinyFeedback(),
      shiny::h3("Multivariate Regression Trees", align = "center"),
      shiny::br(),
      shinyWidgets::radioGroupButtons(ns("mrt_aggregation"), "Choose an aggregation",
        choices = c(
          "species" = "espece",
          "species groups" = "unitobs"
        ),
        justified = TRUE
      ),
      shiny::radioButtons(ns("mrt_metric_table"), "Choose a metric table",
        choices = c(
          "... / station / size class" = "unitSpSz",
          "... / station" = "unitSp")
      ),
      shiny::selectInput(ns("mrt_metric"), "Select a metric",
        choices = c()
      ),
      shiny::radioButtons(ns("mrt_type_fact"), "Subset species per...",
        choices = c(
          "... station characteristic" = "unitobs",
          "... species characteristic" = "refesp")
      ),
      shiny::selectInput(ns("mrt_factGraph"), "Select the factor for the graphic separation",
        choices = c()
      ),
      shiny::selectInput(ns("mrt_factGraphSel"), "Select categories of the factor for the graphic separation (all by default)",
        choices = c(), multiple = TRUE
      ),
      shiny::selectInput(ns("mrt_listFact"), "Select explanatory factors(s) for plotting (order doesn't matter)",
        choices = c(), multiple = TRUE
      ),
      shiny::uiOutput(ns("mrt_listFactSel")),
      shiny::div(
        shiny::actionButton(ns("mrt_launch_button"), "Launch Graphics"),
        align = "center"
      ),
      shiny::div(
        mod_plot_options_ui("plot_options_6"),
        align = "center"
      )
    ),
    shiny::mainPanel(width = 9,
      shiny::h3("Graphics", align = "center"),
      shiny::actionButton(ns("mrt_save_graphics"), "Save graphics"),
      shinycssloaders::withSpinner(
        shiny::uiOutput(ns("stats_mrt")),
        type = 3,
        color = "#CCCCCC",
        color.background = "#FFFFFF"
      )
    )
  )
}

#' mrt Server Functions
#'
#' @noRd
#'
#' @import shiny
#' @importFrom PAMPA mrt.f
#' @importFrom shinyjs reset
#' @importFrom shinyFeedback hideFeedback showFeedbackDanger
mod_mrt_server <- function(id, load_file){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    length_listFact <- shiny::reactive({
      length(input$mrt_listFact)
    })

    length_factGraphSel <- shiny::reactive({
      if (!is.null(input$mrt_factGraphSel) && input$mrt_factGraphSel != "NA"){
        length(input$mrt_factGraphSel)
      } else{
        sel <- unique(PAMPA:::selectModalites.f(tableMetrique = input$mrt_metric_table,
          facts = input$mrt_factGraph, selections = append(list(NA), NA),
          metrique = input$mrt_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$mrt_factGraph])
        sel <- sort(as.character(sel))
        length(sel)
      }
    })

    params <- shiny::reactiveValues(
      aggregation = NULL,
      metric_table = NULL,
      metric = NULL,
      type_graph = NULL,
      fact_graph = NULL,
      fact_graph_sel = NULL,
      list_fact = NULL,
      list_fact_sel = NULL
    )

    next_step <- shiny::reactive(
      switch(input$mrt_aggregation,
        "espece" = "MRT.esp",
        "unitobs" = "MRT.unitobs")
    )

    shiny::observeEvent(load_file(), {
      shinyjs::reset(id = "mrt_factGraphSel")
      output$stats_mrt <- NULL
    })

    shiny::observeEvent({
        input$mrt_aggregation
        load_file()
      }, {
        if (input$mrt_aggregation == "espece"){
          if (load_file() != 0 && (PAMPA:::is.benthos.f() | nrow(get("unitSpSz", envir = .GlobalEnv)) == 0)){
            shiny::updateRadioButtons(inputId = "mrt_metric_table",
              choices = c(
                "... / station / species" = "unitSp")
            )
          } else{
            shiny::updateRadioButtons(inputId = "mrt_metric_table",
              choices = c(
                "... / station / species / size class" = "unitSpSz",
                "... / station / species" = "unitSp"),
              selected = "unitSp"
            )
          }
          shiny::updateRadioButtons(inputId = "mrt_type_fact",
            label = "Generate one plot per...",
            choices = c(
              "... station characteristic" = "unitobs",
              "... species characteristic" = "refesp"),
            selected = "refesp"
          )
        } else{
          if (load_file() != 0 && (PAMPA:::is.benthos.f() | nrow(get("unitSpSz", envir = .GlobalEnv)) == 0)){
            shiny::updateRadioButtons(inputId = "mrt_metric_table",
              choices = c(
               "... / station" = "unitSp",
               "... of biodiversity (/station)" = "unit"),
              selected = "unitSp"
            )
          } else{
            shiny::updateRadioButtons(inputId = "mrt_metric_table",
              choices = c(
                "... / station / size class" = "unitSpSz",
                "... / station" = "unitSp",
                "... / of biodiversity (/station)" = "unit"),
              selected = "unitSp"
            )
          }
          shiny::updateRadioButtons(inputId = "mrt_type_fact",
            label = "Subset species per...",
            choices = c(
              "... species characteristic" = "refesp")
          )
        }
    })

    output$mrt_listFactSel <- shiny::renderUI({
      nb_input <- length_listFact()
      ids <- sapply(1:nb_input, function(i){
        paste("mrt_listFactSel_", i, sep = "")
      })
      labels_input <- sapply(1:nb_input, function(i){
        paste("Choose categories for ", input$mrt_listFact[i], " (all by default)", sep = "")
      })
      output_listFactSel <- shiny::tagList()
      if (nb_input > 0){
        for (i in 1:nb_input){
          choices <- unique(PAMPA:::selectModalites.f(tableMetrique = input$mrt_metric_table,
            facts = input$mrt_listFact[i], selections = append(list(NA), NA),
            metrique = input$mrt_metric, nextStep = next_step(), dataEnv = .GlobalEnv,
            level = 1)[, input$mrt_listFact[i]])
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
        input$mrt_metric_table
        load_file()
      }, {
        if (load_file() != 0){
          choices <- PAMPA:::MetricsField.aliases(input$mrt_metric_table, next_step(), .GlobalEnv)
          shiny::updateSelectInput(inputId = "mrt_metric", choices = c("", choices))

          choices <- PAMPA:::refTablesFields.aliases(nomTable = input$mrt_metric_table, dataEnv = .GlobalEnv)
          shiny::updateSelectInput(inputId = "mrt_listFact", choices = c("", choices))
        }
    })

    shiny::observeEvent(
      {
        input$mrt_type_fact
        load_file()
      }, {
        if (load_file() != 0){
          choices <- switch(input$mrt_type_fact,
            "unitobs" = PAMPA:::UnitobsFields.aliases(dataEnv = .GlobalEnv, ordered = TRUE,
              tableMetrique = input$mrt_metric_table),
            "refesp" = PAMPA:::spRefFields.aliases(site = getOption("P.MPA"), dataEnv = .GlobalEnv,
              ordered = TRUE, tableMetrique = input$mrt_metric_table)
          )
          shiny::updateSelectInput(inputId = "mrt_factGraph", choices = c("", NA, choices))
        }
    })

    shiny::observeEvent(input$mrt_factGraph, {
      if (input$mrt_factGraph != "" & input$mrt_factGraph != "NA"){
        choices <- unique(PAMPA:::selectModalites.f(tableMetrique = input$mrt_metric_table,
          facts = input$mrt_factGraph, selections = append(list(NA), NA),
          metrique = input$mrt_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$mrt_factGraph])
        choices <- sort(as.character(choices))
        shiny::updateSelectInput(inputId = "mrt_factGraphSel", choices = c("", choices))
      } else{
        shiny::updateSelectInput(inputId = "mrt_factGraphSel", choices = c())
      }
    })

    shiny::observeEvent(input$mrt_metric, {
      shinyFeedback::hideFeedback("mrt_metric")
    })

    shiny::observeEvent(input$mrt_listFact, {
      shinyFeedback::hideFeedback("mrt_listFact")
    })

    shiny::observeEvent(input$mrt_launch_button, {
      error <- FALSE
      if (input$mrt_metric == ""){
        shinyFeedback::showFeedbackDanger("mrt_metric", text = "A metric is required.")
        error <- TRUE
      }
      if (is.null(input$mrt_listFact)){
        shinyFeedback::showFeedbackDanger("mrt_listFact", text = "Explanatory factor(s) are required.")
        error <- TRUE
      }
      if (error){
        shiny::req(NULL)
      }

      shiny::showModal(shiny::modalDialog("Creation of graphics...", footer = NULL))

      params$aggregation <- input$mrt_aggregation
      params$metric_table <- input$mrt_metric_table
      params$metric <- input$mrt_metric
      params$type_fact <- input$mrt_type_fact
      params$fact_graph <- if (!is.null(input$mrt_factGraph) && input$mrt_factGraph != "NA"){
        input$mrt_factGraph
      } else{
        ""
      }
      params$fact_graph_sel <- if (!is.null(input$mrt_factGraphSel)){
        input$mrt_factGraphSel
      } else if (params$fact_graph != ""){
        sel <- unique(PAMPA:::selectModalites.f(tableMetrique = input$mrt_metric_table,
          facts = input$mrt_factGraph, selections = append(list(NA), NA),
          metrique = input$mrt_metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$mrt_factGraph])
        sel <- sort(as.character(sel))
        sel
      } else{
        NA
      }
      params$list_fact <- input$mrt_listFact
      params$list_fact_sel <- lapply(1:length_listFact(), function(i){
        id <- paste("mrt_listFactSel_", i, sep = "")
        if (!is.null(input[[id]])) input[[id]] else NA
      })

      if (params$aggregation == "espece" & params$fact_graph != ""){
        output$stats_mrt <- shiny::renderUI({
          lapply(1:isolate(length_factGraphSel()), function(iFact){
            id <- paste("mrt_", iFact, sep = "")
            shiny::plotOutput(outputId = id)

            output[[id]] <- shiny::renderPlot({
              PAMPA::mrt.f(
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
        output$stats_mrt <- shiny::renderUI({
          id <- "mrt"
          shiny::plotOutput(outputId = id)

          output[[id]] <- shiny::renderPlot({
            PAMPA::mrt.f(
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

    shiny::observeEvent(input$mrt_save_graphics, {
      shiny::showModal(shiny::modalDialog(
        shiny::checkboxGroupInput(ns("mrt_export_format"), "Choose the format for the file:",
          choices = c("pdf", "png", "wmf")),
        shiny::h5(paste("The files will be saved at ", get("filePathes", envir = .GlobalEnv)["results"])),
        title = "Save graphics",
        footer = shiny::tagList(
          shiny::actionButton(ns("mrt_save"), "Save"),
          shiny::modalButton("Cancel")
        )
      ))
    })

    shiny::observeEvent(input$mrt_save, {
      if ("pdf" %in% input$mrt_export_format){
        setOption("P.graphPDF", TRUE)
        PAMPA::mrt.f(
          agregation = params$aggregation,
          metrique = params$metric,
          factGraph = params$fact_graph,
          factGraphSel = params$fact_graph_sel,
          listFact = params$list_fact,
          listFactSel = params$list_fact_sel,
          tableMetrique = params$metric_table,
          new_window = TRUE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
        )
        setOption("P.graphPDF", FALSE)
      }
      if ("png" %in% input$mrt_export_format){
        setOption("P.graphPNG", TRUE)
        PAMPA::mrt.f(
          agregation = params$aggregation,
          metrique = params$metric,
          factGraph = params$fact_graph,
          factGraphSel = params$fact_graph_sel,
          listFact = params$list_fact,
          listFactSel = params$list_fact_sel,
          tableMetrique = params$metric_table,
          new_window = TRUE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
        )
        setOption("P.graphPNG", FALSE)
      }
      if ("wmf" %in% input$mrt_export_format){
        setOption("P.graphWMF", TRUE)
        PAMPA::mrt.f(
          agregation = params$aggregation,
          metrique = params$metric,
          factGraph = params$fact_graph,
          factGraphSel = params$fact_graph_sel,
          listFact = params$list_fact,
          listFactSel = params$list_fact_sel,
          tableMetrique = params$metric_table,
          new_window = TRUE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
        )
        setOption("P.graphWMF", FALSE)
      }
      shiny::removeModal()
    })
  })
}

## To be copied in the UI
# mod_mrt_ui("mrt_1")

## To be copied in the server
# mod_mrt_server("mrt_1")
