#' occurrence_frequencies UI Function
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
mod_occurrence_frequencies_ui <- function(id){
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shinyFeedback::useShinyFeedback(),
      shiny::h3("Occurrence frequencies", align = "center"),
      shiny::h5("One value per combination of factor levels", align = "center"),
      shiny::br(),
      shinyWidgets::radioGroupButtons(ns("occurrence_aggregation"), "Choose an aggregation",
        choices = c(
          "species" = "espece",
          "species groups" = "unitobs"
        ),
        justified = TRUE
      ),
      shiny::br(),
      shiny::strong("Plotted metric:"),
      shiny::p("frequency per species and explanatory factor(s)"),
      shiny::br(),
      shiny::radioButtons(ns("occurrence_type_fact"), "Subset species per...",
        choices = c(
          "... station characteristic" = "unitobs",
          "... species characteristic" = "refesp")
      ),
      shiny::selectInput(ns("occurrence_factGraph"), "Select the factor for the graphic separation",
        choices = c()
      ),
      shiny::selectInput(ns("occurrence_factGraphSel"), "Select categories of the factor for the  graphic separation (all by default)",
        choices = c(), multiple = TRUE
      ),
      shiny::selectInput(ns("occurrence_listFact"), "Select explanatory factor(s) for plotting",
        choices = c(), multiple = TRUE
      ),
      shiny::uiOutput(ns("occurrence_listFactSel")),
      shiny::div(
        shiny::actionButton(ns("occurrence_launch_button"), "Launch Graphics"),
        align = "center"
      ),
      shiny::div(
        mod_occurrence_frequencies_options_ui("occurrence_frequencies_options_1"),
        mod_plot_options_ui("plot_options_2"),
        align = "center"
      )
    ),
    shiny::mainPanel(width = 9,
      shiny::h3("Graphics", align = "center"),
      shiny::actionButton(ns("occurrence_save_graphics"), "Save graphics"),
      shinycssloaders::withSpinner(
        shiny::uiOutput(ns("graph_occurrence")),
        type = 3,
        color = "#CCCCCC",
        color.background = "#FFFFFF"
      )
    )
  )
}

#' occurrence_frequencies Server Functions
#'
#' @noRd
#'
#' @import shiny
#' @importFrom PAMPA freq_occurrence.f
#' @importFrom shinyjs reset
#' @importFrom shinyFeedback hideFeedback showFeedbackDanger
#' @importFrom R.utils setOption
mod_occurrence_frequencies_server <- function(id, load_file){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    length_listFact <- shiny::reactive({
      length(input$occurrence_listFact)
    })

    length_factGraphSel <- shiny::reactive({
      if (!is.null(input$occurrence_factGraphSel) && input$occurrence_factGraphSel != "NA"){
        length(input$occurrence_factGraphSel)
      } else{
        sel <- unique(PAMPA:::selectModalites.f(tableMetrique = metric_table,
          facts = input$occurrence_factGraph, selections = append(list(NA), NA),
          metrique = metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$occurrence_factGraph])
        sel <- sort(as.character(sel))
        length(sel)
      }
    })

    metric_table <- "TablePresAbs"
    metric <- "pres.abs"

    params <- shiny::reactiveValues(
      aggregation = NULL,
      metric_table = "TablePresAbs",
      metric = "pres.abs",
      type_fact = NULL,
      fact_graph = NULL,
      fact_graph_sel = NULL,
      list_fact = NULL,
      list_fact_sel = NULL
    )

    next_step <- shiny::reactive(
      switch(input$occurrence_aggregation,
        "espece" = "freq_occurrence",
        "unitobs" = "freq_occurrence.unitobs")
    )

    observeEvent(load_file(), {
      shinyjs::reset(id = "occurrence_factGraphSel")
      output$graph_occurrence <- NULL
    })

    shiny::observeEvent({
        input$occurrence_aggregation
        load_file()
      }, {
        if (input$occurrence_aggregation == "espece"){
          shiny::updateRadioButtons(inputId = "occurrence_type_fact",
            label = "Generate one plot per...",
            choices = c(
              "... station characteristic" = "unitobs",
              "... species characteristic" = "refesp"),
            selected = "refesp"
          )
        } else{
          shiny::updateRadioButtons(inputId = "occurrence_type_fact",
            label = "Subset species per...",
            choices = c(
              "... species characteristic" = "refesp")
          )
        }
    })

    output$occurrence_listFactSel <- shiny::renderUI({
      nb_input <- length_listFact()
      ids <- sapply(1:nb_input, function(i){
        paste("occurrence_listFactSel_", i, sep = "")
      })
      labels_input <- sapply(1:nb_input, function(i){
        paste("Choose categories for ", input$occurrence_listFact[i], " (all by default)", sep = "")
      })
      output_listFactSel <- shiny::tagList()
      if (nb_input > 0){
        for (i in 1:nb_input){
          choices <- unique(PAMPA:::selectModalites.f(tableMetrique = metric_table,
            facts = input$occurrence_listFact[i], selections = append(list(NA), NA),
            metrique = metric, nextStep = next_step(), dataEnv = .GlobalEnv,
            level = 1)[, input$occurrence_listFact[i]])
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
        choices <- PAMPA:::refTablesFields.aliases(nomTable = metric_table, dataEnv = .GlobalEnv)
        shiny::updateSelectInput(inputId = "occurrence_listFact", choices = c("", choices))
      }
    })

    shiny::observeEvent(
      {
        input$occurrence_type_fact
        input$occurrence_aggregation
        load_file()
      }, {
        if (load_file() != 0){
          choices <- switch(input$occurrence_type_fact,
            "unitobs" = PAMPA:::UnitobsFields.aliases(dataEnv = .GlobalEnv, ordered = TRUE,
              tableMetrique = metric_table),
            "refesp" = if(input$occurrence_aggregation == "espece"){
                c(species.code = "species.code", scient.name = "scient.name")
              } else{
                PAMPA:::spRefFields.aliases(site = getOption("P.MPA"), dataEnv = .GlobalEnv,
                  ordered = TRUE, tableMetrique = metric_table)
              }
          )
          shiny::updateSelectInput(inputId = "occurrence_factGraph", choices = c("", NA, choices))
        }
    })

    shiny::observeEvent(input$occurrence_factGraph, {
      if(input$occurrence_factGraph != "" & input$occurrence_factGraph != "NA"){
        choices <- unique(PAMPA:::selectModalites.f(tableMetrique = metric_table,
          facts = input$occurrence_factGraph, selections = append(list(NA), NA),
          metrique = metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$occurrence_factGraph])
        choices <- sort(as.character(choices))
        shiny::updateSelectInput(inputId = "occurrence_factGraphSel", choices = c("", choices))
      } else{
        shiny::updateSelectInput(inputId = "occurrence_factGraphSel", choices = c())
      }
    })

    shiny::observeEvent(input$occurrence_listFact, {
      shinyFeedback::hideFeedback("occurrence_listFact")
      if (length_listFact() > 2){
        shinyFeedback::showFeedbackDanger("occurrence_listFact", text = "Cannot have more than 2 explanatory factors.")
      }
    })

    shiny::observeEvent(input$occurrence_launch_button, {
      error <- FALSE
      if (is.null(input$occurrence_listFact)){
        shinyFeedback::showFeedbackDanger("occurrence_listFact", text = "Explanatory factor(s) are required.")
        error <- TRUE
      }
      if (length_listFact() > 2){
        error <- TRUE
      }
      if (error){
        shiny::req(NULL)
      }

      shiny::showModal(shiny::modalDialog("Creation of graphics...", footer = NULL))

      params$aggregation <- input$occurrence_aggregation
      params$type_fact <- input$occurrence_type_fact
      params$fact_graph <- if (!is.null(input$occurrence_factGraph) && input$occurrence_factGraph != "NA"){
        input$occurrence_factGraph
      } else{
        ""
      }
      params$fact_graph_sel <- if (!is.null(input$occurrence_factGraphSel)){
        input$occurrence_factGraphSel
      } else if (params$fact_graph != ""){
        sel <- unique(PAMPA:::selectModalites.f(tableMetrique = metric_table,
          facts = input$occurrence_factGraph, selections = append(list(NA), NA),
          metrique = metric, nextStep = next_step(),
          dataEnv = .GlobalEnv, level = 0)[, input$occurrence_factGraph])
        sel <- sort(as.character(sel))
      } else{
        NA
      }
      params$list_fact <- input$occurrence_listFact
      params$list_fact_sel <- lapply(1:length_listFact(), function(i){
        id <- paste("occurrence_listFactSel_", i, sep = "")
        if (!is.null(input[[id]])) input[[id]] else NA
      })

      if (params$aggregation == "espece" & params$fact_graph != ""){
        output$graph_occurrence <- shiny::renderUI({
          lapply(1:isolate(length_factGraphSel()), function(iFact){
            id <- paste("occurrence_", iFact, sep = "")
            shiny::plotOutput(outputId = id)

            output[[id]] <- shiny::renderPlot({
              PAMPA::freq_occurrence.f(
                agregation = params$aggregation,
                factGraph = params$fact_graph,
                factGraphSel = params$fact_graph_sel[iFact],
                listFact = params$list_fact,
                listFactSel = params$list_fact_sel,
                new_window = FALSE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
              )
            })
          })
        })
      } else{
        output$graph_occurrence <- shiny::renderUI({
          id <- "occurrence"
          shiny::plotOutput(outputId = id)

          output[[id]] <- shiny::renderPlot({
            PAMPA::freq_occurrence.f(
              agregation = params$aggregation,
              factGraph = params$fact_graph,
              factGraphSel = params$fact_graph_sel,
              listFact = params$list_fact,
              listFactSel = params$list_fact_sel,
              new_window = FALSE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
            )
          })
        })
      }
      shiny::removeModal()
    })

    shiny::observeEvent(input$occurrence_save_graphics, {
      shiny::showModal(shiny::modalDialog(
        shiny::checkboxGroupInput(ns("occurrence_export_format"), "Choose the format for the file:",
          choices = c("pdf", "png", "wmf")),
        shiny::h5("The files will be saved at "),
        shiny::code(get("filePathes", envir = .GlobalEnv)["results"],
          style = "color: #000000; background-color: #ffffff"),
        title = "Save graphics",
        footer = shiny::tagList(
          shiny::actionButton(ns("occurrence_save"), "Save"),
          shiny::modalButton("Cancel")
        )
      ))
    })

    shiny::observeEvent(input$occurrence_save, {
      if ("pdf" %in% input$occurrence_export_format){
        R.utils::setOption("P.graphPDF", TRUE)
        PAMPA::freq_occurrence.f(
          agregation = params$aggregation,
          factGraph = params$fact_graph,
          factGraphSel = params$fact_graph_sel[iFact],
          listFact = params$list_fact,
          listFactSel = params$list_fact_sel,
          new_window = TRUE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
        )
        R.utils::setOption("P.graphPDF", FALSE)
      }
      if ("png" %in% input$occurrence_export_format){
        R.utils::setOption("P.graphPNG", TRUE)
        PAMPA::freq_occurrence.f(
          agregation = params$aggregation,
          factGraph = params$fact_graph,
          factGraphSel = params$fact_graph_sel[iFact],
          listFact = params$list_fact,
          listFactSel = params$list_fact_sel,
          new_window = TRUE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
        )
        R.utils::setOption("P.graphPNG", FALSE)
      }
      if ("wmf" %in% input$occurrence_export_format){
        R.utils::setOption("P.graphWMF", TRUE)
        PAMPA::freq_occurrence.f(
          agregation = params$aggregation,
          factGraph = params$fact_graph,
          factGraphSel = params$fact_graph_sel[iFact],
          listFact = params$list_fact,
          listFactSel = params$list_fact_sel,
          new_window = TRUE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
        )
        R.utils::setOption("P.graphWMF", FALSE)
      }
      shiny::removeModal()
    })
  })
}

## To be copied in the UI
# mod_occurrence_frequencies_ui("occurrence_frequencies_1")

## To be copied in the server
# mod_occurrence_frequencies_server("occurrence_frequencies_1")
