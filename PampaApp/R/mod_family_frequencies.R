#' family_frequencies UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom shinycssloaders withSpinner
mod_family_frequencies_ui <- function(id){
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shinyFeedback::useShinyFeedback(),
      shiny::h3("Family frequencies", align = "center"),
      shiny::h5("One value per family for each combination of factor level", align = "center"),
      shiny::br(),
      shiny::selectInput(ns("family_factGraph"), "Select a 1st explanatory factor for plotting",
        choices = c()
      ),
      shiny::selectInput(ns("family_factGraphSel"), "Select categories for the explanatory factor (all by default)",
        choices = c(), multiple = TRUE
      ),
      shiny::selectInput(ns("family_fact"), "Select a 2nd explanatory factor for plotting",
        choices = c()
      ),
      shiny::selectInput(ns("family_factSel"), "Select categories for the explanatory factor (all by default)",
        choices = c(), multiple = TRUE
      ),
      shiny::selectInput(ns("family_families"), "Select families to plot (all by default)",
        choices = c(), multiple = TRUE
      ),
      shiny::div(
        shiny::actionButton(ns("family_launch_button"), "Launch Graphics"),
        align = "center"
      ),
      shiny::div(
        mod_family_frequencies_options_ui("family_frequencies_options_1"),
        mod_plot_options_ui("plot_options_5"),
        align = "center"
      )
    ),
    shiny::mainPanel(width = 9,
      shiny::h3("Graphics", align = "center"),
      shiny::actionButton(ns("family_save_graphics"), "Save graphics"),
      shiny::uiOutput(ns("graph_family"))
    )
  )
}

#' family_frequencies Server Functions
#'
#' @noRd
#'
#' @import shiny
#' @importFrom PAMPA freq_occurrence_familles.f
#' @importFrom shinyjs reset
#' @importFrom shinyFeedback hideFeedback showFeedbackDanger
#' @importFrom R.utils setOption
mod_family_frequencies_server <- function(id, load_file){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    length_factGraphSel <- shiny::reactive(
      if (!is.null(input$family_factGraphSel) && input$family_factGraphSel != "NA"){
        length(input$family_factGraphSel)
      } else {
        sel <- unique(PAMPA:::selectModalites.f(tableMetrique = metric_table,
          facts = input$family_factGraph, selections = append(list(NA), NA), metrique = metric,
          nextStep = next_step, dataEnv = .GlobalEnv, level = 0)[, input$family_factGraph])
        sel <- as.character(sel)
        length(sel)
      }
    )

   params <- shiny::reactiveValues(
      families = NULL,
      fact_graph = NULL,
      fact_graph_sel = NULL,
      fact = NULL,
      fact_sel = NULL
    )

    aggregation <- "espece"
    metric_table <- "TablePresAbs"
    metric <- "pres.abs"
    next_step = "freq_occurrence"

    shiny::observeEvent(load_file(), {
      shinyjs::reset(id = "family_factGraphSel")
      output$family <- NULL
    })

    shiny::observeEvent(load_file(), {
      if (load_file() != 0){
        choices <- PAMPA:::UnitobsFields.aliases(dataEnv = .GlobalEnv, ordered = TRUE,
          tableMetrique = metric_table)
        shiny::updateSelectInput(inputId = "family_factGraph", choices = c("", NA, choices))
      }
    })

    shiny::observeEvent(input$family_factGraph, {
      if (input$family_factGraph != "" & input$family_factGraph != "NA"){
        choices <- unique(PAMPA:::selectModalites.f(tableMetrique = metric_table,
          facts = input$family_factGraph, selections = append(list(NA), NA), metrique = metric,
          nextStep = next_step, dataEnv = .GlobalEnv, level = 0)[, input$family_factGraph])
        choices <- sort(as.character(choices))
        shiny::updateSelectInput(inputId = "family_factGraphSel", choices = c("", choices))
      } else{
        shiny::updateSelectInput(inputId = "family_factGraphSel", choices = c())
      }
    })

    shiny::observeEvent(load_file(), {
      if (load_file() != 0){
        choices <- PAMPA:::refTablesFields.aliases(nomTable = metric_table, dataEnv = .GlobalEnv)
        shiny::updateSelectInput(inputId = "family_fact", choices = c("", NA, choices))
      }
    })

    shiny::observeEvent(
      {
        input$family_fact
        load_file()
      }, {
        if (input$family_fact != "" & input$family_fact != "NA"){
          choices <- unique(PAMPA:::selectModalites.f(tableMetrique = metric_table,
            facts = input$family_fact, selections = append(list(NA), NA), metrique = metric,
            nextStep = next_step, dataEnv = .GlobalEnv, level = 1)[, input$family_fact])
          choices <- sort(as.character(choices))
          shiny::updateSelectInput(inputId = "family_factSel", choices = c("", choices))
        }
    })

    shiny::observeEvent(load_file(), {
      if (load_file() != 0){
        choices <- unique(PAMPA:::selectModalites.f(tableMetrique = metric_table,
          facts = "family", selections = append(list(NA), NA), metrique = metric,
          nextStep = next_step, dataEnv = .GlobalEnv, level = 1)[, "family"])
        choices <- sort(as.character(choices))
        shiny::updateSelectInput(inputId = "family_families", choices = c("", choices))
      }
    })

    shiny::observeEvent(input$family_factGraph, {
      shinyFeedback::hideFeedback("family_factGraph")
    })

    shiny::observeEvent(input$family_launch_button, {
      error <- FALSE
      if (input$family_factGraph == ""){
        shinyFeedback::showFeedbackDanger("family_factGraph", "An explanatory factor is required")
        error <- TRUE
      }
      if (error){
        shiny::req(NULL)
      }

      shiny::showModal(shiny::modalDialog("Creation of graphics...", footer = NULL))

      params$fact_graph <- if (!is.null(input$family_factGraph) && input$family_factGraph != "NA"){
        input$family_factGraph
      } else{
        ""
      }
      params$fact_graph_sel <- if (!is.null(input$family_factGraphSel)){
        input$family_factGraphSel
      } else if (params$fact_graph != ""){
        sel <- unique(PAMPA:::selectModalites.f(tableMetrique = metric_table,
          facts = input$family_factGraph, selections = append(list(NA), NA), metrique = metric,
          nextStep = next_step, dataEnv = .GlobalEnv, level = 0)[, input$family_factGraph])
        sel <- as.character(sel)
      } else{
        NA
      }
      params$fact <- if (!is.null(input$family_fact) && input$family_fact != "NA" && input$family_fact != ""){
        input$family_fact
      } else{
        NA
      }
      params$fact_sel <- if (!is.null(input$family_factSel)){
        input$family_factSel
      } else{
        NA
      }
      params$families <- if (!is.null(input$family_families)){
        input$family_families
      } else{
        NA
      }

      if (params$fact_graph != ""){
        output$graph_family <- shiny::renderUI({
          lapply(1:isolate(length_factGraphSel()), function(iFact){
            id <- paste("family_", iFact, sep = "")
            shiny::plotOutput(outputId = id)

            output[[id]] <- shiny::renderPlot({
              PAMPA::freq_occurrence_familles.f(
                factGraph = params$fact_graph,
                factGraphSel = params$fact_graph_sel[iFact],
                fact = params$fact,
                factSel = params$fact_sel,
                families = params$families,
                new_window = FALSE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
              )
            })
          })
        })
      } else{
        output$graph_family <- shiny::renderUI({
          id <- "family"
          shiny::plotOutput(outputId = id)

          output[[id]] <- shiny::renderPlot({
            PAMPA::freq_occurrence_familles.f(
              factGraph = params$fact_graph,
              factGraphSel = params$fact_graph_sel,
              fact = params$fact,
              factSel = params$fact_sel,
              families = params$families,
              new_window = FALSE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
            )
          })
        })
      }
      shiny::removeModal()
    })

    shiny::observeEvent(input$family_save_graphics, {
      shiny::showModal(shiny::modalDialog(
        shiny::checkboxGroupInput(ns("family_export_format"), "Choose the format for the file:",
          choices = c("pdf", "png", "wmf")),
        shiny::h5("The files will be saved at:"),
        shiny::code(get("filePathes", envir = .GlobalEnv)["results"],
          style = "color: #000000; background-color: #ffffff"),
        title = "Save graphics",
        footer = shiny::tagList(
          shiny::actionButton(ns("family_save"), "Save"),
          shiny::modalButton("Cancel")
        )
      ))
    })

    shiny::observeEvent(input$family_save, {
      if ("pdf" %in% input$family_export_format){
        R.utils::setOption("P.graphPDF", TRUE)
        PAMPA::freq_occurrence_familles.f(
          factGraph = params$fact_graph,
          factGraphSel = params$fact_graph_sel[iFact],
          fact = params$fact,
          factSel = params$fact_sel,
          families = params$families,
          new_window = FALSE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
        )
        R.utils::setOption("P.graphPDF", FALSE)
      }
      if ("png" %in% input$family_export_format){
        R.utils::setOption("P.graphPNG", TRUE)
        PAMPA::freq_occurrence_familles.f(
          factGraph = params$fact_graph,
          factGraphSel = params$fact_graph_sel[iFact],
          fact = params$fact,
          factSel = params$fact_sel,
          families = params$families,
          new_window = FALSE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
        )
        R.utils::setOption("P.graphPNG", FALSE)
      }
      if ("wmf" %in% input$family_export_format){
        R.utils::setOption("P.graphWMF", TRUE)
        PAMPA::freq_occurrence_familles.f(
          factGraph = params$fact_graph,
          factGraphSel = params$fact_graph_sel[iFact],
          fact = params$fact,
          factSel = params$fact_sel,
          families = params$families,
          new_window = FALSE, dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
        )
        R.utils::setOption("P.graphWMF", FALSE)
      }
      shiny::removeModal()
    })
  })
}

## To be copied in the UI
# mod_family_frequencies_ui("family_frequencies_1")

## To be copied in the server
# mod_family_frequencies_server("family_frequencies_1")
