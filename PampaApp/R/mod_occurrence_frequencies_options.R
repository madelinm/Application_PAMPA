#' occurrence_frequencies_options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_occurrence_frequencies_options_ui <- function(id){
  ns <- NS(id)
  shiny::actionButton(ns("occurrence_options"), "Occurrence frequencies options")
}

#' occurrence_frequencies_options Server Functions
#'
#' @noRd
mod_occurrence_frequencies_options_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    occurrence_optn <- shiny::reactiveValues(
      nb_obs = getOption("P.NbObs"),
      nb_obs_col = getOption("P.NbObsCol"),
      warnings = getOption("P.warnings")
    )

    shiny::observeEvent(input$occurrence_options, {
      shiny::showModal(shiny::modalDialog(
        shiny::checkboxInput(ns("occurrence_options_nb_obs"), "Display the number of stations per bars?",
          value = getOption("P.NbObs")),
        shiny::div(
          colourpicker::colourInput(ns("occurrence_options_nb_obs_col"), "Colours for station numbers:",
            value = getOption("P.NbObsCol"), showColour = "background"),
          style = "margin-left:25px;"
        ),
        shiny::checkboxInput(ns("occurrence_options_warnings"), "Display warnings for low numbers?",
          value = getOption("P.warnings")),
        title = "Options for occurrence barplots",
        footer = shiny::tagList(
          shiny::actionButton(ns("occurrence_options_ok"), "Ok"),
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("occurrence_options_reset"), "Reset")
        ),
        size = "l"
      ))
    })

    shiny::observeEvent(input$occurrence_options_ok,{
      setOption("P.NbObs", input$occurrence_options_nb_obs)
      setOption("P.NbObsCol", input$occurrence_options_nb_obs_col)
      setOption("P.warnings", input$occurrence_options_warnings)

      shiny::removeModal()
    })

    shiny::observeEvent(input$occurrence_options_reset, {
      setOption("P.NbObs", occurrence_optn$nb_obs)
      setOption("P.NbObsCol", occurrence_optn$nb_obs_col)
      setOption("P.warnings", occurrence_optn$warnings)

      shiny::updateCheckboxInput(inputId = "occurrence_options_nb_obs", value = occurrence_optn$nb_obs)
      colourpicker::updateColourInput(session = session, inputId = "occurrence_options_nb_obs_col", value = occurrence_optn$nb_obs_col)
      shiny::updateCheckboxInput(inputId = "occurrence_options_warnings", value = occurrence_optn$warnings)
    })

  })
}

## To be copied in the UI
# mod_occurrence_frequencies_options_ui("occurrence_frequencies_options_1")

## To be copied in the server
# mod_occurrence_frequencies_options_server("occurrence_frequencies_options_1")
