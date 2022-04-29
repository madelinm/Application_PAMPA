#' family_frequencies_options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_family_frequencies_options_ui <- function(id){
  ns <- NS(id)
  shiny::actionButton(ns("family_options"), "Family frequencies options")
}

#' family_frequencies_options Server Functions
#'
#' @noRd
mod_family_frequencies_options_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    family_optn <- shiny::reactiveValues(
      nb_obs = getOption("P.NbObs"),
      nb_obs_col = getOption("P.NbObsCol"),
      warnings = getOption("P.warnings")
    )

    shiny::observeEvent(input$family_options, {
      shiny::showModal(shiny::modalDialog(
        shiny::checkboxInput(ns("family_options_nb_obs"), "Display the number of stations per bars?",
          value = getOption("P.NbObs")),
        shiny::div(
          colourpicker::colourInput(ns("family_options_nb_obs_col"), "Colours for station numbers:",
            value = getOption("P.NbObsCol"), showColour = "background"),
          style = "margin-left:25px;"
        ),
        shiny::checkboxInput(ns("family_options_warnings"), "Display warnings for low numbers?",
          value = getOption("P.warnings")),
        title = "Options for family barplots",
        footer = shiny::tagList(
          shiny::actionButton(ns("family_options_ok"), "Ok"),
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("family_options_reset"), "Reset")
        ),
        size = "l"
      ))
    })

    shiny::observeEvent(input$family_options_ok,{
      setOption("P.NbObs", input$family_options_nb_obs)
      setOption("P.NbObsCol", input$family_options_nb_obs_col)
      setOption("P.warnings", input$family_options_warnings)

      shiny::removeModal()
    })

    shiny::observeEvent(input$family_options_reset, {
      setOption("P.NbObs", family_optn$nb_obs)
      setOption("P.NbObsCol", family_optn$nb_obs_col)
      setOption("P.warnings", family_optn$warnings)

      shiny::updateCheckboxInput(inputId = "family_options_nb_obs", value = family_optn$nb_obs)
      colourpicker::updateColourInput(session = session, inputId = "family_options_nb_obs_col", value = family_optn$nb_obs_col)
      shiny::updateCheckboxInput(inputId = "family_options_warnings", value = family_optn$warnings)
    })

  })
}

## To be copied in the UI
# mod_family_frequencies_options_ui("family_frequencies_options_1")

## To be copied in the server
# mod_family_frequencies_options_server("family_frequencies_options_1")
