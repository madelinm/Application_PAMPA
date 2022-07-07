#' barplot_options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
mod_barplot_options_ui <- function(id){
  ns <- NS(id)
  shiny::actionButton(ns("barplot_options"), "Barplot options")
}

#' barplot_options Server Functions
#'
#' @noRd
#'
#' @import shiny
#' @importFrom colourpicker colourInput updateColourInput
#' @importFrom R.utils setOption
mod_barplot_options_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    barplot_optn <- shiny::reactiveValues(
      nb_obs = getOption("P.NbObs"),
      nb_obs_col = getOption("P.NbObsCol"),
      warnings = getOption("P.warnings"),
      statistics = getOption("P.barplotStat"),
      error_bar = getOption("P.barplotErrorBar")
    )

    shiny::observeEvent(input$barplot_options, {
      shiny::showModal(shiny::modalDialog(
        shiny::checkboxInput(ns("barplot_options_nb_obs"), "Display the number of records per bar?",
          value = getOption("P.NbObs")
        ),
        shiny::div(
          colourpicker::colourInput(ns("barplot_options_nb_obs_col"), "Colour for record numbers:",
            value = getOption("P.NbObsCol"), showColour = "background"),
          style = "margin-left:25px;"
        ),
        shiny::checkboxInput(ns("barplot_options_warnings"), "Display warnings for low numbers?",
          value = getOption("P.warnings")
        ),
        shiny::selectInput(ns("barplot_options_statistics"), "Statistics for the bars:",
          choices = c("mean", "median"), selected = getOption("P.barplotStat")
        ),
        shiny::checkboxInput(ns("barplot_options_error_bar"), "Display error bars?
          (std dev./inter-quartile range, depending on the central statistics)",
          value = getOption("P.barplotErrorBar")
        ),
        title = "Barplot options",
        footer = shiny::tagList(
          shiny::actionButton(ns("barplot_options_ok"), "Ok"),
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("barplot_options_reset"), "Reset")
        ),
        size = "l"
      ))
    })

    shiny::observeEvent(input$barplot_options_ok, {
      R.utils::setOption("P.NbObs", input$barplot_options_nb_obs)
      R.utils::setOption("P.NbObsCol", input$barplot_options_nb_obs_col)
      R.utils::setOption("P.warnings", input$barplot_options_warnings)
      R.utils::setOption("P.barplotStat", input$barplot_options_statistics)
      R.utils::setOption("P.barplotErrorBar", input$barplot_options_error_bar)

      shiny::removeModal()
    })

    shiny::observeEvent(input$barplot_options_reset, {
      R.utils::setOption("P.NbObs", barplot_optn$nb_obs)
      R.utils::setOption("P.NbObsCol", barplot_optn$nb_obs_col)
      R.utils::setOption("P.warnings", barplot_optn$warnings)
      R.utils::setOption("P.barplotStat", barplot_optn$statistics)
      R.utils::setOption("P.barplotErrorBar", barplot_optn$error_bar)

      shiny::updateCheckboxInput(inputId = "barplot_options_nb_obs", value = barplot_optn$nb_obs)
      colourpicker::updateColourInput(session = session, inputId = "barplot_options_nb_obs_col", value = barplot_optn$nb_obs_col)
      shiny::updateCheckboxInput(inputId = "barplot_options_warnings", value = barplot_optn$warnings)
      shiny::updateSelectInput(inputId = "barplot_options_statistics", selected = barplot_optn$statistics)
      shiny::updateCheckboxInput(inputId = "barplot_options_error_bar", value = barplot_optn$error_bar)
    })
  })
}

## To be copied in the UI
# mod_barplot_options_ui("barplot_options_1")

## To be copied in the server
# mod_barplot_options_server("barplot_options_1")
