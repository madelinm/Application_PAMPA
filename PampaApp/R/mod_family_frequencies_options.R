#' family_frequencies_options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
mod_family_frequencies_options_ui <- function(id){
  ns <- NS(id)
  shiny::actionButton(ns("family_options"), "Family frequencies options")
}

#' family_frequencies_options Server Functions
#'
#' @noRd
#'
#' @import shiny
#' @importFrom R.utils setOption
mod_family_frequencies_options_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    family_optn <- shiny::reactiveValues(
      warnings = getOption("P.warnings")
    )

    shiny::observeEvent(input$family_options, {
      shiny::showModal(shiny::modalDialog(
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
      R.utils::setOption("P.warnings", input$family_options_warnings)
      shiny::removeModal()
    })

    shiny::observeEvent(input$family_options_reset, {
      R.utils::setOption("P.warnings", family_optn$warnings)
      shiny::updateCheckboxInput(inputId = "family_options_warnings", value = family_optn$warnings)
    })
  })
}

## To be copied in the UI
# mod_family_frequencies_options_ui("family_frequencies_options_1")

## To be copied in the server
# mod_family_frequencies_options_server("family_frequencies_options_1")
