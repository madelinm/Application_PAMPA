#' boxplot_options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
mod_boxplot_options_ui <- function(id){
  ns <- NS(id)
  shiny::actionButton(ns("boxplot_options"), "Boxplot options")
}

#' boxplot_options Server Functions
#'
#' @noRd
#'
#' @import shiny
#' @importFrom colourpicker colourInput updateColourInput
#' @importFrom R.utils setOption
mod_boxplot_options_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    boxplot_optn <- shiny::reactiveValues(
      max_exclu = getOption("P.maxExclu"),
      prop_max = getOption("P.GraphPartMax"),
      nb_obs = getOption("P.NbObs"),
      nb_obs_col = getOption("P.NbObsCol"),
      warnings = getOption("P.warnings"),
      group_sep = getOption("P.sepGroupes"),
      group_sep_col = getOption("P.sepGroupesCol"),
      colour_legend = getOption("P.legendCouleurs"),
      mean_pts = getOption("P.pointMoyenne"),
      mean_pts_col = getOption("P.pointMoyenneCol"),
      mean_pts_cex = getOption("P.pointMoyenneCex"),
      mean_pts_type = getOption("P.pointMoyennePch"),
      mean_val = getOption("P.valMoyenne"),
      mean_val_col = getOption("P.valMoyenneCol"),
      nb_decimal = getOption("P.NbDecimal")
    )

    shiny::observeEvent(input$boxplot_options, {
      shiny::showModal(shiny::modalDialog(
        shiny::fluidRow(
          shiny::column(width = 6,
            shiny::checkboxInput(ns("boxplot_options_max_exclu"), "Do not display extreme values",
              value = getOption("P.maxExclu")
            ),
            shiny::div(
              shiny::numericInput(ns("boxplot_options_prop_max"), "(greater than ... x the max value)?",
                value = getOption("P.GraphPartMax"), step = 0.05),
              style = "margin-left:25px;"
            ),
            shiny::checkboxInput(ns("boxplot_options_nb_obs"), "Display the number of records per box?",
              value = getOption("P.NbObs")),
            shiny::div(
              colourpicker::colourInput(ns("boxplot_options_nb_obs_col"), "Colour of record numbers:",
                value = getOption("P.NbObsCol"), showColour = "background"),
              style = "margin-left:25px;"
            ),
            shiny::checkboxInput(ns("boxplot_options_warnings"), "Display warnings? (low number + excluded max)",
              value = getOption("P.warnings")),
            shiny::checkboxInput(ns("boxplot_options_group_sep"), "Display group separator (first level factor)?",
              value = getOption("P.sepGroupes")),
            shiny::div(
              colourpicker::colourInput(ns("boxplot_options_group_sep_col"), "Colour for separators:",
                value = getOption("P.sepGroupesCol"), showColour = "background"),
              style = "margin-left:25px;"
            ),
          ),
          shiny::column(width = 6, style = "border-left: 1px solid",
            shiny::checkboxInput(ns("boxplot_options_colour_legend"), "Display colour legend (second level factor)?",
              value = getOption("P.legendeCouleurs")),
            shiny::checkboxInput(ns("boxplot_options_mean_pts"), "Display the means (points) on the boxes?",
              value = getOption("P.pointMoyenne")),
            shiny::div(
              colourpicker::colourInput(ns("boxplot_options_mean_pts_col"), "Colour of points:",
                value = getOption("P.pointMoyenneCol"), showColour = "background"),
              shiny::numericInput(ns("boxplot_options_mean_pts_cex"), "Point size multiplier:",
                value = getOption("P.pointMoyenneCex")),
              shiny::numericInput(ns("boxplot_options_mean_pts_type"), "Point type:",
                value = getOption("P.pointMoyennePch")),
              style = "margin-left:25px;"
            ),
            shiny::checkboxInput(ns("boxplot_options_mean_val"), "Display the mean values on the boxplots?",
              value = getOption("P.valMoyenne")),
            shiny::div(
              colourpicker::colourInput(ns("boxplot_options_mean_val_col"), "Colour for mean values:",
                value = getOption("P.valMoyenneCol"), showColour = "background"),
              shiny::numericInput(ns("boxplot_options_nb_decimal"), "Number of decimals:",
                value = getOption("P.NbDecimal"), width = "50%"),
              style = "margin-left:25px;"
            )
          )
        ),
        title = "Boxplot options",
        footer = shiny::tagList(
          shiny::actionButton(ns("boxplot_options_ok"), "Ok"),
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("boxplot_options_reset"), "Reset")
        ),
        size = "l"
      ))
    })

    shiny::observeEvent(input$boxplot_options_ok, {
      R.utils::setOption("P.maxExclu", input$boxplot_options_max_exclu)
      R.utils::setOption("P.GraphPartMax", input$boxplot_options_prop_max)
      R.utils::setOption("P.NbObs", input$boxplot_options_nb_obs)
      R.utils::setOption("P.NbObsCol", input$boxplot_options_nb_obs_col)
      R.utils::setOption("P.warnings", input$boxplot_options_warnings)
      R.utils::setOption("P.sepGroupes", input$boxplot_options_group_sep)
      R.utils::setOption("P.sepGroupesCol", input$boxplot_options_group_sep_col)
      R.utils::setOption("P.legendeCouleurs", input$boxplot_options_colour_legend)
      R.utils::setOption("P.pointMoyenne", input$boxplot_options_mean_pts)
      R.utils::setOption("P.pointMoyenneCol", input$boxplot_options_mean_pts_col)
      R.utils::setOption("P.pointMoyenneCex", input$boxplot_options_mean_pts_cex)
      R.utils::setOption("P.pointMoyennePch", input$boxplot_options_mean_pts_type)
      R.utils::setOption("P.valMoyenne", input$boxplot_options_mean_val)
      R.utils::setOption("P.valMoyenneCol", input$boxplot_options_mean_val_col)
      R.utils::setOption("P.NbDecimal", input$boxplot_options_nb_decimal)

      shiny::removeModal()
    })

    shiny::observeEvent(input$boxplot_options_reset, {
      R.utils::setOption("P.maxExclu", boxplot_optn$max_exclu)
      R.utils::setOption("P.GraphPartMax", boxplot_optn$prop_max)
      R.utils::setOption("P.NbObs", boxplot_optn$nb_obs)
      R.utils::setOption("P.NbObsCol", boxplot_optn$nb_obs_col)
      R.utils::setOption("P.warnings", boxplot_optn$warnings)
      R.utils::setOption("P.sepGroupes", boxplot_optn$group_sep)
      R.utils::setOption("P.sepGroupesCol", boxplot_optn$group_sep_col)
      R.utils::setOption("P.legendeCouleurs", boxplot_optn$colour_legend)
      R.utils::setOption("P.pointMoyenne", boxplot_optn$mean_pts)
      R.utils::setOption("P.pointMoyenneCol", boxplot_optn$mean_pts_col)
      R.utils::setOption("P.pointMoyenneCex", boxplot_optn$mean_pts_cex)
      R.utils::setOption("P.pointMoyennePch", boxplot_optn$mean_pts_type)
      R.utils::setOption("P.valMoyenne", boxplot_optn$mean_val)
      R.utils::setOption("P.valMoyenneCol", boxplot_optn$mean_val_col)
      R.utils::setOption("P.NbDecimal", boxplot_optn$nb_decimal)

      shiny::updateCheckboxInput(inputId = "boxplot_options_max_exclu", value = boxplot_optn$max_exclu)
      shiny::updateNumericInput(inputId = "boxplot_options_prop_max", value = boxplot_optn$prop_max)
      shiny::updateCheckboxInput(inputId = "boxplot_options_nb_obs", value = boxplot_optn$nb_obs)
      colourpicker::updateColourInput(session = session, inputId = "boxplot_options_nb_obs_col", value = boxplot_optn$nb_obs_col)
      shiny::updateCheckboxInput(inputId = "boxplot_options_warnings", value = boxplot_optn$warnings)
      shiny::updateCheckboxInput(inputId = "boxplot_options_group_sep", value = boxplot_optn$group_sep)
      colourpicker::updateColourInput(session = session, inputId = "boxplot_options_group_sep_col", value = boxplot_optn$group_sep_col)
      shiny::updateCheckboxInput(inputId = "boxplot_options_colour_legend", value = boxplot_optn$colour_legend)
      shiny::updateCheckboxInput(inputId = "boxplot_options_mean_pts", value = boxplot_optn$mean_pts)
      colourpicker::updateColourInput(session = session, inputId = "boxplot_options_mean_pts_col", value = boxplot_optn$mean_pts_col)
      shiny::updateNumericInput(inputId = "boxplot_options_mean_pts_cex", value = boxplot_optn$mean_pts_cex)
      shiny::updateNumericInput(inputId = "boxplot_options_mean_pts_type", value = boxplot_optn$mean_pts_type)
      shiny::updateCheckboxInput(inputId = "boxplot_options_mean_val", value = boxplot_optn$mean_val)
      colourpicker::updateColourInput(session = session, inputId = "boxplot_options_mean_val_col", value = boxplot_optn$mean_val_col)
      shiny::updateNumericInput(inputId = "boxplot_options_nb_decimal", value = boxplot_optn$nb_decimal)
    })
  })
}

## To be copied in the UI
# mod_boxplot_options_ui("boxplot_options_1")

## To be copied in the server
# mod_boxplot_options_server("boxplot_options_1")
