#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  devtools::load_all(path = "C:/Users/mmadelin/Documents/GitHub/Stage_PAMPA/PAMPA")
  load_file <- mod_load_files_server("load_files_1")

  mod_boxplot_server("boxplot_1", load_file)
  mod_boxplot_options_server("boxplot_options_1")
  mod_plot_options_server("plot_options_2")

  mod_plot_options_server("plot_options_1")
}
