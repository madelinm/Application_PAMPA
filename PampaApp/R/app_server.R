#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  options(shiny.maxRequestSize = 30*1024^2)

  load_file <- mod_load_files_server("load_files_1")

  mod_boxplot_server("boxplot_1", load_file)
  mod_boxplot_options_server("boxplot_options_1")
  mod_plot_options_server("plot_options_2")

  mod_barplot_server("barplot_1", load_file)
  mod_barplot_options_server("barplot_options_1")
  mod_plot_options_server("plot_options_3")

  mod_occurrence_frequencies_server("occurrence_frequencies_1", load_file)
  mod_occurrence_frequencies_options_server("occurrence_frequencies_options_1")
  mod_plot_options_server("plot_options_4")

  mod_family_frequencies_server("family_frequencies_1", load_file)
  mod_family_frequencies_options_server("family_frequencies_options_1")
  mod_plot_options_server("plot_options_5")

  mod_linear_model_server("linear_model_1", load_file)

  mod_mrt_server("mrt_1", load_file)
  mod_plot_options_server("plot_options_6")

  mod_permanova_server("permanova_1", load_file)

  mod_maps_symbols_server("maps_symbols_1", load_file)
  mod_maps_colours_server("maps_colours_1", load_file)
  mod_maps_boxplots_server("maps_boxplots_1", load_file)
  mod_maps_barplots_server("maps_barplots_1", load_file)

  mod_plot_options_server("plot_options_1")
}
