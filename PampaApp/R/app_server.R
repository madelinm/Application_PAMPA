#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  devtools::load_all(path = "C:/Users/mmadelin/Documents/package_pampa/PAMPA")
  load_file <- mod_load_files_server("load_files_1")
}
