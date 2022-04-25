#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shiny::navbarPage(
      title = div(
        img(src = "www/pampa.GIF",
          height = "45px",
          style = "position: relative; margin:-15px 0px;"),
        "PAMPA 0.1",

      ),
#      title = img(src = "test.png"),
      shiny::tabPanel("Load Data",
        shiny::fluidPage(
          mod_load_files_ui("load_files_1")
        )
      ),
      shiny::navbarMenu("Graphics",
        shiny::tabPanel("Boxplot",
          shiny::fluidPage(
            mod_boxplot_ui("boxplot_1")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
#  add_resource_path(
#    "www",
#    app_sys("app/www")
#  )

  addResourcePath(
    'www',
    system.file('app/www/', package = 'PampaApp')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "PampaApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
