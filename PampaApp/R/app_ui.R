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
        img(src = "www/logopampa.PNG",
          height = "45px",
          style = "position: relative; margin:-15px 0px;"),
        "PAMPA 0.1",

      ),
      shiny::tabPanel("Load Data",
        shiny::fluidPage(
          mod_load_files_ui("load_files_1")
        )
      ),
      shiny::navbarMenu("Plots",
        shiny::tabPanel("Boxplot",
          shiny::fluidPage(
            mod_boxplot_ui("boxplot_1")
          )
        ),
        shiny::tabPanel("Barplot",
          shiny::fluidPage(
            mod_barplot_ui("barplot_1")
          )
        ),
        shiny::tabPanel("Occurrence frequencies",
          shiny::fluidPage(
            mod_occurrence_frequencies_ui("occurrence_frequencies_1")
          )
        ),
        shiny::tabPanel("Family frequencies",
          shiny::fluidPage(
            mod_family_frequencies_ui("family_frequencies_1")
          )
        )
      ),
      shiny::navbarMenu("Stats",
        shiny::tabPanel("Linear models",
          shiny::fluidPage(
            mod_linear_model_ui("linear_model_1")
          )
        ),
        shiny::tabPanel("Multivariate Regression Trees",
          shiny::fluidPage(
            mod_mrt_ui("mrt_1")
          )
        ),
        shiny::tabPanel("Permanova",
          shiny::fluidPage(
            mod_permanova_ui("permanova_1")
          )
        )
      ),
      shiny::navbarMenu("Maps",
        shiny::tabPanel("With symbols",
          shiny::fluidPage(
            mod_maps_symbols_ui("maps_symbols_1")
          )
        ),
        shiny::tabPanel("With coloured polygons",
          shiny::fluidPage(
            mod_maps_colours_ui("maps_colours_1")
          )
        ),
        shiny::tabPanel("With boxplots",
          shiny::fluidPage(
            mod_maps_boxplots_ui("maps_boxplots_1")
          )
        ),
        shiny::tabPanel("With barplot",
          shiny::fluidPage(
            mod_maps_barplots_ui("maps_barplots_1")
          )
        )
      ),
      shiny::tabPanel("Options",
        shiny::fluidPage(
          mod_plot_options_ui("plot_options_1")
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
