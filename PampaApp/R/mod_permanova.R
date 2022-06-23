#' permanova UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom bsplus shinyInput_label_embed bs_embed_tooltip shiny_iconlink
mod_permanova_ui <- function(id){
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shinyFeedback::useShinyFeedback(),
      shiny::h3("Permanova", align = "center"),
      shiny::br(),
      shiny::radioButtons(ns("permanova_metric_table"), "Choose a metric table",
        choices = c(
          "... / station / size class" = "unitSpSz",
          "... / station" = "unitSp"
        )
      ),
      shiny::selectInput(ns("permanova_metric"), "Select a metric",
        choices = c(
          "",
          "presence-absence" = "pres_abs",
          "abundance",
          "density"
        )
      ),
      bsplus::shinyInput_label_embed(
        shiny::selectInput(ns("permanova_fact"), "Select explanatory factor(s)",
          choices = c(), multiple = TRUE
        ),
        bsplus::bs_embed_tooltip(
          bsplus::shiny_iconlink(),
          placement = "top",
          title = "Used if formula is empty. The default formula will be of the form 'dissimilarity_matrix ~ fact1 * ... * factn'."
        )
      ),
      bsplus::shinyInput_label_embed(
        shiny::textInput(ns("permanova_formula"), "Enter a RHS formula for the model"),
        bsplus::bs_embed_tooltip(
          bsplus::shiny_iconlink(),
          placement = "top",
          title = "RHS part of the formula. If empty, the explanatory factors will be used to create a default formula."
        )
      ),
      shiny::selectInput(ns("permanova_method"), "select a method for the dissimilarity matrix",
        choices = c("manhattan", "euclidean", "canberra", "clark", "bray", "kulczynski",
          "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup", "binomial",
          "chao", "cao", "mahalanobis", "chisq", "chord", "aitchison", "robust.aitchison"),
        selected = "bray"
      ),
      shiny::numericInput(ns("permanova_nb_perm"), "Choose the number of permutations",
        value = 1000, min = 1, step = 1
      ),
      shiny::checkboxInput(ns("permanova_sqrt_root"), "Use the square roots"),
      shiny::div(
        shiny::actionButton(ns("permanova_launch_button"), "Launch analysis")
      )
    ),
    shiny::mainPanel(width = 9,
      shiny::h3("Result", align = "center"),
      shiny::uiOutput(ns("permanova_result"))
    )
  )
}

#' permanova Server Functions
#'
#' @noRd
#'
#' @import shiny
#' @importFrom PAMPA permanova_pampa.f
#' @importFrom shinyFeedback hideFeedback showFeedbackDanger
mod_permanova_server <- function(id, load_file){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    params <- shiny::reactiveValues(
      metric_table = NULL,
      metric = NULL,
      fact = NULL,
      formula = NULL,
      method = NULL,
      nb_perm = NULL,
      sqrt_roots = NULL
    )

    shiny::observeEvent(input$permanova_metric, {
      if (input$permanova_metric == "pres_abs"){
        shiny::updateSelectInput(inputId = "permanova_method", choices = "binomial")
      } else {
        shiny::updateSelectInput(inputId = "permanova_method",
          choices = c("manhattan", "euclidean", "canberra", "clark", "bray", "kulczynski",
            "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup", "binomial",
            "chao", "cao", "mahalanobis", "chisq", "chord", "aitchison", "robust.aitchison"),
          selected = "bray"
        )
      }
    })

    shiny::observeEvent({
      input$permanova_metric_table
      load_file()
    }, {
      if (load_file() != 0){
        choices <- PAMPA:::UnitobsFields.aliases(dataEnv = .GlobalEnv, ordered = TRUE,
          tableMetrique = input$permanova_metric_table)
        shiny::updateSelectInput(inputId = "permanova_fact", choices = c("", choices))
      }
    })

    shiny::observeEvent(input$permanova_metric, {
      shinyFeedback::hideFeedback("permanova_metric")
    })

    shiny::observeEvent({
      input$permanova_fact
      input$permanova_formula
    }, {
      shinyFeedback::hideFeedback("permanova_fact")
      shinyFeedback::hideFeedback("permanova_formula")
    })

    shiny::observeEvent(input$permanova_nb_perm, {
      shinyFeedback::hideFeedback("permanova_nb_perm")
    })

    shiny::observeEvent(input$permanova_launch_button, {
      error <- FALSE
      if (input$permanova_metric == ""){
        shinyFeedback::showFeedbackDanger("permanova_metric", text = "A metric is required.")
        error <- TRUE
      }
      if (is.null(input$permanova_fact) & input$permanova_formula == ""){
        shinyFeedback::showFeedbackDanger("permanova_fact", text = "Explanatory factor(s) or formula are required.")
        shinyFeedback::showFeedbackDanger("permanova_formula", text = "Explanatory factor(s) or formula are required.")
        error <- TRUE
      }
      if (input$permanova_nb_perm < 1){
        shinyFeedback::feedbackDanger("permanova_nb_perm", text = "Number of permutations can be under 0.")
        error <- TRUE
      }
      if (error){
        shiny::req(NULL)
      }

      shiny::showModal(shiny::modalDialog("Calculation...", footer = NULL))

      params$metric_table <- input$permanova_metric_table
      params$metric <- input$permanova_metric
      params$fact <- input$permanova_fact
      params$formula <- input$permanova_formula
      params$method <- input$permanova_method
      params$nb_perm <- input$permanova_nb_perm
      params$sqrt_roots <- input$permanova_sqrt_root

      output$permanova_result <- shiny::renderTable(
        PAMPA::permanova_pampa.f(
          metric_table = params$metric_table,
          metric = params$metric,
          fact = params$fact,
          method = params$method,
          nb_perm = params$nb_perm,
          square_roots = params$sqrt_roots,
          dataEnv = .GlobalEnv, baseEnv = .GlobalEnv
        )
      )
      shiny::removeModal()
    })
  })
}

## To be copied in the UI
# mod_permanova_ui("permanova_1")

## To be copied in the server
# mod_permanova_server("permanova_1")
