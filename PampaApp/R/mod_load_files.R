#' load_files UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom bsplus use_bs_tooltip bs_embed_tooltip shiny_iconlink shinyInput_label_embed
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinyFiles shinyDirButton
mod_load_files_ui <- function(id){
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      bsplus::use_bs_tooltip(),
      shinyjs::useShinyjs(),
      shinyFeedback::useShinyFeedback(),
      shiny::h3("Data / Files", align = "center"),
      shiny::br(),
      shinyWidgets::radioGroupButtons(ns("data_load_selection"), "",
        choices = c(
          "Load" = "load",
          "Data subsetting" = "selection"
        ),
        justified = TRUE
      ),
      shiny::div(id = ns("data_load"),
        shiny::br(),
        bsplus::bs_embed_tooltip(
          shiny::div(shinyFiles::shinyDirButton(ns("load_ws_choosen"), "Select the working directory", "Select a folder", FALSE)),
          bsplus::shiny_iconlink(),
          placement = "top",
          title = "The working directory must contain a data folder where the datasets are located."
        ),
        shiny::br(),
        bsplus::bs_embed_tooltip(
          shiny::textInput(ns("load_ws_written"), "Path of the working directory"),
          bsplus::shiny_iconlink(),
          placement = "top",
          title = "The working directory must contain a data folder where the datasets are located."
        ),
        shiny::fileInput(ns("load_unitobs_file"), "Choose an unitobs file"),
        shiny::fileInput(ns("load_obs_file"), "Choose an observation file"),
        shiny::fileInput(ns("load_refesp_file"), "Choose a species reference table"),
        shiny::fileInput(ns("load_local_refesp_file"), "Choose a local species reference table (optional)"),
        bsplus::shinyInput_label_embed(
          shiny::fileInput(ns("load_refspa_file"), "Choose a spatial reference table (optional)"),
          bsplus::bs_embed_tooltip(
            bsplus::shiny_iconlink(),
            placement = "top",
            title = "The spatial reference table must be a shapefile (.shp) and must be in a folder named 'Maps' (in the folder 'Data')."
          )
        ),
        bsplus::shinyInput_label_embed(
          shiny::numericInput(ns("load_dmin"), "Max value for Dmin (in m) (for STAVIRO data only)", value = 5),
          bsplus::bs_embed_tooltip(
            bsplus::shiny_iconlink(),
            placement = "top",
            title = "Select the minimal threshold of Min.Distance (in m) above which observations are not tacking in consideration. For STAVIRO data only."
          )
        ),
        shiny::div(
          shiny::actionButton(ns("load_load_data_button"), "Load"),
          shiny::actionButton(ns("load_reset_button"), "Reset"),
          align = "center"
        ),
        shiny::br(),
        shiny::br()
      ),
      shinyjs::hidden(
        shiny::div(id = ns("data_selection"),
          shiny::br(),
          shiny::strong("Data subsetting according to observation unit"),
          shiny::br(),
          shiny::actionButton(ns("load_selection_unitobs"), "Selection"),
          shiny::br(),
          shiny::br(),
          shiny::strong("Data subsetting according to species"),
          shiny::br(),
          shiny::actionButton(ns("load_selection_refesp"), "Selection"),
          shiny::br(),
          shiny::br(),
          shiny::strong("Restore initial data"),
          shiny::br(),
          shiny::actionButton(ns("load_restore_data"), "Restore"),
        )
      )
    ),
    shiny::mainPanel(width = 9,
      shiny::h3("Observation Table", align = "center"),
      shiny::h4("Filters :", align = "center"),
      shiny::htmlOutput(ns("text_filters"), align = "center"),
      shiny::dataTableOutput(ns("obs_table"))
    )
  )
}

#' load_files Server Functions
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinyjs show hide reset
#' @importFrom shinyFiles getVolumes shinyDirChoose
#' @importFrom shinyFeedback hideFeedback showFeedbackDanger
#' @importFrom fs path_home
#' @importFrom PAMPA load_files.f selection.f restoreData.f
mod_load_files_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$data_load_selection, {
      if (input$data_load_selection == "load"){
        shinyjs::show("data_load")
        shinyjs::hide("data_selection")
      } else{
        shinyjs::hide("data_load")
        shinyjs::show("data_selection")
      }
    })

    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())
    shinyFiles::shinyDirChoose(input, "load_ws_choosen", roots = volumes, session = session, restrictions = system.file(package = "base"))

    button <- shiny::reactiveValues(
      load_file = 0
    )

    reactives <- shiny::reactiveValues(
      obs = NULL,
      unitobs = NULL,
      refesp = NULL,
      loc_refesp = NULL,
      refspa = NULL,
      list_filters = "None"
    )

    shiny::observeEvent(input$load_ws_choosen, {
      if (length(input$load_ws_choosen) > 1){
        shiny::updateTextInput(inputId = "load_ws_written",
          value = paste(volumes[input$load_ws_choosen$root], paste(input$load_ws_choosen$path[-1],
            collapse = "/"), "", sep = "/")
        )
      }
    })

    shiny::observeEvent(input$load_ws_written, {
      shinyFeedback::hideFeedback("load_ws_written")
    })

    shiny::observeEvent(input$load_obs_file, {
      shinyFeedback::hideFeedback("load_obs_file")
      reactives$obs <- as.character(input$load_obs_file["datapath"])
    })

    shiny::observeEvent(input$load_unitobs_file, {
      shinyFeedback::hideFeedback("load_unitobs_file")
      reactives$unitobs <- as.character(input$load_unitobs_file["datapath"])
    })

    shiny::observeEvent(input$load_refesp_file, {
      shinyFeedback::hideFeedback("load_refesp_file")
      reactives$refesp <- as.character(input$load_refesp_file["datapath"])
    })

    shiny::observeEvent(input$load_local_refesp_file, {
      reactives$loc_refesp <- as.character(input$load_local_refesp_file["datapath"])
    })

    shiny::observeEvent(input$load_refspa_file, {
      reactives$refspa <- as.character(input$load_refspa_file["name"])
    })

    shiny::observeEvent(input$load_load_data_button, {
      error <- FALSE
      if (typeof(input$load_ws_choosen) == "integer" && input$load_ws_written == ""){
        shiny::showModal(shiny::modalDialog(
          shiny::h4(shiny::strong("A working directory is required."),
            style = "color: #d9534f; background-color: #ffffff",
            align = "center"),
          easyClose = TRUE
        ))
        error <- TRUE
      }
      if (!dir.exists(input$load_ws_written)){
        shinyFeedback::showFeedbackDanger("load_ws_written", text = "This directory doesn't exist.")
        error <- TRUE
      }
      if (is.null(reactives$unitobs)){
        shinyFeedback::showFeedbackDanger("load_unitobs_file", text = "An unitobs file is required.")
        error <- TRUE
      }
      if (is.null(reactives$obs)){
        shinyFeedback::showFeedbackDanger("load_obs_file", text = "An observation file is required.")
        error <- TRUE
      }
      if (is.null(reactives$refesp)){
        shinyFeedback::showFeedbackDanger("load_refesp_file", text = "A species reference table is required.")
        error <- TRUE
      }
      if (error) shiny::req(NULL)

      shiny::showModal(shiny::modalDialog("Loading data...", footer = NULL))
      button$load_file <- button$load_file + 1

      ws <- input$load_ws_written

      obs_file <- reactives$obs
      unitobs_file <- reactives$unitobs
      refesp_file <- reactives$refesp
      local_refesp_file <- reactives$loc_refesp
      refspa_file <- if(!is.null(reactives$refspa)) paste(ws, "Data/Maps/", reactives$refspa, sep = "") else NULL

      path <- c(unitobs = unitobs_file, obs = obs_file, refesp =  refesp_file, locrefesp = local_refesp_file, refspa = refspa_file, ws = ws)
      data <- PAMPA::load_files.f(path, dminMax = input$load_dmin, .GlobalEnv, .GlobalEnv)

      output$obs_table <- shiny::renderDataTable(data$obs)
      reactives$list_filters <- "None"
      shiny::removeModal()
      shiny::showModal(shiny::modalDialog(
        shiny::h5("Additional files availaible at:"),
        shiny::code(get("filePathes", envir = .GlobalEnv)["results"],
          style = "color: #000000; background-color: #ffffff"),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$load_reset_button, {
      shinyjs::reset("load_obs_file")
      shinyjs::reset("load_unitobs_file")
      shinyjs::reset("load_refesp_file")
      shinyjs::reset("load_local_refesp_file")
      shinyjs::reset("load_refspa_file")
      reactives$obs <- NULL
      reactives$unitobs <- NULL
      reactives$refesp <- NULL
      reactives$loc_refesp <- NULL
      reactives$refspa <- NULL
    })

    shiny::observeEvent(input$load_selection_unitobs, {
      old_data <- get("DataObs", envir = .GlobalEnv)
      data_selection <- PAMPA::selection.f('unitobs', .GlobalEnv, .GlobalEnv)
      output$obs_table <- shiny::renderDataTable(data_selection$obs)
      if (ncol(data_selection$obs) != ncol(old_data)){
        filter_name <- names(data_selection$obs)[ncol(data_selection$obs)]
        filter_value <- as.character(unique(data_selection$obs[,ncol(data_selection$obs)]))
        filter_text <- paste(filter_name, ": ", paste(filter_value, collapse = ", "), sep = "")
        if (reactives$list_filters == "None"){
          reactives$list_filters <- filter_text
        }else{
          reactives$list_filters <- paste(reactives$list_filters, filter_text, sep = ", <br>")
        }
      }
    })

    shiny::observeEvent(input$load_selection_refesp, {
      old_data <- get("DataObs", envir = .GlobalEnv)
      data_selection <- PAMPA::selection.f('refesp', .GlobalEnv, .GlobalEnv)
      output$obs_table <- shiny::renderDataTable(data_selection$obs)
      if (ncol(data_selection$obs) != ncol(old_data)){
        filter_name <- names(data_selection$obs)[ncol(data_selection$obs)]
        filter_value <- as.character(unique(data_selection$obs[,ncol(data_selection$obs)]))
        filter_text <- paste(filter_name, ": ", paste(filter_value, collapse = ", "), sep = "")
        if (reactives$list_filters == "None"){
          reactives$list_filters <- filter_text
        }else{
          reactives$list_filters <- paste(reactives$list_filters, filter_text, sep = "<br/>")
        }
      }
    })

    shiny::observeEvent(input$load_restore_data, {
      data_selection <- PAMPA::restoreData.f(.GlobalEnv, .GlobalEnv)
      output$obs_table <- shiny::renderDataTable(data_selection$obs)
      reactives$list_filters <- "None"
    })

    output$text_filters <- shiny::renderUI({
      shiny::HTML(reactives$list_filters)
    })

    return(reactive(button$load_file))
  })
}


## To be copied in the UI
# mod_load_files_ui("load_files_1")

## To be copied in the server
# mod_load_files_server("load_files_1")
