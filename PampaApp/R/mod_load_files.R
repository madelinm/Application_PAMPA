#' load_files UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyFiles
mod_load_files_ui <- function(id){
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shinyjs::useShinyjs(),
      shiny::h3("Load data", align = "center"),
      shiny::br(),
      shiny::tagList(
        shinyFiles::shinyDirButton(ns("load_ws"), "Select the working directory", "Select a folder", FALSE),
        shiny::br(),
        shiny::br(),
        shiny::fileInput(ns("load_obs_file"), "Choose an observation file"),
        shiny::fileInput(ns("load_unitobs_file"), "Choose an unitobs file"),
        shiny::fileInput(ns("load_refesp_file"), "Choose a species reference table"),
        shiny::fileInput(ns("load_local_refesp_file"), "Choose a local species reference table"),
        shiny::fileInput(ns("load_refspa_file"), "Choose a spatial reference table"),
        shiny::numericInput(ns("load_dmin"), "Max value for Dmin (in m) (for video data only)", value = 5),
        shiny::div(
          shiny::actionButton(ns("load_load_data_button"), "Load"),
          shiny::actionButton(ns("load_reset_button"), "Reset"),
          align = "center"
        ),
        shiny::br(),
        shiny::br()
      ),
      shiny::h3("Selection on data", align = "center"),
      shiny::br(),
      shiny::tagList(
        shiny::div(
          shiny::actionButton(ns("load_selection_unitobs"), "Selection on unitobs"),
          shiny::actionButton(ns("load_selection_refesp"), "Selection on refesp"),
          shiny::actionButton(ns("load_restore_data"), "Restore data"),
          align = "center"
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
#' @import shinyjs
#' @import shinyFiles
#' @import fs
mod_load_files_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())
    shinyFiles::shinyDirChoose(input, "load_ws", roots = volumes, session = session, restrictions = system.file(package = "base"))

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

    shiny::observeEvent(input$load_obs_file, {
      reactives$obs <- as.character(input$load_obs_file["datapath"])
    })

    shiny::observeEvent(input$load_unitobs_file, {
      reactives$unitobs <- as.character(input$load_unitobs_file["datapath"])
    })

    shiny::observeEvent(input$load_refesp_file, {
      reactives$refesp <- as.character(input$load_refesp_file["datapath"])
    })

    shiny::observeEvent(input$load_local_refesp_file, {
      reactives$loc_refesp <- as.character(input$load_local_refesp_file["datapath"])
    })

    shiny::observeEvent(input$load_refspa_file, {
      reactives$refspa <- as.character(input$load_refspa_file["name"])
    })


    shiny::observeEvent(input$load_load_data_button, {
      shiny::showModal(shiny::modalDialog("Loading data...", footer = NULL))
      button$load_file <- button$load_file + 1

      ws <- paste(volumes["Home"], paste(input$load_ws$path[-1], collapse = "/"), "", sep = "/")

      obs_file <- reactives$obs
      unitobs_file <- reactives$unitobs
      refesp_file <- reactives$refesp
      local_refesp_file <- reactives$loc_refesp
      refspa_file <- if(!is.null(reactives$refspa)) paste(ws, "Maps", reactives$refspa, sep = "/") else NULL

      path <- c(unitobs = unitobs_file, obs = obs_file, refesp =  refesp_file, locrefesp = local_refesp_file, refspa = refspa_file, ws = ws)
      data <- PAMPA::load_files.f(path, dminMax = input$load_dmin, .GlobalEnv, .GlobalEnv)

      output$obs_table <- shiny::renderDataTable(data$obs)
      shiny::removeModal()
      shiny::showModal(shiny::modalDialog(
        paste("Additionnal files availaible at", get("filePathes", envir = .GlobalEnv)["results"] ),
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
