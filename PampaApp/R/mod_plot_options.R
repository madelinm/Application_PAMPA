#' plot_options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_options_ui <- function(id){
  ns <- NS(id)
  shiny::actionButton(ns("plot_options"), "Common graphical options")
}

#' plot_options Server Functions
#'
#' @noRd
mod_plot_options_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    optn <- shiny::reactiveValues(
      colour = getOption("P.colPalette"),
      language = getOption("P.lang"),
      publication = getOption("P.graphPaper"),
      title = getOption("P.title"),
      axes = getOption("P.axesLabels"),
      text_size = getOption("P.cex"),
      min_obs = getOption("P.MinNbObs"),
#      pdf = getOption("P.graphPDF"),
      one_file_page = getOption("P.PDFunFichierPage"),
      embed_fonts = getOption("P.pdfEmbedFonts"),
#      png = getOption("P.graphPNG"),
#      wmf = getOption("P.graphWMF"),
      multiple_graphics_page = getOption("P.plusieursGraphPage"),
      nrow_graph = getOption("P.nrowGraph"),
      ncol_graph = getOption("P.ncolGraph")
    )

    shiny::observeEvent(input$plot_options, {
      shiny::showModal(shiny::modalDialog(
        shiny::fluidRow(
          shiny::column(width = 6,
            shiny::h4("Graphics look :"),
            shiny::selectInput(ns("plot_options_colour"), "Colour palette",
              choices = c("default", "blue", "heat", "grey", "map1", "map2", "topo"),
              selected = getOption("P.colPalette")
            ),
            shiny::selectInput(ns("plot_options_language"), "Language for axis labels",
              choices = c("en", "fr"),
              selected = getOption("P.lang")
            ),
            shiny::checkboxInput(ns("plot_options_publication"),
              "Simplified graphics for publishing (smaller, no title)",
              value = getOption("P.graphPaper")),
            shiny::checkboxInput(ns("plot_options_title"), "Graphics with titles?",
              value = getOption("P.title")),
            shiny::checkboxInput(ns("plot_options_axes"), "Display axes labels?",
              value = getOption("P.axesLabels")),
            shiny::numericInput(ns("plot_options_text_size"), "Text size multiplier:",
              value = getOption("P.cex")),
            shiny::numericInput(ns("plot_options_min_observation"),
              "Do not produce graphics with less than ... observations",
              value = getOption("P.MinNbObs")),
          ),
          shiny::column(width = 6, style = "border-left: 1px solid",
            shiny::h4("Output options :"),
#            shiny::checkboxInput(ns("plot_options_pdf"), "PDF files?",
#              value = getOption("P.graphPDF")),
            shiny::h5("Options for pdf files:"),
            shiny::div(
              shiny::checkboxInput(ns("plot_options_one_file_page"), "One file per page (PDF)?",
                value = getOption("P.PDFunFichierPage")),
              shiny::checkboxInput(ns("plot_options_embed_fonts"), "Embed fonts in PDF?",
                value = getOption("P.pdfEmbedFonts")),
              style = "margin-left:25px;"
            ),
#            shiny::checkboxInput(ns("plot_options_png"), "PNG files?",
#              value = getOption("P.graphPNG")),
#            shiny::checkboxInput(ns("plot_options_wmf"), "WMF files? (+ displayed on screen; Windows only)",
#              value = getOption("P.graphWMF")),
            shiny::checkboxInput(ns("plot_options_multiple_graphics_page"), "Multiple graphics per page",
              value = getOption("P.plusieursGraphPage")),
            shiny::column(width = 6,
              shiny::selectInput(ns("plot_options_nrow_graph"), "Number of graphics per rows",
                choices = c(1, 2, 3), selected = getOption("P.nrowGraph"))
            ),
            shiny::column(width = 6,
              shiny::selectInput(ns("plot_options_ncol_graph"), "Number of graphics per columns",
                choices = c(1, 2, 3), selected = getOption("P.ncolGraph"))
            ),
          )
        ),
        title = "Common graphical options",
        footer = shiny::tagList(
          shiny::actionButton(ns("plot_options_ok"), "OK"),
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("plot_options_reset"), "Reset")
        ),
        size = "l"
      ))
    })

    observeEvent({
        input$plot_options_publication
#        input$plot_options
      }, {
        shinyjs::toggleState("plot_options_title", !input$plot_options_publication)
    })

    observeEvent(
      {
        input$plot_options_pdf
#        input$plot_options
      }, {
        shinyjs::toggleState("plot_options_one_file_page", input$plot_options_pdf)
        shinyjs::toggleState("plot_options_embed_fonts", input$plot_options_pdf)
        shinyjs::toggleState("plot_options_wmf", !input$plot_options_pdf)
    })

    observeEvent(
      {
        input$plot_options_png
#        input$plot_options
      }, {
        shinyjs::toggleState("plot_options_wmf", !input$plot_options_png)
    })

    observeEvent(input$plot_options_ok, {
      setOption("P.colPalette", input$plot_options_colour)
      setOption("P.lang", input$plot_options_language)
      setOption("P.graphPaper", input$plot_options_publication)
      setOption("P.title", input$plot_options_title)
      setOption("P.axesLabels", input$plot_options_axes)
      setOption("P.cex", input$plot_options_text_size)
      setOption("P.MinNbObs", input$plot_options_min_observation)
#      setOption("P.graphPDF", input$plot_options_pdf)
      setOption("P.PDFunFichierPage", input$plot_options_one_file_page)
      setOption("P.pdfEmbedFonts", input$plot_options_embed_fonts)
#      setOption("P.graphPNG", input$plot_options_png)
#      setOption("P.graphWMF", input$plot_options_wmf)
      setOption("P.plusieursGraphPage", input$plot_options_multiple_graphics_page)
      setOption("P.nrowGraph", input$plot_options_nrow_graph)
      setOption("P.ncolGraph", input$plot_options_ncol_graph)

      shiny::removeModal()
    })

    observeEvent(input$plot_options_reset, {
      setOption("P.colPalette", optn$colour)
      setOption("P.lang", optn$language)
      setOption("P.graphPaper", optn$publication)
      setOption("P.title", optn$title)
      setOption("P.axesLabels", optn$axes)
      setOption("P.cex", optn$text_size)
      setOption("P.MinNbObs", optn$min_obs)
#      setOption("P.graphPDF", optn$pdf)
      setOption("P.PDFunFichierPage", optn$one_file_page)
      setOption("P.pdfEmbedFonts", optn$embed_fonts)
#      setOption("P.graphPNG", optn$png)
#      setOption("P.graphWMF", optn$wmf)
      setOption("P.plusieursGraphPage", optn$multiple_graphics_page)
      setOption("P.nrowGraph", optn$nrow_graph)
      setOption("P.ncolGraph", optn$ncol_graph)

      shiny::updateSelectInput(inputId = "plot_options_colour", selected = optn$colour)
      shiny::updateSelectInput(inputId = "plot_options_language", selected = optn$language)
      shiny::updateCheckboxInput(inputId = "plot_options_publication", value = optn$publication)
      shiny::updateCheckboxInput(inputId = "plot_options_title", value = optn$title)
      shiny::updateCheckboxInput(inputId = "plot_options_axes", value = optn$axes)
      shiny::updateNumericInput(inputId = "plot_options_text_size", value = optn$text_size)
      shiny::updateNumericInput(inputId = "plot_options_min_observation", value = optn$min_obs)
#      shiny::updateCheckboxInput(inputId = "plot_options_pdf", value = optn$pdf)
      shiny::updateCheckboxInput(inputId = "plot_options_one_file_page", value = optn$one_file_page)
      shiny::updateCheckboxInput(inputId = "plot_options_embed_fonts", value = optn$embed_fonts)
#      shiny::updateCheckboxInput(inputId = "plot_options_png", value = optn$png)
#      shiny::updateCheckboxInput(inputId = "plot_options_wmf", value = optn$wmf)
      shiny::updateCheckboxInput(inputId = "plot_options_multiple_graphics_page", value = optn$multiple_graphics_page)
      shiny::updateSelectInput(inputId = "plot_options_nrow_graph", selected = optn$nrow_graph)
      shiny::updateSelectInput(inputId = "plot_options_ncol_graph", selected = optn$ncol_graph)
    })
  })
}

## To be copied in the UI
# mod_plot_options_ui("plot_options_1")

## To be copied in the server
# mod_plot_options_server("plot_options_1")
