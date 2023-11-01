##' Create a customized Shiny UI with additional CSS and HTML
#'
#' This function extends a Shiny UI by adding custom CSS and HTML from the
#' \code{inst/www/} directory of the \code{untheme} package.
#'
#' @param ... Arguments passed to \code{\link[shiny]{div}} function.
#'
#' @return A \code{shiny.semantic::semanticPage} object with custom styling.
#' @export
#'
#' @examples
#' \dontrun{
#' ui <- fluidUnTheme(
#'   tabset(
#'     tabs = list(
#'       list(
#'         menu = "Tab 1",
#'         content = plotWithDownloadButtonsUI("plot1"),
#'         id = "first_tab"
#'       )
#'     )
#'   )
#' }
#' @seealso \code{\link[shiny]{addResourcePath}}, \code{\link[shiny]{includeHTML}}
fluidUnTheme <- function(...) {
  # Add custom CSS and HTML from inst/www/
  shiny::addResourcePath("custom-css", system.file("www", package = "untheme"))
  header_html <- shiny::includeHTML(system.file("www/index.html", package = "untheme"))

  shiny.semantic::semanticPage(
    margin = "0px",
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom-css/styles.css")
    ),
    header_html,
    shiny::div(class = "custom-width", ...)
  )
}

#' Create a UI component with a plot and optional download buttons
#'
#' This function creates a Shiny UI component that includes a plot, a download button
#' for the plot, and optionally, a download button for the data and radio buttons for
#' selecting the scale type.
#'
#' @param id A unique identifier for the UI component.
#' @param radio_button A radio button widget to be placed in a sidebar panel.
#'
#' @return A \code{shiny::sidebarLayout} object containing the plot and download buttons.
#' @export
#'
#' @examples
#' \dontrun{
#' ui <- plotWithDownloadButtonsUI("plot1", radio_choices = c("Absolute", "Percentage"))
#' }
#' @seealso \code{\link[shiny]{NS}}, \code{\link[shiny]{downloadButton}}, \code{\link[shiny]{plotOutput}}
plotWithDownloadButtonsUI <- function(id, radio_button = NULL) {
  ns <- shiny::NS(id)

  layout <-
    shiny.semantic::sidebar_layout(
      shiny.semantic::sidebar_panel(
        radio_button,
        shiny::br(),
        shiny::downloadButton(ns("downloadPlot"), "Download Plot"),
        shiny::downloadButton(ns("downloadData"), "Download Data")
      ),
      shiny.semantic::main_panel(
        plotly::plotlyOutput(ns("plot"), height = "600px", width = "1000px"),
        width = 4
      )
    )

  layout
}



#' Create a server component for rendering and downloading a plot
#'
#' This function creates a Shiny server component for rendering a ggplot object
#' and providing download buttons for the plot and data.
#'
#' @param input A list of input values from the Shiny UI.
#' @param output A list of output values to be modified by this function.
#' @param session The Shiny session object.
#' @param ggplot_obj A ggplot object to be rendered.
#' @param update_ggplot_func An optional function for updating the ggplot object
#'   based on user input.
#'
#' @importFrom utils write.csv
#'
#' @return None. This function modifies the \code{output} list in-place.
#' @export
#'
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   plotWithDownloadButtons(input, output, session, data, ggplot_obj)
#' }
#' }
#' @seealso \code{\link[shiny]{reactive}}, \code{\link[shiny]{renderPlot}}, \code{\link[shiny]{downloadHandler}}
plotWithDownloadButtons <- function(input, output, session, ggplot_obj, update_ggplot_func = NULL) {
  reactive_ggplot_obj <- shiny::reactive({
    if (is.null(update_ggplot_func)) {
      ggplot_obj
    } else {
      update_ggplot_func(ggplot_obj, input$scaleType)
    }
  })

  output$plot <- plotly::renderPlotly({
    print(reactive_ggplot_obj())
  })

  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      paste("plot.png")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = reactive_ggplot_obj(), bg = "white")
    }
  )

  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      paste("data.csv")
    },
    content = function(file) {
      write.csv(ggplot_obj$data, file)
    }
  )
}
