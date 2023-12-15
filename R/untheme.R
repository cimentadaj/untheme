#' Create a field set with a given icon, label, and selectInput
#'
#' This function creates a field set for the Shiny application, consisting of an icon, a label, and a select or numeric input.
#' The function allows for easy generation of a consistent UI element for different types of inputs.
#'
#' @param icon_name Name of the icon used next to the label.
#' @param label_text Text for the label describing the input field.
#' @param input_id ID for the select or numeric input, used for input retrieval in server logic.
#' @param input_choices Choices for the select input (ignored if numeric_input is TRUE).
#' @param input_selected Default selected choice for the select input or default value for the numeric input.
#' @param numeric_input Logical indicating whether to use a numeric input instead of a select input (default is FALSE).
#' @importFrom shiny.semantic icon label selectInput numeric_input
#' @export
create_field_set <- function(icon_name, label_text, input_id, input_choices, input_selected, numeric_input = FALSE) {
  if (numeric_input) {
    input_widget <-
      numeric_input(
        input_id,
        NULL,
        value = input_selected
      )
  } else {
    input_widget <-
      selectInput(
        input_id,
        NULL,
        choices = input_choices,
        selected = input_selected
      )
  }

  div(
    class = "field",
    icon(icon_name),
    label(
      class = "main label",
      label_text
    ),
    input_widget
  )
}


#' Generate a Tab UI Component for the Application
#'
#' This function generates a tab UI component for Shiny applications. It is designed to encapsulate a plot with optional additional UI elements in a tabbed interface.
#'
#' @param tab_name The name displayed on the tab, defining its identity and purpose.
#' @param plot_ui_id The UI id for the plot, used for rendering the plot in the server logic.
#' @param extra_ui An optional UI component (such as additional inputs or text) to be included in the tab.
#' @param width The width of the plotly output. By default, it is 1000px
#' @importFrom shiny div
#' @return A list representing a tab UI component suitable for integration into a tabset panel.
#' @export
create_tab <- function(tab_name, plot_ui_id, extra_ui = NULL, width = "1000px") {
  list(
    menu = tab_name,
    content = plotWithDownloadButtonsUI(plot_ui_id, extra_ui, width = width),
    id = paste0(tolower(gsub(" ", "_", tab_name)), "_tab")
  )
}

#' Generate the plot modules for an arbitrary number of plots.
#'
#' This function facilitates the generation of multiple plot modules in a Shiny application, particularly following a simulation or data analysis process.
#' It accepts a variable number of reactive expressions, each returning a ggplot2 object, and renders these plots within the application.
#'
#' @param ... A variable number of reactive expressions, each returning a ggplot2 object for rendering.
#' @importFrom shiny callModule observe
#' @export
plots_tabset <- function(...) {
  observe({
    args <- list(...)
    tab_counter <- 0

    lapply(args, function(arg_list) {
      tab_counter <<- tab_counter + 1
      plot_data <- arg_list$plt_reactive()

      callModule(
        plotWithDownloadButtons,
        paste0("plot", tab_counter),
        ggplot_obj = plot_data,
        file_name = arg_list$filename
      )
    })
  })
}


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
  header_html <- shiny::includeHTML(system.file("www/header.html", package = "untheme"))
  footer_html <- shiny::includeHTML(system.file("www/footer.html", package = "untheme"))

  shiny.semantic::semanticPage(
    margin = "0px",
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom-css/header_styles.css")
    ),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom-css/footer_styles.css")
    ),
    header_html,
    shiny::div(class = "custom-width", ...)
    ## footer_html
  )
}


#' Create a UI component with a plot and optional download buttons
#'
#' This function creates a Shiny UI component that includes a plot from Plotly, a download button
#' for the plot, and optionally, a download button for the data and radio buttons for
#' selecting the scale type.
#'
#' @param id A unique identifier for the UI component.
#' @param radio_button A radio button widget to be placed in a sidebar panel.
#' @param width The width of the plotly output. By default, it is 1000px
#'
#' @return A \code{shiny::sidebarLayout} object containing the plot and download buttons.
#' @export
#'
#' @examples
#' \dontrun{
#' ui <- plotWithDownloadButtonsUI("plot1", radio_choices = c("Absolute", "Percentage"))
#' }
#' @seealso \code{\link[shiny]{NS}}, \code{\link[shiny]{downloadButton}}, \code{\link[shiny]{plotOutput}}
plotWithDownloadButtonsUI <- function(id, radio_button = NULL, width = "1000px") {
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
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("plot"), height = "600px", width = width)
        ),
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
#' @param ggplot_obj A reactive expression returning a list with a named ggplot object (gg) and a named plotly plot (plotly).
#' @param file_name The file name to use for the plot download and the data.
#' @param update_ggplot_func An optional function for updating the ggplot object
#'   based on user input.
#'
#' @importFrom utils write.csv
#'
#' @return None. This function modifies the \code{output} list in-place.
#' @export
#'
#' @seealso \code{\link[shiny]{reactive}}, \code{\link[shiny]{renderPlot}}, \code{\link[shiny]{downloadHandler}}
plotWithDownloadButtons <- function(input, output, session, ggplot_obj, file_name, update_ggplot_func = NULL) {
  reactive_ggplot_obj <- shiny::reactive({
    if (is.null(update_ggplot_func)) {
      ggplot_obj
    } else {
      update_ggplot_func(ggplot_obj, input$scaleType)
    }
  })

  output$plot <- plotly::renderPlotly({
    reactive_ggplot_obj()$plotly
  })

  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      paste0(file_name, "_plot.png")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = reactive_ggplot_obj()$gg, bg = "white")
    }
  )

  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      paste0(file_name, "_data.csv")
    },
    content = function(file) {
      write.csv(reactive_ggplot_obj()$gg$data, file)
    }
  )
}
