#' Create a field set with a given icon, label, and selectInput
#'
#' @param icon_name Name of the icon used next to the label.
#' @param label_text Text for the label describing the input field.
#' @param input_id ID for the select or numeric input, used for input retrieval in server logic.
#' @param input_choices Choices for the select input (ignored if numeric_input is TRUE).
#' @param input_selected Default selected choice for the select input or default value for the numeric input.
#' @param numeric_input Logical indicating whether to use a numeric input instead of a select input (default is FALSE).
#' @export
create_field_set <- function(icon_name, label_text, input_id, input_choices, input_selected, numeric_input = FALSE) {
  if (numeric_input) {
    input_widget <- shiny.semantic::numeric_input(input_id, NULL, value = input_selected)
  } else {
    input_widget <- shiny.semantic::selectInput(input_id, NULL, choices = input_choices, selected = input_selected)
  }

  shiny::div(
    class = "field",
    shiny.semantic::icon(icon_name),
    shiny.semantic::label(class = "main label", label_text),
    input_widget
  )
}

#' Generate the plot modules for an arbitrary number of plots.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param id Unique identifier for the plot module.
#' @param plot_object An object containing a reactive expression for the plot and its filename.
#' @export
plots_tabset <- function(input, output, id, plot_object) {
  shiny::observe({
    plotWithDownloadButtons(
      input,
      output,
      id,
      ggplot_obj = plot_object$plt_reactive(),
      file_name = plot_object$filename
    )
  })
}

#' Create a customized Shiny UI with additional CSS and HTML
#'
#' @param ... Arguments passed to \code{\link[shiny]{div}} function.
#' @return A \code{shiny.semantic::semanticPage} object with custom styling.
#' @export
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
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom-css/layout_styles.css")
    ),
    shiny::div(
      class = "main-container",
      header_html,
      shiny::div(class = "content", ...),
      footer_html
    )
  )
}


#' Create a UI component with a plot and optional download buttons
#'
#' @param id A unique identifier for the UI component.
#' @param radio_button A radio button widget to be placed in a sidebar panel.
#' @param width The width of the plotly output. By default, it is 1000px
#' @return A \code{shiny::sidebarLayout} object containing the plot and download buttons.
#' @export
plotWithDownloadButtonsUI <- function(id, radio_button = NULL, width = "100%") {
  ids <- generate_ids(id)

  # Define the responsive grid template
  responsiveGrid <- custom_grid_template(
    default = list(
      areas = rbind(
        c("sidebar", "main")
      ),
      rows_height = c("auto"),
      cols_width = c("1fr", "4fr")
    ),
    mobile = list(
      areas = rbind(
        c("sidebar"),
        c("main")
      ),
      rows_height = c("min-content", "auto"),
      cols_width = c("1fr")
    )
  )

  container_style <- "height: 100%; margin: 0; padding: 0; border: 1px solid #dcdcdc; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1); box-sizing: border-box;"
  sidebar_style <- "background-color: #f5f5f5; border: 1px solid #dcdcdc; box-sizing: border-box; max-width: 100%;"
  main_style <- "padding: 20px; box-sizing: border-box; max-width: 100%;"

  # Create the grid layout with additional styles for the sidebar
  layout <- shiny.semantic::grid(
    responsiveGrid,
    container_style = container_style,
    area_styles = list(
      sidebar = sidebar_style,
      main = main_style
    ),
    sidebar = shiny::div(
      class = "custom-sidebar",
      style = "overflow-x: auto;",
      br(),
      radio_button,
      shiny::br(),
      shiny::downloadButton(ids$download_plot_id, "Download Plot"),
      shiny::downloadButton(ids$download_data_id, "Download Data")
    ),
    main = shiny::div(
      class = "custom-main-panel",
      shinycssloaders::withSpinner(plotly::plotlyOutput(ids$plot_id, height = "600px", width = "100%"))
    )
  )

  # Return the layout
  layout
}

#' Create a server component for rendering and downloading a plot
#'
#' @param input A list of input values from the Shiny UI.
#' @param output A list of output values to be modified by this function.
#' @param ids A list of IDs generated for the plot and download buttons.
#' @param ggplot_obj A reactive expression returning a list with a named ggplot object and a named plotly plot.
#' @param file_name The file name to use for the plot download and the data.
#' @param update_ggplot_func An optional function for updating the ggplot object based on user input.
#' @export
plotWithDownloadButtons <- function(input, output, ids, ggplot_obj, file_name, update_ggplot_func = NULL) {
  ids <- generate_ids(ids)

  reactive_ggplot_obj <- shiny::reactive({
    if (is.null(update_ggplot_func)) {
      ggplot_obj
    } else {
      update_ggplot_func(ggplot_obj, input$scaleType)
    }
  })

  output[[ids$plot_id]] <- plotly::renderPlotly(reactive_ggplot_obj()$plotly)

  output[[ids$download_plot_id]] <- shiny::downloadHandler(
    filename = function() paste0(file_name, "_plot.png"),
    content = function(file) ggplot2::ggsave(file, plot = reactive_ggplot_obj()$gg, bg = "white")
  )

  output[[ids$download_data_id]] <- shiny::downloadHandler(
    filename = function() paste0(file_name, "_data.csv"),
    content = function(file) utils::write.csv(reactive_ggplot_obj()$gg$data, file)
  )
}

#' Generate Shiny Element IDs
#'
#' @param id A character string representing the base ID.
#' @return A named list containing three elements: plot_id, download_plot_id, and download_data_id.
#' @export
generate_ids <- function(id) {
  if (!is.character(id) || length(id) != 1) {
    stop("id must be a single character string")
  }

  id <- tolower(gsub(" ", "", id))
  plot_id <- paste0("plot_", id)
  download_plot_id <- paste0("downloadplot_", id)
  download_data_id <- paste0("downloaddata_", id)

  list(plot_id = plot_id, download_plot_id = download_plot_id, download_data_id = download_data_id)
}

# Internal function from shiny.semantic which I copied to be able to change the mobile
# screen size definition to 1400 so that tablet related content is applied the change
# of moving the sidebar_panel to the top of the main_panel.
custom_grid_template <- function(default = NULL, mobile = NULL) {
  if (!("areas" %in% names(default))) {
    stop(paste(
      "grid_template() default argument must contain list with `areas` definition.",
      "See documentation for examples."
    ))
  }

  area_names <- unique(as.vector(default$areas))
  area_tags <- shiny::tagList(shiny.semantic:::list_of_area_tags(area_names))
  css_grid_template_areas <- shiny.semantic:::data_frame_to_css_grid_template_areas(default$areas)

  css_default <- shiny::tags$style(paste(
    "#{{ grid_id }} {",
    shiny.semantic:::grid_container_css(
      css_grid_template_areas, default$rows_height,
      default$cols_width
    ), "}"
  ))

  css_mobile <- NULL
  if (!is.null(mobile)) {
    if (!("areas" %in% names(mobile))) {
      stop(paste(
        "grid_template() mobile argument must contain list with `areas` definition.",
        "See documentation for examples."
      ))
    }
    css_grid_template_areas <- shiny.semantic:::data_frame_to_css_grid_template_areas(mobile$areas)

    css_mobile <- shiny::tags$style(paste(
      "@media screen and (max-width: 1400px) {",
      "#{{ grid_id }} {", shiny.semantic:::grid_container_css(
        css_grid_template_areas,
        mobile$rows_height, mobile$cols_width
      ), "}",
      "}"
    ))
  }

  template <- htmltools::renderTags(
    shiny::tagList(css_default, css_mobile, shiny::tags$div(
      id = "{{ grid_id }}",
      area_tags
    ))
  )
  return(list(template = template$html, area_names = area_names))
}
