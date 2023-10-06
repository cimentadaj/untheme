#' @export
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

# UI for the custom module
# UI for the custom module
plotWithDownloadButtonsUI <- function(id, radio_choices = NULL) {
  ns <- NS(id)

  button <- NULL

  if (!is.null(radio_choices)) {
    button <- multiple_radio(ns("scaleType"), "Scale Type", choices = radio_choices, type = "inline")
  }

  layout <-
    sidebar_layout(
      sidebar_panel(
        button,
        br(),
        downloadButton(ns("downloadPlot"), "Download Plot"),
        downloadButton(ns("downloadData"), "Download Data")
      ),
      main_panel(
        plotOutput(ns("plot"), height = "600px", width = "900px"),
        width = 4
      )
    )

  layout
}

# Server logic for the custom module
plotWithDownloadButtons <- function(input, output, session, data, ggplot_obj, update_ggplot_func = NULL) {
  reactive_ggplot_obj <- reactive({
    if (is.null(update_ggplot_func)) {
      ggplot_obj
    } else {
      update_ggplot_func(ggplot_obj, input$scaleType)
    }
  })

  output$plot <- renderPlot({
    print(reactive_ggplot_obj())
  })

  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot.png")
    },
    content = function(file) {
      ggsave(file, plot = reactive_ggplot_obj())
    }
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data.csv")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
}

ui <- fluidUnTheme(
  tabset(
    tabs = list(
      list(menu = "Tab 1", content = plotWithDownloadButtonsUI("plot1"), id = "first_tab"),
      list(menu = "Tab 2", content = plotWithDownloadButtonsUI("plot2"), id = "second_tab"),
      list(menu = "Tab 3", content = plotWithDownloadButtonsUI("plot3", radio_choices = c("Absolute", "Percentage")), id = "third_tab")
    )
  )
)

server <- function(input, output, session) {
  data1 <- mtcars
  plot1 <- ggplot(data1, aes(x = mpg, y = wt)) +
    geom_point()

  data2 <- iris
  plot2 <- ggplot(data2, aes(x = Sepal.Length, y = Sepal.Width)) +
    geom_point()

  data3 <- mtcars
  plot3 <- ggplot(data3, aes(x = mpg, y = wt)) +
    geom_point()

  update_ggplot_func <- function(ggplot_obj, scale_type) {
    if (scale_type == "Percentage") {
      ggplot_obj + scale_y_continuous(labels = scales::percent)
    } else {
      ggplot_obj
    }
  }

  callModule(plotWithDownloadButtons, "plot1", data = data1, ggplot_obj = plot1)
  callModule(plotWithDownloadButtons, "plot2", data = data2, ggplot_obj = plot2)
  callModule(plotWithDownloadButtons, "plot3", data = data3, ggplot_obj = plot3, update_ggplot_func = update_ggplot_func)
}

shinyApp(ui, server)
