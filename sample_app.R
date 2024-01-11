devtools::load_all()
library(shiny)
library(shiny.semantic)
library(ggplot2)
library(plotly)

ui <- fluidUnTheme(
  selectInput("select_id", "Choose a plot:", choices = c("Tab 1", "Tab 2", "Tab 3", "Tab 4")),
  uiOutput("current_tab")
)

plt4 <-
  ggplot(mtcars, aes(x = mpg, y = disp, color = as.character(gear))) +
  geom_point() + # Scatter plot
  labs(
    title = "\nScatter Plot with Legend",
    x = "Value 1",
    y = "Value 2",
    color = "Category"
  ) +
  theme_minimal()

server <- function(input, output, session) {
  output$current_tab <- renderUI({
    switch(input$select_id,
      "Tab 1" = plotWithDownloadButtonsUI("Tab 1"),
      "Tab 2" = plotWithDownloadButtonsUI("Tab 2"),
      "Tab 3" = plotWithDownloadButtonsUI("Tab 3"),
      "Tab 4" = plotWithDownloadButtonsUI("Tab 4")
    )
  })

  data <- mtcars
  ggplot_obj1 <- reactive({
    gg <- ggplot(data, aes(x = mpg, y = wt)) + geom_point()
    list(gg = gg, plotly = ggplotly(gg))
  })

  ggplot_obj2 <- reactive({
    gg <- ggplot(data, aes(x = mpg, y = cyl)) + geom_point()
    list(gg = gg, plotly = ggplotly(gg))
  })

  ggplot_obj3 <- reactive({
    gg <- ggplot(data, aes(x = mpg, y = disp)) + geom_point()
    list(gg = gg, plotly = ggplotly(gg, tooltip = c("x","y")))
  })

  ggplot_obj4 <- reactive({
    list(
      gg = plt4,
      plotly = ggplotly(plt4, tooltip = c("x", "y")) %>%
        layout(legend = list(
          xanchor = "bottom",
          y = -0.22,
          xref = "container",
          yanchor = "bottom",
          orientation = "h"
        ))
    )
  })

  chosen_plt <-
    reactive({
      switch(
        input$select_id,
        "Tab 1" = list(plt_reactive = ggplot_obj1, filename = "plot1"),
        "Tab 2" = list(plt_reactive = ggplot_obj2, filename = "plot2"),
        "Tab 3" = list(plt_reactive = ggplot_obj3, filename = "plot3"),
        "Tab 4" = list(plt_reactive = ggplot_obj4, filename = "plot4")
      )
    })

  observe({
    input$select_id
    plots_tabset(
      input,
      output,
      input$select_id,
      chosen_plt()
    )
  })

}

shinyApp(ui, server)
