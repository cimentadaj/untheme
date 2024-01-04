devtools::load_all()
library(shiny)
library(shiny.semantic)
library(ggplot2)
library(plotly)

ui <- fluidUnTheme(
  selectInput("select_id", "Choose a plot:", choices = c("Tab 1", "Tab 2", "Tab 3")),
  uiOutput("current_tab")
)

server <- function(input, output, session) {
  output$current_tab <- renderUI({
    switch(input$select_id,
      "Tab 1" = plotWithDownloadButtonsUI("Tab 1"),
      "Tab 2" = plotWithDownloadButtonsUI("Tab 2"),
      "Tab 3" = plotWithDownloadButtonsUI("Tab 3"),
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
    list(gg = gg, plotly = ggplotly(gg))
  })


  chosen_plt <-
    reactive({
      switch(
        input$select_id,
        "Tab 1" = list(plt_reactive = ggplot_obj1, filename = "plot1"),
        "Tab 2" = list(plt_reactive = ggplot_obj2, filename = "plot2"),
        "Tab 3" = list(plt_reactive = ggplot_obj3, filename = "plot3"),
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
