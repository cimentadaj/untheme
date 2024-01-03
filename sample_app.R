library(untheme)
library(shiny)
library(shiny.semantic)
library(ggplot2)
library(plotly)

ui <- fluidUnTheme(
  tabset(
    tabs = list(
      create_tab("Tab 1", "plot1"),
      create_tab("Tab 2", "plot2"),
      create_tab("Tab 3", "plot3")
    )
  )
)

server <- function(input, output, session) {
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

  plots_tabset(
    list(plt_reactive = ggplot_obj1, filename = "plot1"),
    list(plt_reactive = ggplot_obj2, filename = "plot2"),
    list(plt_reactive = ggplot_obj3, filename = "plot3")
  )
}

shinyApp(ui, server)
