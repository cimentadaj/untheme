# File for testing currently being called from a Makefile

devtools::load_all()
library(shiny)
library(shiny.semantic)

ui <- fluidUnTheme(
  titlePanel("Hello Shiny!"),
  sidebar_layout(
    sidebar_panel(
      sliderInput("obs", "Number of observations:", min = 0, max = 1000, value = 500),
      width = 3
    ),
    main_panel(
      tags$head(
        tags$style(HTML("
          /* Scrollable tab container */
          .scrollable-tabs {
            overflow-x: auto;
            white-space: nowrap;
          }
          /* Individual tabs */
          .scrollable-tabs .item {
            display: inline-block;
            float: none;
          }
          /* Minimum width for plot container */
          #plotContainer {
            min-width: 1200px; /* Adjust this value as needed */
          }
        "))
      ),
      div(class = "scrollable-tabs", # Add this class
        tabset(
          list(
            list(menu = "Some tab1", content = div(id = "plotContainer", plotOutput("distPlot"))),
            list(menu = "Some tab1", content = plotOutput("distPlot")),
            list(menu = "Some tab2", content = plotOutput("distPlot"))
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs))
  })
}

shinyApp(ui, server)
