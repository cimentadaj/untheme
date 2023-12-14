
<!-- README.md is generated from README.Rmd. Please edit that file -->

# untheme

<!-- badges: start -->
<!-- badges: end -->

The goal of untheme is to …

## Installation

You can install the development version of untheme from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cimentadaj/untheme")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(untheme)
library(shiny)
library(shiny.semantic)
library(ggplot2)

ui <- fluidUnTheme(
  tabset(
    tabs = list(
      list(
        menu = "Tab 1",
        content = plotWithDownloadButtonsUI("plot1"),
        id = "first_tab"
      )
    )
  )
)

server <- function(input, output, session) {
  data <- mtcars
  ggplot_obj <- ggplot(data, aes(x = mpg, y = wt)) +
    geom_point()

  plotWithDownloadButtons(input, output, session, data, ggplot_obj)
}

shinyApp(ui, server)
```
