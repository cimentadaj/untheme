all: README

README: README.Rmd
	R -e "devtools::build_readme()"

run_app:
	Rscript -e "devtools::load_all(); library(untheme); library(shiny); library(shiny.semantic); library(ggplot2); \
	ui <- fluidUnTheme( \
    plotOutput('bigPlot', height = '600px') \
	); \
	server <- function(input, output, session) { \
	  data <- mtcars; \
	  ggplot_obj <- ggplot(data, aes(x = mpg, y = wt)) + \
	    geom_point(); \
	  plotWithDownloadButtons(input, output, session, data, ggplot_obj); \
	}; \
	shinyApp(ui, server)"


clean:
	rm README.md
