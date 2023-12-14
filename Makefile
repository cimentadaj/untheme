all: README

README: README.Rmd
	R -e "devtools::build_readme()"

run_app:
	Rscript -e "library(untheme); library(shiny); library(shiny.semantic); library(ggplot2); \
	ui <- fluidUnTheme( \
	  tabset( \
	    tabs = list( \
	      list( \
	        menu = 'Tab 1', \
	        content = plotWithDownloadButtonsUI('plot1'), \
	        id = 'first_tab' \
	      ) \
	    ) \
	  ) \
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
