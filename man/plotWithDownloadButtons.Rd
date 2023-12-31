% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/untheme.R
\name{plotWithDownloadButtons}
\alias{plotWithDownloadButtons}
\title{Create a server component for rendering and downloading a plot}
\usage{
plotWithDownloadButtons(
  input,
  output,
  ids,
  ggplot_obj,
  file_name,
  update_ggplot_func = NULL
)
}
\arguments{
\item{input}{A list of input values from the Shiny UI.}

\item{output}{A list of output values to be modified by this function.}

\item{ids}{A list of IDs generated for the plot and download buttons.}

\item{ggplot_obj}{A reactive expression returning a list with a named ggplot object and a named plotly plot.}

\item{file_name}{The file name to use for the plot download and the data.}

\item{update_ggplot_func}{An optional function for updating the ggplot object based on user input.}
}
\description{
Create a server component for rendering and downloading a plot
}
