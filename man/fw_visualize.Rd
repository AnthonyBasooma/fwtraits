% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fw_vizualize.R
\name{fw_visualize}
\alias{fw_visualize}
\title{ggplot2 visualization.}
\usage{
fw_visualize(
  output,
  scales = "free",
  ncol = 2,
  params = NULL,
  organism_group = NULL,
  color = "purple",
  na.rm = TRUE
)
}
\arguments{
\item{output}{fetchdata output only accepted.}

\item{scales}{indicate if the scales are free, free_x, and free_y.}

\item{ncol}{Number of columns to display the data in facet_wrap.}

\item{params}{species ecological parameters selected if there are more than one.}

\item{organism_group}{Particular organism group to filter out to allow visualization.}

\item{color}{\code{string} Change the color for the bar graphs being plotted.}

\item{na.rm}{\code{logical} To remove NA in the category names or species traits.}
}
\value{
ggplot2 display
}
\description{
ggplot2 visualization.
}
