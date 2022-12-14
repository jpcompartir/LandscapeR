% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ls_plot_variation_matrix.R
\name{ls_plot_variation_matrix}
\alias{ls_plot_variation_matrix}
\title{Compare proportion of x in y}
\usage{
ls_plot_variation_matrix(df, x_var, y_var)
}
\arguments{
\item{df}{Data Frame or Tibble object}

\item{x_var}{The grouping variable for the x axis.}

\item{y_var}{The grouping variable for the y axis.}
}
\value{
a ggplot object
}
\description{
Use this function to compare two grouping variables, for example to see what percentage of each cluster fall into each sentiment category (by volume).
}
\examples{
ls_example \%>\% ls_plot_variation_matrix(cluster, sentiment)
}
