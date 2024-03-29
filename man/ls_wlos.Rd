% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz.R
\name{ls_wlos}
\alias{ls_wlos}
\title{A LandscapeR version of ParseR's Weighted Log-odds}
\usage{
ls_wlos(
  df,
  group_var = cluster,
  text_var = clean_text,
  top_n = 30,
  text_size = 4,
  nrow = 4,
  top_terms_cutoff = 5000
)
}
\arguments{
\item{df}{Data Frame or Tibble object}

\item{group_var}{The variable to group with e.g. topic, sentiment}

\item{text_var}{Your text variable}

\item{top_n}{Number of terms per plot}

\item{text_size}{An integer determining text size, higher = larger}

\item{nrow}{Number of rows to display the plots across}

\item{top_terms_cutoff}{The top x words which should have WLOs calculated for them}
}
\value{
a ggplot object
}
\description{
Function should be used for identifying the differences between levels of a
grouping variable.
}
