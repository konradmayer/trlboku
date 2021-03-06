% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{panel.hist}
\alias{panel.hist}
\title{Add a histogram to diagonal panel in pairs()}
\usage{
panel.hist(x, ...)
}
\arguments{
\item{x}{vector}

\item{...}{additional arguments}
}
\description{
This function can be used inside pairs() function to show
  histograms in the diagonal cells of the matrix.
}
\examples{
pairs(iris, diag.panel = panel.hist)
}
