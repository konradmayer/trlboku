% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-rwl.R
\name{avg_trees}
\alias{avg_trees}
\title{avg_trees}
\usage{
avg_trees(rwl, stc = c(3, 4, 1))
}
\arguments{
\item{rwl}{a data.frame/rwl object.}

\item{stc}{parameter as defined in \code{\link[dplR]{read.ids}}.}
}
\value{
a data frame/rwl object.
}
\description{
Averages series from multiple cores taken from the same tree/object.
}
\examples{
library("dplR")
data("gp.rwl")
avg_trees(gp.rwl, stc = c(0,2,1))
}
