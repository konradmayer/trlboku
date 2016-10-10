#--------------------------
#sortByIndex
#--------------------------
#' @title sortByIndex
#' @description sortByIndex from package dplR, shifts series to start with index 1,
#'   maintaining the same vector length by adding NA values to the end
#' @param x a numeric vector, representing an individual rwl series,
#'   potentially containing NA values.
#'
#' @return a numeric vector with the same length as x.
#' @examples
#' x <- c(NA,NA,NA,1,2,3,4,5, NA, NA)
#' sortByIndex(x)
#' #[1]  1  2  3  4  5 NA NA NA NA NA
sortByIndex <- function (x)
{
  lowerBound <- which.min(is.na(x))
  c(x[lowerBound:length(x)], rep(NA, lowerBound - 1))
}

#--------------------------
##yr.range
#--------------------------
#' @title yr.range
#' @description function yr.range from dplR, extracts the first and last year in
#'   yr.vec where the corresponding numeric vector shows a value different to NA
#' @param x a numeric vector
#' @param yr.vec a character- or numeric vector containing years with the same
#'   length as x
#' @return a character vector of length 2 containing the first and last year
#' @examples
#' library(dplR)
#' data(ca533)
#' yr.range(ca533[ ,1], rownames(ca533))
#' #[1] "1530" "1983"

yr.range <- function (x, yr.vec = as.numeric(names(x)))
{
  na.flag <- is.na(x)
  if (all(na.flag)) {
    res <- rep(NA, 2)
    mode(res) <- mode(yr.vec)
    res
  }
  else {
    range(yr.vec[!na.flag])
  }
}


