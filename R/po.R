#po_transform-------------------------------------------------------------------
#' @title po_transform
#' @description Estimates no. of missing rings by estimated distance to pith.
#'
#' @param po \code{data.frame} of measured distance po (in the same unit as rwl!)
#'   with series names in the first column and po in the second.
#' @param rwl rwl object containing series measured.
#' @param nyrs the first \code{1:nyrs} years will be used to calculate the mean growth
#'   rate, default is 4.
#'
#' @return A \code{data.frame} just as the input "po" with po as no. of tree rings.
#' @export
#' @examples no examples available in the development version

po_transform <- function(po, rwl, nyrs = 4) {

  #checking arguments:
  if(!(is.data.frame(po) && is.data.frame(rwl))) {
    stop('po and rwl must be data frames')
  }

  if(!setequal(po[ , 1], names(rwl))) {
    stop('series names in po are not the same as provided in rwl')
  }

  for (p in as.character(po[ , 1])) {
    meanrw <- mean(na.omit(rwl[p])[seq_len(nyrs), ])
    po$meanrw[po$series == p] <- meanrw
  }

  po$po.new <- round(po[ , 2] / po$meanrw) + 1
  out <- po[ , c(1,4)]
  out[ , 1] <- as.character(out[ , 1])
  out[ , 2] <- as.integer(out[ , 2])
  return(out)
}

#po_find------------------------------------------------------------------------
#' @title po_find
#' @description Estimates pith offset of series by finding the position with
#'   minimum RSS to existing regional curve.
#' @param rwl rwl/data.frame object containing the tree ring series.
#' @param rc existing "regional curve".
#' @param maxpo maximal po until which RSS gets calculated.
#' @param nyrs setting for the ffcsaps function used to smooth the series:
#'   a number greater than 1, affecting the rigidity of the
#'   spline.  When \code{\var{f}} is kept constant, a larger
#'  \code{\var{nyrs}} produces a more rigid spline.  Defaults to
#'  \code{length(\var{y})/2}.
#' @param f  setting for the ffcsaps function used to smooth the series:
#'   a number between 0 and 1 giving the frequency response at a
#'   wavelength of \code{\var{nyrs}} years.  When \code{\var{nyrs}} is
#'   kept constant, a smaller \code{\var{f}} produces a more rigid
#'   spline: At one extreme, \code{\var{f} = 0} causes the function to
#'   return the least-squares straight line fit to the data.  At the
#'   other extreme, \code{\var{f} = 1} results in the natural spline,
#'   i.e. the function outputs \code{\var{y}}.  The default value is
#'   0.5.
#' @param make.plot a \code{logical} indicating if a plot should be drawn.
#'
#' @return a \code{data.frane} with the columns "series" and "po", containing
#'   the series names and po estimations.
#' @export
#' @examples #no examples added in the current development version - will be
#'   added in future

po_find <- function(rwl, rc, maxpo = NULL, min.overlap = 10, nyrs = NULL, f = 0.5, make.plot = TRUE){
  #argument checks:

  if(!is.data.frame(rwl)) {
    stop('rwl must be a data.frame')
  }

  if (!(is.numeric(rc) && length(rc) > 1)) {
    stop('rc needs to be a numeric vector')
  }

  rc <- na.omit(rc)
  names(rc) <- 1:length(rc)

  outdf <- data.frame(rwl = names(rwl), po = rep(NA, ncol(rwl)))

  for (p in seq_along(rwl)) {
    prof <- na.omit(rwl[ , p])
    nyr <- length(prof)

    if(is.null(nyrs)) {
      nyrs <- nyr / 2
    }

    profspline <- dplR::ffcsaps(prof, nyrs = nyrs, f = f)
    names(profspline) <- seq_along(profspline)

    if(is.null(maxpo)) {
      maxpo <- (length(rc) - 1)
    }

    out <- c()

    for (s in seq_len(maxpo)-1) {
      names(profspline) <- (seq_along(profspline)) + s
      is <- intersect(names(rc), names(profspline))
      if(length(is) > min.overlap) {
        # lines(names(profspline),profspline, col=s+2)
        res <- rc[is] - profspline[is]
        residsq <- sum(res ^ 2)
        out[s + 1] <- residsq #growing vector is bad practise but this function is not optimized as its rarely used
      }
    }

    po.new <- min(which(out == min(out)))
    if(make.plot == TRUE) {
      plot(rc, type = 'l', lwd = 3, main = paste0(names(rwl)[p], ' - new po'))
      lines(po.new:(length(profspline) + po.new - 1), profspline,
            col = 'orange', lwd = 2)
    }
    outdf[p, 2] <- po.new
  }
  return(outdf)
}

#sort_by_index------------------------------------------------------------------
#' @title sort_by_index
#' @description internal function sortByIndex from package dplR, shifts
#'   series to start with index 1, maintaining the same vector length by
#'   adding NA values to the end. Code copied and not imported to make function
#'   name consistent to naming convention in package trlboku and to provide
#'   documentation.
#' @param x a numeric vector, representing an individual rwl series,
#'   potentially containing NA values.
#' @export
#' @return a numeric vector with the same length as x.
#' @examples
#' x <- c(NA,NA,NA,1,2,3,4,5, NA, NA)
#' sort_by_index(x)
#' #[1]  1  2  3  4  5 NA NA NA NA NA
sort_by_index <- function (x) {
  lowerBound <- which.min(is.na(x))
  c(x[lowerBound:length(x)], rep(NA, lowerBound - 1))
}

#to_cambial_age-----------------------------------------------------------------
#' @title to_cambial_age
#' @description This function aligns tree ring series to match their cambial ages,
#'   taking pith offset into account if provided.
#' @param rwl a data frame/rwl object.
#' @param po optional, a data frame containing series names in the first and po
#'   data as nr. of years in the second column.
#'
#' @return A data.frame with aligned series
#' @export
#'
#' @examples
#' library("dplR")
#' data("gp.rwl")
#' data("gp.po")
#' gp.po$series <- as.character(gp.po$series)
#' to_cambial_age(gp.rwl, gp.po)
to_cambial_age <- function(rwl, po = NULL) {
  #check arguments

  if(!is.data.frame(rwl)) {
    stop('rwl must be of class data.frame')
  }

  if(!(is.data.frame(po) || is.null(po))) {
    stop('po must be of class data.frame or NULL')
  }

  if (is.null(po)) {
    po <- data.frame(series = names(rwl), po = 1)
  }


  if(!(nrow(po) > 2 && is.numeric(po[, 2]))) {
    stop('please provide a po object with two columns and pith offset as numeric
         values (cambial age of innermost ring) in the second column')
  }

  if(!all(po[, 2] > 0)) {stop('please provide po as cambial age of the first ring -
    this has to be > 0')}

  if(!setequal(po[ , 1], names(rwl))) {
    stop('series names in po are not the same as provided in rwl')
  }

 #execute function
  po.ordered <- po[po[ , 1] %in% names(rwl), ]

  lengths <- (series_length(rwl) + po[ , 2]) - 1
  rows <- max(lengths)
  out <- data.frame(matrix(NA, ncol = length(rwl), nrow = rows))
  for (i in seq_along(rwl)){
    start <- (po.ordered[i,2])
    out[start:lengths[i], i] <- na.omit(rwl[ , i])
  }
  names(out) <- names(rwl)
  return(out)
}

