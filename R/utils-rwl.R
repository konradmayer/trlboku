#yr_range-----------------------------------------------------------------------
#' @title yr_range
#' @description function yr_range from dplR, extracts the first and last year in
#'   yr.vec where the corresponding numeric vector shows a value different to NA
#' @param x a numeric vector
#' @param yr.vec a character- or numeric vector containing years with the same
#'   length as x
#' @return a character vector of length 2 containing the first and last year
#' @examples
#' library(dplR)
#' data(ca533)
#' yr_range(ca533[ ,1], rownames(ca533))
#' #[1] "1530" "1983"

yr_range <- function (x, yr.vec = as.numeric(names(x))) {
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

#first_last---------------------------------------------------------------------
#' @title first_last
#' @description function to return the first and the last year of a tree ring series
#' @param x a data.frame/rwl object
#'
#' @return a data.frame with series names in the first column as character strings,
#'   and the first as well as the last years of the series in the second resp.
#'   third column
#' @export
#' @examples
#' library('dplR')
#' data('gp.rwl')
#' first_last(gp.rwl)
first_last <- function(x) {

  if(!is.data.frame(x)){
    stop('please provide a data.frame/rwl object')
  }

  if(any(is.na(suppressWarnings(as.integer(rownames(x)))))){
    stop('please provide an input object with correct rownames')
  }

  tmp <- sapply(seq_along(x), FUN=function(i) {
    as.double(yr_range(x[ , i], as.numeric(rownames(x))))
  })
  out <- data.frame(names(x), t(tmp))
  names(out) <- c('series', 'first', 'last')
  out$series <- as.character(out$series) #not elegant but works
  return(out)
}

#series_length------------------------------------------------------------------
#' @title series length
#'
#' @param x a data.frame/rwl object
#'
#' @return a numeric vector
#' @export
series_length <- function(x) {
  sapply(x, FUN = function(y) length(na.omit(y)))
}

#truncate_rwl-------------------------------------------------------------------
#' @title truncate_rwl
#' @description This function removes lines at the beginning and end of a
#'   data.frame/rwl object only containing NA values (as present after subsetting etc.).
#' @param x a data.frame/rwl object
#'
#' @return a data.frame/rwl object
#' @export
#'
#' @examples
#' library("dplR")
#' data("gp.rwl")
#' subset <- gp.rwl[ ,1:10] #first value 1720, but rows start with 1570
#' truncate_rwl(subset) #rows start with 1720
truncate_rwl <- function(x) {

  if(!is.data.frame(x)) {
    stop('x must be of class data.frame')
  }

  while (all(is.na(x[1, , drop=FALSE]))) {
    x=x[-1, , drop=FALSE]
  }

  while (all(is.na(x[nrow(x), , drop=FALSE]))) {
    x=x[-nrow(x), , drop=FALSE]
  }
  return(x)
}


#expand_apply generic-----------------------------------------------------------
#' @title expand_apply
#' @description Apply a function on an expanding window.
#' @param x a numeric vector (NA is allowed and will be omitted)
#'   or a data.frame/rwl object
#' @param FUN character, name of a function e.g. 'median'.
#' @return The form of the value depends on the class of x. returns a vector
#'   for default method, a data.frame for data.frame method.
#' @export
#' @examples
#' #example for numeric method:
#' x <- c(NA, NA, 1, 3, 2, 3, NA)
#' expand_apply(x, 'median')

#' #example for data.frame method:
#' library('dplR')
#' data('ca533')
#' expand_apply(ca533, 'median')

expand_apply <- function(x, ...) UseMethod('expand_apply', x)

#expand_apply.numeric-----------------------------------------------------------
expand_apply.default <- expand_apply.numeric <- expand_apply.integer <- function(x, FUN = 'median') {
  if(!is.numeric(x)) {
    stop('x must be numeric or integer')
  }

  out <- sapply(seq_along(x), function(y) do.call(FUN, list(na.omit(x[seq_len(y)]))))
  out[is.na(x)] <- NA
  return(out)
}

#expand_apply.data.frame--------------------------------------------------------
expand_apply.data.frame <- function(x, FUN = 'median') {
  x[] <- lapply(x, function(y) trlboku:::expand_apply.numeric(as.vector(y), FUN))
  return(x)
}

#radius_rwl---------------------------------------------------------------------
#' @title radius_rwl
#' @description A function to get the cumulated tree ring widths
#'   (approx. radius of the tree in the given year).
#' @param rwl a rwl/data.frame object
#' @return a rwl/data.frame object with cumulated tree ring widths for each
#'   series (column)
#' @export
#' @examples
#' library("dplR")
#' data("gp.rwl")
#' radius_rwl(gp.rwl)
radius_rwl <- function(rwl) {

  if(!is.data.frame(rwl)) {
    stop('please provide input of class rwl or data.frame')
  }

  if(!all(apply(rwl, 2, is.numeric))) {
    stop('input contains non numeric values')
  }

  expand_apply.data.frame(rwl, 'sum')

}

#age_rwl------------------------------------------------------------------------
#' @title age_rwl
#' @description A function returning an rwl/data.frame object with the same
#'   dimensions as rwl with showing the cambial age instead of tree ring width
#'   in the corresponding year.
#' @param rwl a rwl/data.frame object
#' @return a rwl/data.frame object with cambial age of the years of the series.
#' @export
#' @examples
#' library("dplR")
#' data("gp.rwl")
#' age_rwl(gp.rwl)
age_rwl <- function(rwl) {

  if(!is.data.frame(rwl)) {
    stop('please provide input of class rwl or data.frame')
  }

  if(!all(apply(rwl, 2, is.numeric))){
    stop('input contains non numeric values')
  }

  expand_apply.data.frame(rwl, 'length')
}

#avg_trees----------------------------------------------------------------------
#' @title avg_trees
#' @description Averages series from multiple cores taken from the same tree/object.
#' @param rwl a data.frame/rwl object.
#' @param stc parameter as defined in \code{\link[dplR]{read.ids}}.
#' @return a data frame/rwl object.
#' @export
#'
#' @examples
#' library("dplR")
#' data("gp.rwl")
#' avg_trees(gp.rwl, stc = c(0,2,1))
avg_trees <- function(rwl, stc = c(3, 4, 1)) {
  #argument checks:
  if(!any(class(rwl) %in% c('rwl', 'data.frame'))) {
    stop('please provide an object of class rwl or data.frame')
  }

  if (!(length(stc) == 3 && is.numeric(stc))) {
    stop('argument stc has to be numeric of length 3')
  }

  ids <- dplR::read.ids(rwl, stc = stc)
  treeIds <- ids$tree
  unique.trees <- unique(treeIds)
  n.trees <- length(unique.trees)
  cores.of.tree <- list()
  seq.tree <- seq_along(unique.trees)
  for (i in seq.tree) {
    cores.of.tree[[i]] <- which(treeIds == unique.trees[i])
  }
  rwl.2 <- data.frame(matrix(data = as.numeric(NA), nrow = nrow(rwl), ncol = n.trees,
                             dimnames = list(rownames(rwl))))
  for (i in seq.tree) {
    these.cols <- cores.of.tree[[i]]
    tmp <- rowMeans(rwl[, these.cols, drop=FALSE], na.rm=TRUE)
    tmp[is.nan(tmp)] <- NA
    rwl.2[ , i] <- tmp
  }
  names(rwl.2) <- unique(substr(names(rwl), 1, stc[1] + stc[2]))
  class(rwl.2) <- c('rwl', 'data.frame')
  return(rwl.2)
}
