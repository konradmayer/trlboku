#--------------------------
#yr.range
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

#--------------------------
#first_last
#--------------------------
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
first_last <- function(x){

  if(!is.data.frame(x)){
    stop('please provide a data.frame/rwl object')
  }

  tmp <- sapply(seq_along(x), FUN=function(i) as.double(yr.range(gp.rwl[ , i],
                                                                 rownames(x))))
  out <- data.frame(names(x), t(tmp))
  names(out) <- c('series', 'first', 'last')
  out$series <- as.character(out$series) #not elegant but works
  return(out)
}

#--------------------------
#series_length
#--------------------------
series_length <- function(x){
sapply(x, FUN = function(y) length(na.omit(y)))
}
#--------------------------
#truncate_rwl
#--------------------------
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
truncate_rwl <- function(x){

  while (all(is.na(x[1, ]))){
    x=x[-1, ]
  }

  while (all(is.na(x[nrow(x), ]))){
    x=x[-nrow(x), ]
  }
  return(x)
}

################################HIER WEITER

#--------------------------
#avg_trees
#--------------------------
#' @title avg_trees
#' @description Averages series from multiple cores taken from the same tree/object.
#' @param rwl a data.frame/rwl object.
#' @param stc parameter as defined in \code{\link[dplR]{read.ids}}.
#'
#' @return a data frame/rwl object.
#' @export
#'
#' @examples
#' library("dplR")
#' data("gp.rwl")
#' avg_trees(gp.rwl, stc = c(0,2,1))
avg_trees <- function(rwl, stc = c(3, 4, 1)){
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
  names(rwl.2) <- unique(substr(names(rwl), stc[1] + 1, stc[2]))
  class(rwl.2) <- class(rwl)
  return(rwl.2)
}
