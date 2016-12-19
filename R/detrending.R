#detrend_given_rc---------------------------------------------------------------
detrend_given_rc <- function(rwl, rc, po) {

  if(!is.data.frame(rwl)) {
    stop('rwl must be of class data.frame')
  }

  if(!(is.data.frame(po))) {
    stop('po must be of class data.frame')
  }

  if(!setequal(po[ , 1], names(rwl))) {
    stop('series names in po are not the same as provided in rwl')
  }

  if(!is.numeric(rc)){
    stop('rc must be a numeric vector')
  }

  if(length(na.omit(rc)) < max(series_length(rwl))) {
    greater <- series_length(rwl) > length(na.omit(rc))
    warning(paste0('rc is shorter than series: ', paste0(names(rwl)[greater],
                  collapse = ', ')))
  }

  n.col <- ncol(rwl)
  col.names <- names(rwl)
  seq.cols <- seq_len(n.col)
  rwl2 <- rwl
  rownames(rwl2) <- rownames(rwl2)
  rwl.ord <- apply(rwl2, 2, sort_by_index)
  rwca <- matrix(NA, ncol = n.col, nrow = sum(nrow(rwl.ord) + max(po[ , 2])))
  nrow.m1 <- nrow(rwl.ord) - 1
  for (i in seq.cols) {
    yrs2pith <- po[po[ , 1] %in% col.names[i], 2]
    rwca[yrs2pith:(yrs2pith + nrow.m1), i] <- rwl.ord[ , i]
  }

  rwica <- rwca / rc
  rwi <- rwl2
  yrs <- as.numeric(row.names(rwl2))
  for (i in seq.cols) {
    series.yrs <- as.numeric(range(rownames(rwl2)[!is.na(rwl2[[i]])]))
    first <- series.yrs[1]
    last <- series.yrs[2]
    tmp <- na.omit(rwica[ , i])
    if (first + length(tmp) != last + 1) {
      warning(paste("indexing problem when restoring to cal years: first+length(tmp) != last+1", "problem in interation", i, "rc curve probably shorter than series",sep=" "))
    }
    rwi[[i]][yrs %in% first:last] <- tmp
  }
  return(rwi)
}


#sf_rcs-------------------------------------------------------------------------
#' @title signal free rcs
#' @description signal free rcs detrending as described by
#'   \href{http://dx.doi.org/10.1016/j.dendro.2013.06.002}{Melvin (2013)} using
#'   parts of the code of \code{\link[dplR]{rcs}} as well as the whole function
#'   internally.
#' @param rwl a rwl/data.frame object
#' @param po a data frame containing series names in the first and po
#'   data as nr. of years in the second column.
#' @param maxit maximal number of iterations to take - the iterations stop if
#'   the all values of the SF residual chronology are below 0.002 as suggested
#'   by \href{http://dx.doi.org/10.1016/j.dendro.2013.06.002}{Melvin (2013)} or
#'   reach maxit - default to \code{Inf}
#' @references
#'   Thomas M. Melvin, Keith R. Briffa, CRUST: Software for the
#'   implementation of Regional Chronology Standardisation: Part 1. Signal-Free
#'   RCS, Dendrochronologia, Volume 32, Issue 1, 2014, Pages 7-20, ISSN 1125-7865,
#'   \url{http://dx.doi.org/10.1016/j.dendro.2013.06.002}.
#'
#'   Thomas M. Melvin, Keith R. Briffa, A “signal-free” approach to
#'   dendroclimatic standardisation, Dendrochronologia, Volume 26, Issue 2,
#'   1 October 2008, Pages 71-86, ISSN 1125-7865,
#'   \url{http://dx.doi.org/10.1016/j.dendro.2007.12.001}.
#' @return a list containing:
#'   \describe{
#'     \item{rwi}{the indexed tree ring data as data.frame}
#'     \item{rc}{the final rc curve as numeric vector}
#'   }
#' @export
#'
#' @examples #not available in development version
sf_rcs <- function (rwl, po, maxit = Inf) {

  if(!is.data.frame(rwl)) {
    stop('rwl must be of class data.frame')
  }

  if(!(is.data.frame(po))) {
    stop('po must be of class data.frame')
  }

  if(!setequal(po[ , 1], names(rwl))) {
    stop('series names in po are not the same as provided in rwl')
  }

  if(!is.numeric(maxit) || length(maxit) != 1) {
    stop('maxit must be an integer')
  }

  sf.m <- rwl
  sf.crn <- data.frame(sf.chron = rep(1, nrow(rwl)))
  rownames(sf.crn) <- rownames(rwl)
  crn <- data.frame(chron = rep(1, nrow(rwl)))
  rownames(crn) <- rownames(rwl)

  res <- 1
  it <- 0

  rc.curves <- list()
  while ((!(all(na.omit(res < 0.002)))) & (it < maxit)){

    sf.m <- sf.m / sf.crn[ , 1]
    sf.rcs <- dplR::rcs(sf.m, po, biweight = FALSE, rc.out = TRUE,
                        make.plot = FALSE)
    sf.crn <- chron(sf.rcs$rwi, biweight=FALSE)
    res <- abs(1 - sf.crn[ , 1])
    crn <- crn * sf.crn[ , 1]
    it <- it + 1
    rc.curves[[it]] <- sf.rcs$rc
  }

  if(it == maxit){warning('max iterations reached')}

  rc <- rc.curves[[it]]
  rwi <- detrend_given_rc(rwl = rwl, rc = rc, po = po)

  list('rwi' = rwi, 'rc' = rc)
}
