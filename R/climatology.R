
#dcc_spear----------------------------------------------------------------------
#' @title correlation analysis for tree ring chronologies and climate data
#' @description A function similar to the functionality of \code{\link[treeclim]{dcc}}; 
#'   without moving windows, but spearman correlations used.
#' @param crn A chronology - the first column is used by the function, make
#'   sure to give the right input (in case multiple chronologies are in one
#'   object, e.g. as for AR-chronologies).
#' @param clim climate data in "wide format" with years in the first
#'   column and monthly climate measurement in the consecutive 12 columns.
#' @param months an integer sequence selecting the end months of the windows to
#'   use. 1:5 selects jan to may of the "current" years as end months. to select
#'   months of the "previous" years use negative integers - c(-2:10) selects last
#'   years October to current years October as end months. Selection needs to be
#'   within c(-11:12).
#' @param method a character string, 'range', 'mean' or 'sum', selecting whether
#'   to calculate the correlation with individual months, or with means or sums
#'   over all (by argument month) selected months of the climate data.
#' @param replicates integer specifying the number of bootstrap replicates used
#'   to calculate the bootstrapped confidence interval.
#' @param conf the confidence level to be used for the
#'   \code{\link[stats]{cor.test}} (represented by column p_value in the output
#'   and the bootstrapped confidence interval computation).
#' @param ci.type type of confidence interval to be computed as defined as
#'   "type" in \code{\link[boot]{boot.ci}}.
#'
#' @return a data frame with following names:
##'   \describe{
##'    \item{coef}{spearmans rho}
##'    \item{p_value}{p-values from calling \code{\link[stats]{cor.test}} - that
##'      is without taking multiplicity into account}
##'    \item{ci_lower}{lower bound of bootstrapped confidence interval}
##'    \item{ci_upper}{upper bound of bootstrapped confidence interval}
##'   }
#' @export
#'
#' @examples #not available in development version
dcc_spear <- function(crn, clim, months = c(-2:10),
                      method = c('range', 'mean', 'sum'), replicates = 1000,
                      conf = 0.95, ci.type = "basic") {

    #argchecks:
  method <- method[1]
  if (!(method %in% c('range', 'mean', 'sum'))){
    stop('please choose "range", "sum" or "mean" as method')
  }

  if(!is.data.frame(crn) || !is.data.frame(clim)) {
    stop('crn and clim need to be data frames')
  }

  if(ncol(clim)!=13){
    stop('please supply clim with years in the first and montly climate data in the consecutive 12 columns')
  }

  if(!all(months >= (-11) && months[1] <= 12)) {
    stop('months need to be within -11:12')
  }

  if(!is.wholenumber(replicates) || (replicates < 1)) {
    stop('replicates must be a positive integer')
  }

  #pretreatment of climate data
  clim <- data.frame(clim[ , 2:13], row.names = clim[ , 1])
  clim_py <- clim
  rownames(clim_py) <- as.character(as.numeric(rownames(clim)) + 1)

  clim_pycy <- data.frame(clim_py[1:(nrow(clim_py) - 1), 1:12],
                          clim[2:nrow(clim), 1:12])
  names(clim_pycy) <- c(paste0(names(clim_py[1:12]), '-py'),
                        paste0(names(clim[1:12]), '-cy'))


  clim_pycy <- clim_pycy[ , 12 + months] #selection of months

  #calculation of sums or means of climare data - the correlations will be the
  #same for both methods
  if (method == 'mean') {
    nam <- colnames(clim_pycy)
    clim_pycy <- as.data.frame(rowMeans(clim_pycy))
    colnames(clim_pycy) <- paste0(method, ': ', nam[1], ' to ', nam[length(nam)])
  } else if (method == 'sum') {
    nam <- colnames(clim_pycy)
    clim_pycy <- as.data.frame(rowSums(clim_pycy))
    colnames(clim_pycy) <- paste0(method, ': ', nam[1], ' to ', nam[length(nam)])
  }

  #calculation of correlation
  years <- intersect(rownames(crn), rownames(clim_pycy))
  cor_list <- lapply(seq_along(clim_pycy),
                     FUN = function(x) cor.test(clim_pycy[years, x],
                                                crn[years, 1],
                                                method = c('spearman'),
                                                exact = FALSE,
                                                conf.level = conf))

  cor_spear <- sapply(cor_list, function(x) x$estimate)
  names(cor_spear) <- names(clim_pycy)
  p_spear <- sapply(cor_list, function(x) x$p.value)
  names(p_spear) <- names(clim_pycy)


  #bootstrapped confidence intervals
  boot_ci <- list()
  for(r in seq_along(clim_pycy)) {
    af <- data.frame(clim_pycy[years, r], crn[years, 1])

    getcor <- function(x, ndx) {
      return(cor(x[ndx, 1], x[ndx, 2], method = "spearman"))
    }

    boot_result <- boot::boot(af, getcor, R = replicates, stype = "i")

    boot_ci[[names(clim_pycy)[r]]] <- boot::boot.ci(boot_result, type = ci.type,
                                                    conf = conf)
  }

  boot_ci <- sapply(boot_ci, function(x) x[[4]][4:5])

  #output
  out <- data.frame(cbind(cor_spear, p_spear, t(boot_ci)))
  colnames(out) <- c('coef', 'p_value', 'ci_lower', 'ci_upper')
  class(out) <- c('dcc_spear', 'data.frame')

  return(out)
}

#plot.dcc_spear-----------------------------------------------------------------
#' @describeIn dcc_spear plot method for object of class "dcc_spear"
#' @param dcc_spear_object an output object from function dcc_spear()
#' @export
plot.dcc_spear <- function(dcc_spear_object, ...) {

  plot(NULL, xlim = c(0, nrow(dcc_spear_object) + 1), ylim = c(-1, 1),
       xaxt = 'n', xlab = '', ylab = 'Spearman Correlation', ...)
  grid()
  abline(h = 0, col = 'red')
  plotrix::plotCI(x = seq_len(nrow(dcc_spear_object)),
                  y = dcc_spear_object$coef,
                  li = dcc_spear_object$ci_lower,
                  ui = dcc_spear_object$ci_upper,
                  slty = 1, xaxt = 'n', pch = 19,
                  col = ifelse((dcc_spear_object$ci_lower *
                                  dcc_spear_object$ci_upper) > 0 ,
                               "darkblue", "red"),
                  add = T, lwd = 2, sfrac = 0.01)
  axis(1, at = seq_len(nrow(dcc_spear_object)),
  labels = rownames(dcc_spear_object),
  las = 3)
}



#accumulation_level-------------------------------------------------------------
#' @title Correlation between a chronology and climate data at different
#'   accumulation levels
#' @description The climate data is accumulated (eg. summed or averaged) over
#'   varying window sizes (from 1 to window size set with "accumulation")
#'   and correlations with a given chronology are calculated ("spearman" by
#'   default, but can be set).
#' @param crn A chronology - the first column is used by the function, make
#'   sure to give the right input (in case multiple chronologies are in one
#'   object, e.g. as for AR-chronologies).
#' @param climate.data climate data in "wide format" with years in the first
#'   column and monthly climate measurement in the consecutive 12 columns.
#' @param accumulation an integer specifying up to which window size
#'   accumulations get calculated.
#' @param accumfun function to use for accumulation of the climate measure
#'   within the window.
#' @param months an integer sequence selecting the end months of the windows to
#'   use. 1:5 selects jan to may of the "current" years as end months. to select
#'   months of the "previous" years use negative integers - c(-2:10) selects last
#'   years October to current years October as end months. Selection needs to be
#'   within c(-11:12).
#' @param method method to calculate correlations - "pearson", "kendall"
#'   or "spearman".
#'
#' @return a matrix with the same number of rows as selected aggregation levels
#'   (argument "aggregation") and columns as the selection of end-months.
#' @export
#'
#' @examples # not available in development version
accumulation_level <- function(crn, climate.data, accumulation, accumfun = sum,
                               months = c(-2:10), method = "spearman") {

  if(!is.data.frame(crn) || !is.data.frame(climate.data)) {
    stop('crn and climate.data need to be data frames')
  }

  if(ncol(climate.data)!=13){
    stop('please supply climate.data with years in the first and montly climate data in the consecutive 12 columns')
  }

  if(!all(months >= (-11) && months[1] <= 12)) {
    stop('months need to be within -11:12')
  }

  if(!method %in% c("pearson", "kendall", "spearman") || length(method) > 1){
    stop('method must be "pearson", "kendall" or "spearman"')
  }

  if(accumulation < 1) {
    stop('accumulation must be greater than or equal to 1')
  }

  climate_data_vec <- as.vector(t(climate.data[ , -1]))

  roll <- list()
  for (r in seq_len(accumulation)) {
    climate_data_roll <- c(rep(NA, r - 1), zoo::rollapply(climate_data_vec, r,
                                                          FUN = accumfun))
    out <- data.frame(matrix(climate_data_roll, nrow = nrow(climate.data),
                             ncol = 12, byrow = T),
                      row.names = climate.data[ , 1])
    names(out) <-  colnames(climate.data)[-1]

    out_py <- out
    rownames(out_py) <- as.character(as.numeric(rownames(out)) + 1)


    out_pycy <- data.frame(out_py[1:(nrow(out_py) - 1), ],
                           out[2:nrow(out), ])
    names(out_pycy) <- c(paste(names(out_py), '-py', sep = ''),
                         paste(names(out), '-cy', sep = ''))

    clim_cor <- out_pycy[, c(12 + months), drop = FALSE]

    years <- intersect(rownames(crn), rownames(out_pycy))
    roll[[r]] <- t(cor(clim_cor[years, ], crn[years, 1], method = method))
  }

  cat <- as.matrix(Reduce('rbind', roll))
  rownames(cat) <- seq_len(accumulation)
  class(cat) <- c('accumulation_level', 'matrix')
  cat
}

#plot.accumulation_level--------------------------------------------------------
#' @describeIn accumulation_level plot the output of accumulation_level()
#'   as contour plot.
#'
#' @param accumulation_level_object an object obtained by accumulation_level.
#' @param main a plot title

plot.accumulation_level <- function(accumulation_level_object,
                                    main = 'accumulation level') {

  cl <- colorRampPalette(c('darkred', 'white', 'darkblue')) (200)
  filled_contour(accumulation_level_object, axes = FALSE,
                 plot.axes = {
                   axis(1, at = plotrix::rescale(seq_len(
                     nrow(accumulation_level_object)), c(0, 1)),
                     labels = rownames(accumulation_level_object))
                   axis(2, at = plotrix::rescale(seq_len(
                     ncol(accumulation_level_object)), c(0, 1)),
                     labels = colnames(accumulation_level_object))
                 }, las = 1, zlim = c(-1, 1), col = cl, nlevels = 200,
                 main = main)
  axis(4, at = c(0, 0.25, 0.5, 0.75, 1),
       labels = c('-1', '.0,75', '0', '0.75', '1'), line = -2)
}
