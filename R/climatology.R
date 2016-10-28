#-------------------------
#accumulation_level
#-------------------------

#' @title Correlation of a chronology at different accumulation levels
#' @description The climate data is accumulated (eg. summed or averaged) over
#'   variing window sizes (from 1 to window size set with "accumulation")
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
#'   years october to current years october as end months. Selection needs to be
#'   within c(-11:12).
#' @param method method to calculate correlations - "pearson", "kendall"
#'   or "spearman".
#'
#' @return a matrix with the same number of rows as selected aggregation levels
#'   (argument "aggregation") and colums as the selection of endmonths.
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

#-------------------------
#plot.accumulation_level
#-------------------------
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
