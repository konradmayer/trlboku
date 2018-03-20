#scatter_rwl--------------------------------------------------------------------
#' @title scatter_rwl
#' @description Produces a scatter plot matrix with rainbow colored points as
#'   well as a panel of correlation bubbles (size corresponds to squared
#'   correlation coef, blue positive, red negative)
#'
#' @param rwl a data frame (potentially containing tree ring series / chronologies).
#' @param ... additional plotting arguments (e.g. \code{pch = 19}).
#' @export
#'
#' @examples
#' library('dplR')
#' data('gp.rwl')
#' scatter_rwl(gp.rwl[1:6], main = 'example plot', pch=19)
scatter_rwl <- function(rwl, ...) {
  if(ncol(rwl) < 2) {
    stop('data.frame has to contain minimum two overlapping columns')
  }

  pairs(as.data.frame(rwl), col = scales::alpha(rainbow(nrow(rwl)), 0.3),
        upper.panel=panel.cor, ...)
}

# rwl_plot ------------------------------------------------------------------
#' @title rwl_plot
#' @description useful defaults for \code{\link[graphics]{matplot}} to plot a number of treering series
#' @param rwl an object of class dataframe /rwl
#' @param col character string indicating the color(s) tu use for the lines
#' @param ... otional further plotting arguments
#'
#' @return side effect - a plot
#' @export
rwl_plot <- function(rwl, col = 'black', ...) {
  matplot(rownames(rwl), rwl, type = 'l', lty = 1, col = col, xlab = 'year', xaxt = 'n', ...)
  axis(1, las = 2)
}



# quantile_crn_plot ------------------------------------------------------------

quantile_crn_plot <- function(rwl) {
  qu <- apply(rwl, 1, quantile, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  plot(NULL, xlim=range(as.numeric(rownames(rwl))), ylim=c(0, max(qu,na.rm=T)), type='l', lty=1, , xlab='year', ylab='median with quantiles', xaxt='n')
  abline(v=seq(1000, 3000, 10), col='gray60', lty=2)
  abline(v=seq(1000, 3000, 1), col='gray60', lty=3)
  axis(1,at=seq(0,3000,10), las=2)
  lines(colnames(qu), qu['50%',], lwd=3, col='darkgreen')
  polygon(c(colnames(qu), rev(colnames(qu))),c(qu['25%',],rev(qu['75%',])),border=F, col=scales::alpha('darkgreen',0.2))
}

