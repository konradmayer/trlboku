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


# segment plot ------------------------------------------------------------

seg_plot <- function (rwl, col = 1, lwd = 4, cex = 1, xlab = '', order = c('first', 'last'), ...)
{
  if (!is.data.frame(rwl)) {
    stop("'rwl' must be a data.frame")
  }

  order <- match.arg(order)

  if(length(col) == 1) {col <- rep(col, ncol(rwl))}

  yr <- as.numeric(row.names(rwl))
  first.year <- as.matrix(apply(rwl, 2, dplR:::yr.range, yr.vec = yr))[1, ]
  last.year <- as.matrix(apply(rwl, 2, dplR:::yr.range, yr.vec = yr))[2, ]
  neworder1 <- order(first.year, decreasing = FALSE)
  segs <- rwl[, neworder1, drop = FALSE]

  if(order == 'last') {
    cols = col[neworder1]
  }
  if(order == 'first') {
    last.year <- as.matrix(apply(segs, 2, dplR:::yr.range, yr.vec = yr))[2, ]
    neworder2 <- order(last.year, decreasing = FALSE)
    segs <- segs[, neworder2, drop = FALSE]
    cols <- col[neworder1][neworder2]
  }

  n.col <- ncol(segs)
  seq.col <- seq_len(n.col)
  for (i in seq.col) {
    segs[[i]][!is.na(segs[[i]])] <- i
  }
  segs.axis2 <- names(segs)
  segs.axis4 <- names(segs)
  segs.axis2[seq(2, n.col, by = 2)] <- NA
  segs.axis4[seq(1, n.col, by = 2)] <- NA
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mar = c(2, 5, 2, 5) + 0.1, mgp = c(1.1, 0.1, 0), tcl = 0.5,
      xaxs = "i", yaxs = "i")
  plot(yr, segs[[1]], type = "n", ylim = c(0, n.col + 1),
       axes = FALSE, ylab = "", xlab = xlab, cex = cex,
       ...)
  abline(h = seq.col, lwd = 1, col = "grey")
  grid(ny = NA)
  # apply(segs, 2, lines, x = yr, lwd = lwd, lend = 2, col = col)
  purrr::map2(segs, cols, ~lines(.x, x = yr, lwd = lwd, lend = 2, col = .y))

  axis(2, at = seq.col, labels = segs.axis2,  tick = FALSE,
       las = 2, lwd = -1, cex.axis = cex)
  axis(4, at = seq.col, labels = segs.axis4,  tick = FALSE,
       las = 2, lwd = -1, cex.axis = cex)
  axis(1, lwd = -1, cex.axis = cex)
  axis(3, lwd = -1, cex.axis = cex)
  #box()
}

