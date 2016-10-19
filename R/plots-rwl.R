#' @title scatter_rwl
#' @description Produces a scatter plot matrix with rainbow colored points as
#'   well as a panel ob correlation bubbles (size corresponds to squared
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

  pairs(as.data.frame(rwl), col = scales::alpha(rainbow(nrow(rwl)), 0.3), upper.panel=panel.cor, ...)
}
