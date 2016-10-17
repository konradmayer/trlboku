#---------------------------
#custom barplots
#---------------------------

#' @title custom barplots
#' @description  A custom barplot separated in in one init function and two
#'   different barplot functions so it can easily be added to other plots or
#'   used in combination with points, lines and so on.
#'@describeIn init_barplot draws an empty plot for rect_barplot and
#'  lines_barplot
#' @param x,y double, x also character possible, if only one argument is provided
#'  it is taken as y and its names as x. If x can be converted to double it is
#'  used as x, if not it is only used as labels
#' @param include0 logic, if TRUE y axis includes origin
#' @param ylab a title for the y axis
#' @param xlab a title for the x axis
#' @param main an overall title for the plot
#' @param las numeric, see \code{\link[graphics]{par}}
#' @export
#'
#' @examples
#' x <- 10 + rnorm(40)
#' names(x) <- 1901:1940
#'
#' # rect_barplot:
#' init_barplot(x)
#' rect_barplot(x, width = 0.5)
#'
#' # lines_barplot:
#' init_barplot(x)
#' lines_barplot(x, lwd = 3)
init_barplot <- function(x = names(y), y, include0 = TRUE, ylab = '',
                         xlab = '', main = 'barplot', las = 2) {

  if(missing(y)) {
    y <- x
    x <- names(y)
  }

  #argument check
  if(!is.double(y)) {
    stop('y must be double')
  }

 #main function


  x2 <- x

  if(!all(!is.na(suppressWarnings(as.double(c(x)))))) {
    x2 <- seq_along(y)
  }

  xlim <- range(as.double(x2))
  ylim <- range(y)
  if(include0 == TRUE){ylim[1] <- 0}
  plot(NULL, xlim = xlim, ylim = ylim, main = main, xlab = xlab, ylab = ylab,
       xaxt = 'n')
  axis(1, at = x2, labels = x, las = las)

}

#' @param lwd line width as used in \code{\link[graphics]{lines}}
#' @param col bar color
#' @describeIn init_barplot is a plotting function based on \code{\link[graphics]{lines}}
#' @export
lines_barplot <- function(x = names(y), y, lwd = 3 , col = 'black'){

  #also only one input when named for user convenience
  if(missing(y)) {
    y <- x
    x <- names(y)
  }

  #argument check
  if(!is.double(y)) {
    stop('y must be double')
  }

  #main function

  x2 <- suppressWarnings(as.double(c(x)))
  if(!all(!is.na(x2))) {
    x2 <- seq_along(y)
  }

  tmp <- lapply(seq_along(y), FUN = function (z)
    lines(c(rep(x2[z], 2)), c(0, y[z]), lend = 3, lwd = lwd, col = col))
}


#' @param width specifies the box width (with = 1 leaves no space between the bars)
#' @describeIn init_barplot is a plotting function based on \code{\link[graphics]{rect}}
#' @export
rect_barplot <- function(x = names(y), y, width = 0.7, col = 'black'){
  #also only one input when named for user convenience
  if(missing(y)) {
    y <- x
    x <- names(y)
  }

  #argument check
  if(!is.double(y)) {
    stop('y must be double')
  }

  #main function

  wh <- width / 2

  x2 <- suppressWarnings(as.double(c(x)))
  if(!all(!is.na(x2))) {
    x2 <- seq_along(y)
  }

  tmp <- lapply(seq_along(y), FUN= function (z)
    rect(x2[z] - wh, 0, x2[z] + wh, y[z], col = col, border = NA))
}


