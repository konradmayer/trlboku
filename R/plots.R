# plot_species_table -----------------------------------------------------------
#' @title Plot species table
#' @description  Plots the information contained in the species dataset.
#' @return nothing, produces a side effect.
#' @export
#'
#' @examples
#' plot_species_table()
#'

plot_species_table <- function(variables) {
  plot(gridExtra::tableGrob(species,
                            theme = gridExtra::ttheme_default(
                              core = list(
                                bg_params = list(fill = species$color)
                              )
                            )
  )
  )
}


#custom barplots----------------------------------------------------------------
#' @title custom barplots
#' @description  A custom barplot separated into one init function and two
#'   different barplot functions so it can easily be added to other plots or
#'   used in combination with points, lines and so on.
#'@describeIn init_barplot draws an empty plot for rect_barplot and
#'  lines_barplot
#' @param x,y double, for x also a character vector is allowed, if only one argument
#'  is provided it is taken as y and its names as x. If x can be converted to double it is
#'  used as x, if not it is only used as labels.
#' @param xlim,ylim limits for both axes; if \code{NULL} they will be calculated
#'   by the function. if \code{include0 == TRUE}, ylim[1] will be overwritten
#'   with 0.
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
init_barplot <- function(x = names(y), y, xlim = NULL, ylim = NULL,
                         include0 = TRUE, ylab = '', xlab = '',
                         main = 'barplot', las = 2) {

  if(missing(y)) {
    y <- x
    x <- names(y)
  }

  #argument check
  if(!is.numeric(y)) {
    stop('y must be numeric')
  }

  #main function


  x2 <- x

  if(!all(!is.na(suppressWarnings(as.double(c(x)))))) {
    x2 <- seq_along(y)
  }

  if(is.null(xlim)){
    xlim <- range(as.double(x2), na.rm = TRUE)
  }

  if(is.null(ylim)){
    ylim <- range(y, na.rm = TRUE)
  }

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

#error_bar----------------------------------------------------------------------
#' @title error bar
#' @description add a single error bar to a plot
#' @param x x coordinate
#' @param y y coordinate - a point gets drawn there
#' @param ll lower limit - position of the lower whisker
#' @param ul upper limit - position of the upper whisker
#' @param width width of the whisker ends
#' @export
#'
#' @examples
#' #single:
#' plot(NULL, xlim = c(0, 2), ylim = c(0, 5))
#' error_bar(1, 2.5, 1, 4.5, 1)
#'
#' #multiple:
#' percentile <- do.call('rbind', lapply(list(c(1:4), c(2:5), c(3:6)), quantile))
#' plot(NULL, xlim = c(0, 4), ylim = c(0, 7))
#' lapply(seq_len(nrow(percentile)),
#'        FUN = function(x) error_bar(x,
#'                                    percentile[x, 3],
#'                                    percentile[x, 2],
#'                                    percentile[x, 4],
#'                                    width = 0.5))

error_bar <- function(x, y, ll, ul, width) {
  w <- width/2
  lines(rep(x, 2), c(ll, ul))
  points(x, y, pch = 20)
  lines(c(x - w, x + w), rep(ul, 2))
  lines(c(x - w, x + w), rep(ll, 2))
}

#panel.hist---------------------------------------------------------------------
#' @title Add a histogram to diagonal panel in pairs()
#' @description This function can be used inside pairs() function to show
#'   histograms in the diagonal cells of the matrix.
#' @param x vector
#' @param ... additional arguments
#' @export
#' @examples
#' pairs(iris, diag.panel = panel.hist)
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "gray80", ...)
}
