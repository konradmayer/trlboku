#panel.cor----------------------------------------------------------------------
#' @title panel.cor
#' @description adds a bubble plot panel to a plot produced by pairs()
#' @param w variable
#' @param z variable
#' @param ... additional arguments
#' @export
#'
#' @examples
#' pairs(mtcars, upper.panel=panel.cor)
panel.cor <- function(w, z, ...) {

  colorRange <- c('#69091e', '#e37f65', 'white', '#aed2e6', '#042f60')
  ## colorRamp() returns a function which takes as an argument a number
  ## on [0,1] and returns a color in the gradient in colorRange
  myColorRampFunc <- colorRamp(colorRange)

  correlation <- cor(w, z, use="complete.obs") #here maybe add an "use" argument for data sets with missing values

  ## because the func needs [0,1] and cor gives [-1,1], we need to
  ## shift and scale it
  col <- rgb(myColorRampFunc((1 + correlation) / 2) / 255)

  ## square it to avoid visual bias due to "area vs diameter"
  radius <- sqrt(abs(correlation))
  radians <- seq(0, 2 * pi, len = 50)     # 50 is arbitrary
  x <- radius * cos(radians)
  y <- radius * sin(radians)
  ## make them full loops
  x <- c(x, tail(x,n=1))
  y <- c(y, tail(y,n=1))

  ## I trick the "don't create a new plot" thing by following the
  ## advice here: http://www.r-bloggers.com/multiple-y-axis-in-a-r-plot/
  ## This allows
  par(new = TRUE)
  plot(0, type = 'n', xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, asp = 1)
  polygon(x, y, border = col, col = col)
}
