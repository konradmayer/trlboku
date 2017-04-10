#' @title visually test normality assumtion
#' @description try to spot the qqplot of your data within 8 other qqplots,
#'   drawn randomly from normal distributions as suggested here:
#'   \url{http://www.nate-miller.org/blog/how-normal-is-normal-a-q-q-plot-approach}
#' @param model a model, output of \code{\link{lm}}
#'
#' @return the number of the plot using real data
#' @export
#' @examples
#' qqfunc(lm(speed ~ dist, data = cars))

qqfunc <- function(model){
  N <- length(resid(model))
  sigma <- summary(model)$sigma
  op <- par(mfrow = c(3,3))
  rnum <- sample(1:9, 1)
  for (i in 1:(rnum - 1)) {
    x <- rnorm(N, 0, sigma)
    qqnorm(x, main = i)
    qqline(x)
  }
  qqnorm(resid(model), main = rnum)
  qqline(resid(model))
  for (i in (rnum + 1):9) {
    x <- rnorm(N, 0, sigma)
    qqnorm(x, main = i)
    qqline(x)
  }
  on.exit(par(op))
  return(rnum)
}
