#--------------------------
#yr.range
#--------------------------
#' @title yr.range
#' @description function yr.range from dplR, extracts the first and last year in
#'   yr.vec where the corresponding numeric vector shows a value different to NA
#' @param x a numeric vector
#' @param yr.vec a character- or numeric vector containing years with the same
#'   length as x
#' @return a character vector of length 2 containing the first and last year
#' @examples
#' library(dplR)
#' data(ca533)
#' yr.range(ca533[ ,1], rownames(ca533))
#' #[1] "1530" "1983"

yr.range <- function (x, yr.vec = as.numeric(names(x)))
{
  na.flag <- is.na(x)
  if (all(na.flag)) {
    res <- rep(NA, 2)
    mode(res) <- mode(yr.vec)
    res
  }
  else {
    range(yr.vec[!na.flag])
  }
}

#--------------------------
#trl_palette
#--------------------------
#' @title trl_palette
#' @description manipulate the color palette to contain a set of 25 colors
trl_palette <- function(){
  c25 <- c("dodgerblue2","#E31A1C", # red
           "green4",
           "#6A3D9A", # purple
           "#FF7F00", # orange
           "orchid3","gold1",
           "skyblue2","#FB9A99", # lt pink
           "palegreen2",
           "#CAB2D6", # lt purple
           "#FDBF6F", # lt orange
           "gray70", "khaki2",
           "maroon","orchid1","deeppink1","blue1","steelblue4",
           "darkturquoise","green1","yellow4","yellow3",
           "darkorange4","brown")
  palette(c25)
}

#--------------------------
#round_up
#--------------------------
#' @title round_up
#' @description round up a number to the nearest multiple of the argument "to"
#' @param x numeric or integer
#' @param to numeric or integer
#' @examples
#' round_up(1412, to = 1000)
#' # [1] 2000
round_up <- function(x, to = 1000)
{
  to * (x %/% to + as.logical(x %% to))
}

#--------------------------
#round_down
#--------------------------
#' @title round_down
#' @description round down a number to the nearest multiple of the argument "to"
#' @param x numeric or integer
#' @param to numeric or integer
#' @examples
#' round_down(1412, to = 1000)
#' # [1] 1000
round_down <- function(x, to = 1000)
{
  to * (x %/% to)
}

#--------------------------
#is.wholenumber
#--------------------------
is.wholenumber <- function(x) x %% 1 == 0

#--------------------------
#first_last
#--------------------------
#' @title first_last
#' @description function to return the first and the last year of a tree ring series
#' @param x a data.frame/rwl object
#'
#' @return a data.frame with series names in the first column as character strings,
#'   and the first as well as the last years of the series in the second resp.
#'   third column
#' @export
#' @examples
#' library('dplR')
#' data('gp.rwl')
#' first_last(gp.rwl)
first_last <- function(x){

  if(!is.data.frame(x)){
    stop('please provide a data.frame/rwl object')
  }

  tmp <- sapply(seq_along(x), FUN=function(i) as.double(yr.range(gp.rwl[ , i],
                                                                 rownames(x))))
  out <- data.frame(names(x), t(tmp))
  names(out) <- c('series', 'first', 'last')
  out$series <- as.character(out$series) #not elegant but works
  return(out)
}
