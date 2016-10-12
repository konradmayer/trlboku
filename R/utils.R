#--------------------------
#trl_palette
#--------------------------
#' @title trl_palette
#' @description manipulate the color palette to contain a set of 25 colors
#' @export
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
#' @export
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
#' @export
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

