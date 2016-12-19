#trl_palette--------------------------------------------------------------------
#' @title trl_palette
#' @description manipulate the color palette to contain a set of 25 colors
#' @export
trl_palette <- function() {
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

#round_up-----------------------------------------------------------------------
#' @title round_up
#' @description round a number up to the nearest multiple of the argument "to"
#' @param x numeric or integer
#' @param to numeric or integer
#' @export
#' @examples
#' round_up(1412, to = 1000)
#' # [1] 2000
round_up <- function(x, to = 1000) {

  if(is.factor(x) || is.factor(to) || is.null(to)) {
    stop('input must be numeric')
  }

  to * (x %/% to + as.logical(x %% to))
}

#round_down---------------------------------------------------------------------
#' @title round_down
#' @description round a number down to the nearest multiple of the argument "to"
#' @param x numeric or integer
#' @param to numeric or integer
#' @export
#' @examples
#' round_down(1412, to = 1000)
#' # [1] 1000
round_down <- function(x, to = 1000) {

  if(is.factor(x) || is.factor(to) || is.null(to)) {
    stop('input must be numeric')
  }

  to * (x %/% to)
}

#is.wholenumber-----------------------------------------------------------------
is.wholenumber <- function(x) {
  if(is.factor(x)) { stop('x needs to be numeric') }
  x %% 1 == 0
}

#mgsub--------------------------------------------------------------------------
#' @title generalization of gsub
#' @description a generalization of gsub which allows to do multiple
#'   replacements at once as described in
#'  \link{http://stackoverflow.com/questions/15253954/replace-multiple-arguments-with-gsub}
#' @param myrepl a list containing character vectors of length two each with the
#'   string to be replaced at the first and the replacement at the second index
#' @param mystring a character vector where matches are sought, or an object
#'   which can be coerced by as.character to a character vector.
#'
#' @return a character string
#' @export
#'
#' @examples
#' mystring = 'This is good'
#' myrepl = list(c('o', 'a'), c('i', 'n'))
#' mgsub(myrepl, mystring)
mgsub <- function(myrepl, mystring) {

  stopifnot(is.list(myrepl), is.character(mystring),
            all(lapply(myrepl, length) == 2),
            all(unlist(lapply(myrepl, is.character))))

  gsub2 <- function(l, x) {
    do.call('gsub', list(x = x, pattern = l[1], replacement = l[2]))
  }
  Reduce(gsub2, myrepl, init = mystring, right = T)
}


#intersect_all------------------------------------------------------------------
#' @title intersect_all
#' @description a generalization of intersect() to take more than two vectors as
#'   input
#' @param a a vector
#' @param b a vector
#' @param ... even more vectors
#' @export
#' @examples
#' intersect_all(c(1,2,3,4), c(3,4,5), c(3,4,6,7))
intersect_all <- function(a,b,...){
  Reduce('intersect', list(a,b,...))
}

