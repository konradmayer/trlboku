#' @title combine fragments
#' @description combine multiple fragments of a series as from measuring parts
#'   of densitometry samples in WinDENDRO according to set rules - this function
#'   also averages overlapping measurements.
#' @param rwl a rwl/data.frame object.
#' @param stc numeric, parameter as defined in \code{\link[dplR]{read.ids}}.
#' @param rules combining rules as a vector of named regular expressions -
#'   the default combines fragments ending with "f-l" to "a", "m-w" to "b",
#'   "c" to "c" and "x-z" to "d".
#'
#' @return a data.frame.
#' @export
#'
#' @examples
#' rwl <- data.frame('AbcAA01f' = c(1, 1, NA, NA, NA),
#'                   'AbcAA01g' = c(NA, NA, 1, 1, NA),
#'                   'AbcAA01h' = c(NA, NA, NA, NA, 1),
#'                   'AbcAA01m' = c(2, 2, 2, NA, NA),
#'                   'AbcAA01n' = c(NA, NA, NA, 2, 2),
#' combine_fragments(rwl)
combine_fragments <- function(rwl, stc = c(3, 4, 1),
                              rules = c('a' = '[af-l]$',
                                        'b' = '[bm-w]$',
                                        'c' = '[c]$',
                                        'd' = '[dx-z]$')){

  if(!is.data.frame(rwl)) {
    stop('rwl must be a data.frame')
  }

  if((length(stc) != 3) || !is.numeric(stc)) {
    stop('stc must be a numeric vector of length 3')
  }

  if(any(nchar(names(rwl)) != sum(stc))) {
    stop('number of characters in series names must equal the sum of stc')
  }

  if(!is.character(rules) || is.null(names(rules))) {
    stop('please supply rules as named character vector')
  }

  possible_trees <- outer(unique(substr(names(rwl), 1, sum(stc[1:2]))),
                          names(rules), paste0)

  possible_trees_regexp <- outer(unique(substr(names(rwl), 1, sum(stc[1:2]))),
                                 rules, paste0)

  select <- list()
  for (t in seq_along(possible_trees)){
    select[[possible_trees[t]]] <- grep(possible_trees_regexp[t], names(rwl))
  }

  select <- select[lengths(select) > 0]

  out <- do.call('data.frame',
                 lapply(select, function(x) rowMeans(rwl[x], na.rm = TRUE)))
  out <- out[order(names(out))]
  return(out)
}
