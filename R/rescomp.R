# res_comp ---------------------------------------------------------------------

#' @title resilience components
#' @description The function calculates resilience components on a data.frame of
#'  tree-ring series after Lloret et al. (2011), similar to function
#'  \code{\link[pointRes]{res.comp}}. This function also accepts pre- and post-
#'  disturbance periods of 1 in contrast to the above mentioned function.
#' @param rwl a data frame of tree ring measures.
#' @param nyrs_pre number of years defining the pre disturbance performance.
#' @param nyrs_post number of years defining the post disturbance performance.
#'
#' @return a list of data frames with the members resistance, recovery,
#'   resilience and rel_resilience. Each data frame consists of the same series
#'   as supplied with rwl row numbers reduced by nyrs_pre and nyrs_post.
#' @export
#'
#' @examples
#' library('dplR')
#' data('ca533')
#' res_comp(ca533)
res_comp <- function(rwl, nyrs_pre = 4, nyrs_post = 4) {
  if (nyrs_pre < 1 || nyrs_post < 1 || !is.wholenumber(nyrs_pre) || is.wholenumber(nyrs_post)) {
    stop('nyrs_pre and nyrs_post need to be integers >= 1')
  }
  if(!is.data.frame(x)) {
    stop('x must be of class data.frame')
  }
  common_years <- rownames(rwl)[(nyrs_pre + 1):(nrow(rwl)-nyrs_post)]
  resistance <- recovery <- resilience <- rel_resilience <- rwl
  resistance[] <- purrr::map(rwl, resistance_vector, nyrs_pre)
  recovery[] <- purrr::map(rwl, recovery_vector, nyrs_post)
  resilience <- resistance * recovery
  rel_resilience <- resilience - resistance
  list(resistance = resistance[common_years, ],
       recovery = recovery[common_years, ],
       resilience = resilience[common_years, ],
       rel_resilience = rel_resilience[common_years, ])
}


# res_comp vector functions ----------------------------------------------------

resistance_vector <- function(x, nyrs_pre = 1) {
  pre <- zoo::rollapplyr(x, by = 1, width = nyrs_pre, FUN = 'mean', fill = NA)
  shifted_pre <- c(NA, pre[-length(pre)])
  resistance <- x / shifted_pre
  resistance
}

recovery_vector <- function(x, nyrs_post = 1) {
  post <- zoo::rollapply(x, by = 1, width = nyrs_post, FUN = 'mean', fill = NA,
                         align = 'left')
  shifted_post <- c(post[-1], NA)
  recovery <- shifted_post / x
  recovery
}

resilience_vector <- function(x, nyrs_pre = 1, nyrs_post = 1) {
  resistance <- resistance_vector(x, nyrs_pre)
  recovery <- recovery_vector(x, nyrs_post)
  resilience <- resistance * recovery
  resilience
}

rel_resilience_vector <- function(x, nyrs_pre = 1, nyrs_post = 1) {
  resistance <- resistance_vector(x, nyrs_pre)
  recovery <- recovery_vector(x, nyrs_post)
  resilience <- resistance * recovery
  rel_resilience <- resilience - resistance
  rel_resilience
}



# resilience_vector2 <- function(x, nyrs_pre = 1, nyrs_post = 1) {
#   pre <- zoo::rollapplyr(x, by = 1, width = nyrs_pre, FUN = 'mean', fill = NA)
#   shifted_pre <- c(NA, pre[-length(pre)])
#
#   post <- zoo::rollapply(x, by = 1, width = nyrs_post, FUN = 'mean', fill = NA,
#                          align = 'left')
#   shifted_post <- c(post[-1], NA)
#
#   resilience <- shifted_post/shifted_pre
#   resilience
# }
#
# rel_resilience_vector2 <- function(x, nyrs_pre = 1, nyrs_post = 1) {
#   pre <- zoo::rollapplyr(x, by = 1, width = nyrs_pre, FUN = 'mean', fill = NA)
#   shifted_pre <- c(NA, pre[-length(pre)])
#
#   post <- zoo::rollapply(x, by = 1, width = nyrs_post, FUN = 'mean', fill = NA,
#                          align = 'left')
#   shifted_post <- c(post[-1], NA)
#
#   rel_resilience <- (shifted_post-x)/shifted_pre
#   rel_resilience
# }
