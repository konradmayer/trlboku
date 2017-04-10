
# tidyrwl -----------------------------------------------------------------

#' @title tidy and untidy ring width data
#' @description little helper functions to convert dataframes from the data
#'   format used in multiple dendro-related R packages such as \pkg{dplR} to
#'   tidy data used in the \pkg{tidyverse} and vice versa
#' @name tidyrwl
#' @param crn a chronology as obtained from \code{\link[dplR]{chron}}
#' @param tidy_crn a tidy chronology as obtained from \code{\link{tidy_crn}}
#' @param rwl ring width data as obtained from \code{\link[dplR]{read.rwl}}
#' @param tidy_rwl tidy ring width data as obtained from \code{\link{tidy_rwl}}
#' @param value_col column name of the value column in the tidy tibble of the
#'   input resp output object
#' @return data frames or tibbles
NULL

#' @rdname tidyrwl
#' @export
tidy_crn <- function(crn) {
  dplyr::bind_cols(tibble::tibble_(list(year = ~as.numeric(rownames(crn)))), crn)
}

#' @rdname tidyrwl
#' @export
untidy_crn <- function(tidy_crn) {
  tmp <- as.data.frame(tidy_crn)
  rownames(tmp) <- tmp[,'year']
  tmp[ ,!names(tmp) %in% 'year']
}

#' @rdname tidyrwl
#' @export
tidy_rwl <- function(rwl, value_col = 'rwl') {
  rwl %>%
    tibble::rownames_to_column('year') %>%
    tibble::as_tibble() %>%
    dplyr::mutate_(.dots = setNames(list(lazyeval::interp(~as.numeric(var), var = as.name('year'))), 'year')) %>%
    tidyr::gather_("series", value_col, select_vars_(names(.),names(.), exclude = "year")) %>%
    dplyr::filter_(lazyeval::interp(~(!is.na(nam)), nam = as.name('rwl')))
}

#' @rdname tidyrwl
#' @export
untidy_rwl <- function(tidy_rwl, value_col = 'rwl') {
  tidy_rwl %>%
    tidyr::spread_("series", value_col) %>%
    tibble::remove_rownames() %>%
    as.data.frame() %>%
    tibble::column_to_rownames('year')
}

