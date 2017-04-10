
untidy_crn <- function(cr) {
  tmp <- as.data.frame(cr)
  rownames(tmp) <- tmp[,'year']
  dplyr::select(tmp, -year)
}

#in SA formulieren

tidy_rwl <- function(rwl) {
  tibble::as_tibble(rwl)%>%
    dplyr::mutate(year = as.integer(rownames(.))) %>%
    tidyr::gather(series, rwl, -year) %>%
    filter(!is.na(rwl))
}



