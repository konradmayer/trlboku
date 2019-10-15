
# load_windendro ----------------------------------------------------------

## helpers
create_single_series_df <- function(series, dateend) {
  rownames <- (dateend - length(series) + 1):dateend
  series_df <- data.frame(series, row.names = as.character(rownames))
  series_df
}

check_names <- function(nm) {
  nm[(duplicated(nm))]
}


# main fun


#' Read WinDENDRO files
#'
#' @param file path to the WinDENDRO file
#' @param encoding encoding of the WinDENDRO file (should be "latin1")
#' @param trust_ringcount if FALSE: values not beeing metadata (= measured values) are checked whether they are of the same length as the ring count specified in the metadata. If commends or marked features are used in WinDENDRO this has to be set to TRUE, then this check is omitted and only the number of values up to the specified ring count read in.
#'
#' @return an object of class `rwl`
#' @export

read_windendro <- function(file, encoding = 'latin1',
                           trust_ringcount = FALSE) {
  con <- file(file, encoding = encoding)
  on.exit(close(con))
  lines <- readLines(con, encoding = encoding)
  #  close(con)
  lst <- strsplit(lines, '\t')

  version <- lst[[1]][2] #windendro format version
  start_idx <- as.numeric(lst[[1]][4]) # measurement starts at column
  ring_order <- lst[[1]][5] # P (pith to bark) or B (bark to pith)
  bark <- lst[[1]][7] # Y or N
  ring_based <- lst[[1]][8] # RING or pixel based

  if(version != '4' | ring_order != 'P' | bark != 'N' | ring_based != 'RING') {
    stop("The current import function only supports ring based measurements in the WinDENDRO 4 file format measured from pith to bark.")
  }

  meta <- lapply(lst[c(-1, -2)], function(.x) .x[1:(start_idx - 1)])
  series_ids <- unlist(lapply(meta, function(.x) .x[1]))
  last_year <- lapply(meta, function(.x) as.numeric(.x[4]))
  ring_count <- lapply(meta, function(.x) as.numeric(.x[10]))
  data_type <- unlist(lapply(meta, function(.x) .x[11]))

  # ring features and commends are not supported by the current version of the function
  # of present they are added to the right of the measured values
  # if trust_ringcount == TRUE, only values from start_idx to start_idx+ring_count are read, otherwise
  # checked if number of available values right of the metadata equals to ring_count
  # (throws error if features or comments are included)
  if(trust_ringcount) {
    values <- mapply(function(.x, .y) na.omit(as.numeric(.x[start_idx:(start_idx + .y)])), lst[c(-1, -2)], ring_count)
  } else {
    values <- lapply(lst[c(-1, -2)], function(.x) na.omit(as.numeric(.x[start_idx:length(.x)])))
    if(!all(unlist(ring_count) == vapply(values, length, FUN.VALUE = numeric(1)))) {
      stop('Number of measured rings does not equal ring count in the metadata.')
    }
  }

  #create rwl
  values_df <- mapply(create_single_series_df, values, last_year,
                      SIMPLIFY = FALSE)
  rwl <- dplR::combine.rwl(values_df)
  class(rwl) <- c("rwl", "data.frame")

  # split into list of datatypes
  column_ids_list <- split(seq_len(ncol(rwl)), data_type)
  rwl_list <- lapply(column_ids_list, function(.x) rwl[ , .x])

  #name series
  series_ids_list <- split(series_ids, data_type)
  lapply(names(rwl_list), function(.x) colnames(rwl_list[[.x]]) <- series_ids_list[[.x]])


  out <- sapply(names(rwl_list), function(.x) setNames(rwl_list[[.x]], series_ids_list[[.x]]),
                USE.NAMES = TRUE, simplify = FALSE)


  #check for duplicated series names
  check_duplicated_names <- sapply(out, function(.x) check_names(names(.x)), simplify = FALSE)
  if (any(sapply(check_duplicated_names, function(.x) length(.x) > 0))) {
    warning("Duplicated series names contained.")
    cat('Duplicated series names contained:\n')
    invisible(sapply(names(check_duplicated_names),
                     function(.x) cat(.x, ': ',
                                      paste(check_duplicated_names[[.x]], collapse = ', '),
                                      '\n', sep = '')))
  }

  # return object
  out
}


