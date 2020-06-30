#yr_range-----------------------------------------------------------------------
#' @title yr_range
#' @description function yr_range from dplR, extracts the first and last year in
#'   yr.vec where the corresponding numeric vector shows a value different to NA
#' @param x a numeric vector
#' @param yr.vec a character- or numeric vector containing years with the same
#'   length as x
#' @return a character vector of length 2 containing the first and last year
#' @examples
#' \dontrun{
#' library(dplR)
#' data(ca533)
#' yr_range(ca533[ ,1], rownames(ca533))
#' #[1] "1530" "1983"
#' }

yr_range <- function (x, yr.vec = as.numeric(names(x))) {
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

#first_last---------------------------------------------------------------------
#' @title first_last
#' @description function to return the first and the last year of tree ring series
#'  in an rwl object
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
first_last <- function(x) {

  if(!is.data.frame(x)){
    stop('please provide a data.frame/rwl object')
  }

  if(any(is.na(suppressWarnings(as.integer(rownames(x)))))){
    stop('please provide an input object with correct rownames')
  }

  tmp <- sapply(seq_along(x), FUN=function(i) {
    as.double(yr_range(x[ , i], as.numeric(rownames(x))))
  })
  out <- data.frame(names(x), t(tmp))
  names(out) <- c('series', 'first', 'last')
  out$series <- as.character(out$series) #not elegant but works
  return(out)
}

#series_length------------------------------------------------------------------
#' @title series length
#' @description returns the series length of the series within a data.frame/rwl
#'   object.
#' @param x a data.frame/rwl object
#'
#' @return a numeric vector
#' @export
series_length <- function(x) {
  sapply(x, FUN = function(y) length(na.omit(y)))
}

#truncate_rwl-------------------------------------------------------------------
#' @title truncate_rwl
#' @description This function removes lines at the beginning and end of a
#'   data.frame/rwl object only containing NA values (as present after subsetting etc.).
#' @param x a data.frame/rwl object
#'
#' @return a data.frame/rwl object
#' @export
#'
#' @examples
#' library("dplR")
#' data("gp.rwl")
#' subset <- gp.rwl[ ,1:10] #first value 1720, but rows start with 1570
#' truncate_rwl(subset) #rows start with 1720
truncate_rwl <- function(x) {

  if(!is.data.frame(x)) {
    stop('x must be of class data.frame')
  }

  skip_idx <- apply(x, 1, function(x) all(is.na(x)))

  if(all(skip_idx)) {
    return(x[NULL,NULL])
    warning("empty rwl/dataframe returned")
  } else {
    first_false <- which.min(skip_idx)
    last_false <- length(skip_idx) - which.min(rev(skip_idx)) + 1
    x[first_false:last_false, , drop = FALSE]
  }
}


#expand_apply generic-----------------------------------------------------------
#' @title expand_apply
#' @description Apply a function on an expanding window.
#' @param x a numeric vector (NA is allowed and will be omitted)
#'   or a data.frame/rwl object
#' @param ... additional arguments
#' @param FUN character, name of a function e.g. 'median'.
#' @return The form of the value depends on the class of x. returns a vector
#'   for default method, a data.frame for data.frame method.
#' @export
#' @examples
#' #example for numeric method:
#' x <- c(NA, NA, 1, 3, 2, 3, NA)
#' expand_apply(x, 'median')
#' #example for data.frame method:
#' library('dplR')
#' data('ca533')
#' expand_apply(ca533, 'median')

expand_apply <- function(x, FUN) UseMethod('expand_apply', x)

#expand_apply.numeric-----------------------------------------------------------
#' @export
expand_apply.default <- function(x, FUN = 'median') {
  if(!is.numeric(x)) {
    stop('x must be numeric or integer')
  }

  out <- sapply(seq_along(x), function(y) do.call(FUN, list(na.omit(x[seq_len(y)]))))
  out[is.na(x)] <- NA
  return(out)
}

#expand_apply.data.frame--------------------------------------------------------
#' @export
#' @method expand_apply data.frame
expand_apply.data.frame <- function(x, FUN = 'median') {
  x[] <- lapply(x, function(y) expand_apply.default(as.vector(y), FUN))
  return(x)
}

#radius_rwl---------------------------------------------------------------------
#' @title radius_rwl
#' @description A function to get the cumulated tree ring widths
#'   (approx. radius of the tree in the given year).
#' @param rwl a rwl/data.frame object
#' @return a rwl/data.frame object with cumulated tree ring widths for each
#'   series (column)
#' @export
#' @examples
#' library("dplR")
#' data("gp.rwl")
#' radius_rwl(gp.rwl)
radius_rwl <- function(rwl) {

  if(!is.data.frame(rwl)) {
    stop('please provide input of class rwl or data.frame')
  }

  if(!all(apply(rwl, 2, is.numeric))) {
    stop('input contains non numeric values')
  }

  expand_apply.data.frame(rwl, 'sum')

}

#age_rwl------------------------------------------------------------------------
#' @title age_rwl
#' @description A function returning an rwl/data.frame object with the same
#'   dimensions as rwl with showing the cambial age instead of tree ring width
#'   in the corresponding year.
#' @param rwl a rwl/data.frame object
#' @return a rwl/data.frame object with cambial age of the years of the series.
#' @export
#' @examples
#' library("dplR")
#' data("gp.rwl")
#' age_rwl(gp.rwl)
age_rwl <- function(rwl) {

  if(!is.data.frame(rwl)) {
    stop('please provide input of class rwl or data.frame')
  }

  if(!all(apply(rwl, 2, is.numeric))){
    stop('input contains non numeric values')
  }

  expand_apply.data.frame(rwl, 'length')
}

#avg_trees----------------------------------------------------------------------
#' @title avg_trees
#' @description Averages series from multiple cores taken from the same tree/object.
#' @param rwl a data.frame/rwl object.
#' @param stc parameter as defined in \code{\link[dplR]{read.ids}}.
#' @return a data frame/rwl object.
#' @export
#'
#' @examples
#' library("dplR")
#' data("gp.rwl")
#' avg_trees(gp.rwl, stc = c(0,2,1))
avg_trees <- function(rwl, stc = c(3, 4, 1)) {
  #argument checks:
  if(!any(class(rwl) %in% c('rwl', 'data.frame'))) {
    stop('please provide an object of class rwl or data.frame')
  }

  if (!(length(stc) == 3 && is.numeric(stc))) {
    stop('argument stc has to be numeric of length 3')
  }

  ids <- dplR::read.ids(rwl, stc = stc)
  treeIds <- ids$tree
  unique.trees <- unique(treeIds)
  n.trees <- length(unique.trees)
  cores.of.tree <- list()
  seq.tree <- seq_along(unique.trees)
  for (i in seq.tree) {
    cores.of.tree[[i]] <- which(treeIds == unique.trees[i])
  }
  rwl.2 <- data.frame(matrix(data = as.numeric(NA), nrow = nrow(rwl), ncol = n.trees,
                             dimnames = list(rownames(rwl))))
  for (i in seq.tree) {
    these.cols <- cores.of.tree[[i]]
    tmp <- rowMeans(rwl[, these.cols, drop=FALSE], na.rm=TRUE)
    tmp[is.nan(tmp)] <- NA
    rwl.2[ , i] <- tmp
  }
  names(rwl.2) <- unique(substr(names(rwl), 1, stc[1] + stc[2]))
  class(rwl.2) <- c('rwl', 'data.frame')
  return(rwl.2)
}

# internal NA_series -----------------------------------------------------------
#code stolen from dplR function
internalNA_series <- function(x) {
  x.na <- is.na(x)
  x.ok <- which(!x.na)
  n.ok <- length(x.ok)
  if (n.ok <= 1) {
    return(x)
  }
  first.ok <- x.ok[1]
  last.ok <- x.ok[n.ok]
  return(last.ok - first.ok + 1 > n.ok)
}

# internal NA_rwl --------------------------------------------------------------

internalNA_rwl <- function(x) {
  vapply(x, internalNA_series, logical(1))
}

# rwl_problems --------------------------------------------------------------

rwl_problems <- function(x, message = FALSE) {

  a <- !is.data.frame(x)
  if (a) {
    message("your tree-ring data must be provided as a data.frame")
  }

  b <- !all(vapply(x, purrr::possibly(is.numeric, TRUE), FALSE, USE.NAMES = FALSE))
  if (b) {
    message("your tree-ring data must have numeric columns")
  }

  c <- any(internalNA_rwl(x))
  if(c) {
    message(paste0("your tree-ring data must not have internal NAs - Problems appear at series: ", paste0(names(which(internalNA_rwl(x))), collapse = ', ')))  }

  if (any(a, b, c)) {
    return(TRUE)
  }else{
    if(message){
      message('your data looks fine - no problems detected')
    }
    return(FALSE)
  }
}


# test_rwl_set ----------------------------------------------------------
all_identical <- function(l) all(mapply(identical, head(l, 1), tail(l, -1)))

rwl_set_problems <- function(rw = NULL, ew = NULL, lw = NULL, ewp = NULL,
                             lwp = NULL, rd = NULL, ed = NULL, ld = NULL,
                             maxd = NULL, mind = NULL, tolerance = 0.01,
                             tolerance.percentage = 0.05, logic.output = FALSE) {

  dat <- purrr::compact(list(rw = rw, ew = ew, lw = lw, ewp = ewp, lwp = lwp,
                             rd = rd, ed = ed, ld = ld, maxd = maxd, mind = mind))


  # compute min mean and max per dataset
  smry <- function(x) {
    funs <- c(min, mean, max)
    sapply(funs, function(f) round(f(as.matrix(x), na.rm = TRUE), 2))
  }
  tmp <- purrr::reduce(purrr::map(dat, smry), rbind)
  if(is.null(nrow(tmp))){
    smry_table <- as.data.frame(t(as.data.frame(c(names(dat), tmp))))
  }else{
    smry_table <- as.data.frame(cbind(names(dat), tmp))
  }
  names(smry_table) <- c('parameter', 'min', 'mean', 'max')
  rownames(smry_table) <- NULL


  # TESTS

  # if only one dataset provided, run rwl_problems() on it:
  if (length(dat) == 1) {
    message('you only provided a single object - only the checks of test_rwl() will be done')
    a <- rwl_problems(dat[[1]])
    if(a) {
      message("the object you provided has a problem - use test_rwl() to get details")
    }
    x <- b <- c <- d <- e  <- NULL
  }else{

    # test for dimensions
    x <- !all_identical(purrr::map(dat, dim))
    if(x) {
      message("objects have differing dimensions")
      return(TRUE)
    }

    # test if individual rwls are fine (object type, data type, internal NAs):
    a <- any(unlist(purrr::map(dat, rwl_problems)))
    if(a) {
      message("at least one of the individual object has a problem - use test_rwl() to find it")
    }

    # test if names are equal:
    b <- !all_identical(purrr::map(dat, names))
    if(b){
      message("the objects have series with differing names or wrong series order")
    }


    # test if first and last years are equal:
    c <- !all_identical(purrr::map(dat, function(z) first_last(z)[2:3]))
    if(c){
      message("the objects have differing fist or last years")
    }

    # test if sum of ew and lw equals rw:
    d <- NULL
    if(all(c('rw', 'ew', 'lw') %in% names(dat)) && !x) {
      d <- !isTRUE(all.equal(dat[['rw']],  (dat[['ew']] + dat[['lw']]),
                             check.attributes = FALSE, use.names = TRUE,
                             tolerance = tolerance))
      if(d){
        message("sum of ew and lw differs to rw")
      }
    }

    # test if sum of ewp and lwp equals 1:
    e <- NULL
    if(all(c('ewp', 'lwp') %in% names(dat)) && !x) {
      e <- !all(na.omit(as.logical(abs((dat[['ewp']] + dat[['lwp']]) - 1) < tolerance.percentage)))
      if(e){
        message("ewp and lwp doesn't sum up to 1")
      }
    }


    #ADD TESTS HERE FOR DENSITY (MAXD > MIND, ED < LD, LD < MAXD...) IN FUTURE!!!

  }
  # output summary table
  message(paste0("Summary:\n", paste0(capture.output(smry_table), collapse = "\n")))


  # output true if there is at minimum one problem detected
  all_returns <- purrr::compact(c(x, a, b, c, d, e))
  if(logic.output){
    any(all_returns)
  }else{
    if(any(all_returns)){stop('there are some problems existing')}
  }

}



# write_fh ----------------------------------------------------------------

# helpers
fh_write_header <- function(header_keycode, header_datebegin, header_dateend,
                            header_length, header_dataformat, header_unit,
                            header_meta, file, line_termination) {
  cat("HEADER:",
      paste0('KeyCode=', header_keycode),
      paste0('DateBegin=', header_datebegin),
      paste0('DateEnd=', header_dateend),
      paste0('Length=', header_length),
      paste0('DataFormat=', header_dataformat),
      paste0('Unit=', header_unit),
      file = file, sep = line_termination)
  cat(paste(names(header_meta), header_meta,
            sep = '=', collapse = line_termination),
      sep = "") #last item separate call to cat, to avoid a newline after header
}

fh_write_data <- function(series, prec, file, line_termination) {
  values_series <- round(na.omit(series / prec))
  #  full_lines <- length(values_series) %/% 10 # number of complete line
  n_pad <- 10 - (length(values_series) %% 10) # how many values are missing for a full line?
  #  n_lines <- ifelse(n_pad == 0, full_lines, full_lines + 1)
  values_out <- c(sprintf("%06s", as.character(values_series)), # padded values
                  rep(sprintf("%06s", '0'), n_pad)) # 0 to fill to full line

  cat("DATA:Single", file = file, sep = line_termination)
  for (i in seq_along(values_out)) {
    cat(values_out[[i]], file = file, sep = "")
    if (i %% 10 == 0)
      cat(line_termination, file = file, sep = "")
  }
}

# main function

#' Write to a file in Heidelberg format
#'
#' @description \code{write_fh}, just as its \link[dplR]{dplR} consistent
#' equivalent \code{write.fh}, writes an rwl object to a file in Heidelberg
#' (.fh) format. It currently only supports "Single format".
#' @param rwl a data.frame/rwl object such as returned e.g. by
#' \code{\link[dplR]{as.rwl}}.
#' @param path a character vector specifying the path to the file where the
#' data will be written, usually ending with ".fh".
#' @param data.format DataFormat as specified by the definition of the
#' Heidelberg format (either Tree, HalfChrono or Chrono). This is used
#' for all series within the written object.
#' @param prec numeric, indicating the precision of the output file. This must
#' be equal to either 0.01 or 0.001 (units are in mm). This is used
#' for all series within the written object.
#' @param meta optional additional meta data. If a named vector is supplied,
#' name value pairs are used as metadata for all series. Supply a list of named
#' vectors to use different metadata for each individual series.
#' @param append logical, indicating whether data should be appended to an
#' existing file. Defaults to create a new file, overwriting a file in case
#' it already exists.
#' @aliases write.fh
#'
#' @export

write_fh <- write.fh <- function(rwl, path,
                                 data.format = c("Tree", "HalfChrono", "Chrono"),
                                 prec = 0.01,
                                 meta = NULL, append = FALSE) {
  line_termination <- "\n"
  # input validation
  header_dataformat <- match.arg(data.format)
  if (length(prec) > 1 | !any(prec %in% c(0.01, 0.001))) {
    stop("prec needs to be a numeric vector of length 1 with a value of 0.01 or 0.001")
  }
  if (rwl_problems(rwl)) {
    stop("please fix your rwl object")
  }

  if (is.list(meta)) {
    if (length(meta) != ncol(rwl)) {
      stop("please provide meta data as a list of the same length as the number of series in rwl or provide a single named vector to use the same metadata for all series.")
    }
  }

  # derive intrinsic metadata
  firstlast <- first_last(rwl)
  header_keycode <- firstlast[[1]]
  header_datebegin <- firstlast[[2]]
  header_dateend <- firstlast[[3]]
  header_length <- as.vector(series_length(rwl))


  unit <- switch(as.character(prec),
                 "0.01" = "1/100 mm",
                 "0.001" = "1/1000 mm")

  if (append) {
    if (!file.exists(path)) {
      warning(gettextf("file %s does not exist, therefore data is not appended but written to a new file",
                       path))
    }
    rwl_out <- file(path, "a")
  }
  else {
    rwl_out <- file(path, "w")
  }
  on.exit(close(rwl_out))

  # loop over series
  for (i in seq_along(header_keycode)) {
    # meta can be supplied either as an atomic named vector if it is the same
    # for all series, or as a list of named vectors if different for each series
    meta_tmp <- if (is.atomic(meta)) {
      meta
    }else{
      meta[[i]]
    }

    # write header
    fh_write_header(header_keycode[[i]],  header_datebegin[[i]],
                    header_dateend[[i]], header_length[[i]],
                    header_dataformat, unit, meta_tmp,
                    rwl_out, line_termination)

    # write data
    fh_write_data(rwl[, i], prec, rwl_out, line_termination)

  }
}
