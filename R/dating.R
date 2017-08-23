#new_date_end-------------------------------------------------------------------
#' @title new_date_end
#' @description Batch adjust the date end of multiple series.
#' @param rwl a data.frame/rwl object.
#' @param date.end a data.frame with series names in the first, and date end
#'   in the second column.
#'
#' @return a data.frame/rwl object
#' @export
#' @examples
#' library("dplR")
#' data("gp.rwl")
#' de <- dplR::rwl.stats(gp.rwl)[ , c(1,3)] #construct a data frame of current date ends
#' de[1:4, 2] <- 1600 #changing the date end of the first 4 series to 1600
#' seg.plot(new_date_end(gp.rwl, de))
new_date_end <- function(rwl, date.end) {
  #argument check
  if(missing(rwl) || missing(date.end)) {
    stop('please provide rwl and date.end as input')
  }

  if(!(is.data.frame(rwl) && is.data.frame(date.end))) {
    stop('please provide input data with correct class')
  }

  if(!(all(dim(rwl) > 0) && nrow(date.end) > 0 && ncol(date.end) == 2)) {
    stop('please provide input data with correct dimensions')
  }

  if(!setequal(date.end[ , 1], names(rwl))) {
    stop('series names in po are not the same as provided in rwl')
  }
  #execute function
  tmp <- list()
  for (i in seq_along(date.end[,1])) {
    seriesname <- as.character(date.end[i, 1])
    dateend <- as.numeric(date.end[i, 2])
    series <- rwl[,seriesname]
    series <- na.exclude(series)
    rownames <- (dateend - length(series) + 1):dateend
    series.df <- data.frame(series, row.names = as.character(rownames))
    names(series.df) <- seriesname

    tmp[[i]] <- series.df
  }
  out <- dplR::combine.rwl(tmp)
  return(out)
}


#rwl_subout---------------------------------------------------------------------
#' @title rwl_subout
#' @description This function takes an rwl object as well as a csv as input and
#'   subsets the rwl by the identifiers given in the first column of the csv.
#'   The subset as well as the remaining series are saved to disk as rwl file
#'   using the basename of the path provided with argument subset or as
#'   specified by out.nam.
#'
#' @param rwl.file a path to an rwl file (tucson format).
#' @param subset a path to a csv file containing series names (or site IDs if
#'   \code{site.only == TRUE}) in the first column.
#' @param header logic, indicates if subset csv file has column names
#' @param out.nam character, optional string used for output file naming.
#' @param write.missing logic, if \code{write.missing == TRUE} an output csv is written containing
#'   series identifiers missing in rwl
#' @param site.only logic, if \code{TRUE} the subset is done based on the site
#'   specification in stc
#' @param stc parameter as defined in \code{\link[dplR]{read.ids}}, only used if
#'   \code{site.only == TRUE}.
#' @export

rwl_subout <- function(rwl.file, subset, header = FALSE, out.nam = NULL,
                       write.missing = FALSE, site.only = FALSE,
                       stc = c(3, 4, 1)) {

  #get filename of subset file
  if(is.null(out.nam)) {
    out.nam <- basename(subset)
  }
  on <- gsub('/', '', out.nam)

  #read files
  dat <- dplR::read.tucson(rwl.file)
  subsetdf <- read.csv(subset, header = header, stringsAsFactors = F)

  #treat setting site.only
  if (isTRUE(site.only) && !all(nchar(subsetdf[,1]) == stc[1])) {
    stop('site ids are not the same length as specified by stc')
  }

  if (isTRUE(site.only)) {
    nam <- substr(names(dat), 1, stc[1])
  } else {
    nam <- names(dat)
  }


  #check series names
  if(sum(nam %in% subsetdf[ ,1]) == 0) {
    stop('no identifier found in rwl.file')
  }

  #treat missing series
  missing <- subsetdf[!(subsetdf[ ,1] %in% nam), 1]

  if(write.missing == TRUE && length(missing) > 0) {
    write.csv(missing,paste0("series of - \'",on,"\' missing in data.csv"))
  }

  if(length(missing) > 0) {
    warning(paste0(missing, ' - not included in rwl'))
  }

  #get subset
  subs <- dat[nam %in% subsetdf[ ,1]]
  remaining <- dat[!(nam %in% subsetdf[ ,1])]

  #write files
  dplR::write.tucson(subs, paste0("subset of - ",on," .rwl"), long.names = TRUE)
  dplR::write.tucson(remaining, paste0("all except - ",on," .rwl"), long.names = TRUE)
}
