#--------------------------
#new_date_end
#--------------------------
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
new_date_end <- function(rwl, date.end){
  #argument check
  if(missing(rwl) || missing(date.end)){
    stop('please provide rwl and date.end as input')
  }

  if(!(is.data.frame(rwl) && is.data.frame(date.end))){
    stop('please provide input data with correct class')
  }

  if(!(all(dim(rwl) > 0) && nrow(date.end) > 0 && ncol(date.end) == 2)){
    stop('please provide input data with correct dimensions')
  }

  if(!setequal(date.end[ , 1], names(rwl))){
    stop('series names in po are not the same as provided in rwl')
  }
  #execute function
  tmp <- list()
  for (i in seq_along(date.end[,1])){
    seriesname <- as.character(date.end[i, 1])
    dateend <- as.numeric(date.end[i, 2])
    series <- rwl[,seriesname]
    series <- na.exclude(series)
    rownames <- (dateend - length(series) + 1):dateend
    series.df <- as.data.frame(series, row.names = rownames)
    names(series.df) <- seriesname

    tmp[[i]] <- series.df
  }
  out <- dplR::combine.rwl(tmp)
  return(out)
}
