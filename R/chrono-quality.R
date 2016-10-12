#-------------------------
#agegrowthplot
#-------------------------
#' @title agegrowthplot
#' @description plots mean annual growth rate in a common interval (shown in
#'   first plot) over tree age - a subset (eg. historic vs. recent trees,
#'   a different site ect.) can be specified to get a different color in the plot.
#' @param rwl an rwl object
#' @param po a data frame with series names in the first and pith offset values
#'   (number of rings) in the second column
#' @param subset provide a character string of names in rwl which get
#'   a different color in the last plot
#' @param subset.color color of the subset
#' @param main optional plot title
#' @export
#' @examples
#' library("dplR")
#' data("gp.rwl")
#' data("gp.po")
#' gp.po[ ,1] <- as.character(gp.po[ ,1])
#' sub <- names(gp.rwl)[substr(names(gp.rwl), 3, 3) == "B"]
#' agegrowthplot(gp.rwl, gp.po, subset = sub, main = 'my age-growth-plot')
agegrowthplot <- function(rwl, po, subset = NULL, subset.color = 'violet', main = ''){
  #check arguments
  if(!all(class(rwl) == c('rwl', 'data.frame'))){
    stop('provide input object of class rwl')
  }

  if(!((dim(po)[2] == 2) && is.numeric(po[,2]) && is.character(po[,1]))){
    stop('provide valid po object with series names as characters in first column
         and pith offset as integer values in the second')
  }

  rwl.ord <- apply(rwl, 2, sortByIndex)
  col.names <- names(rwl)
  n.col=ncol(rwl)
  seq.cols <- seq_len(n.col)
  rwca <- matrix(NA, ncol =n.col, nrow = sum(nrow(rwl.ord) + max(po[, 2])))
  nrow.m1 <- nrow(rwl.ord) - 1
  for (i in seq.cols) {
    yrs2pith <- po[po[, 1] %in% col.names[i], 2]
    rwca[yrs2pith:(yrs2pith + nrow.m1), i] <- rwl.ord[, i]
  }
  colnames(rwca) <- col.names
  ci <- dplR::common.interval(as.data.frame(rwca), type = c("both"))
  title(main, line = 2.5)

  mzuw <- colMeans(ci, na.rm = T)
  alter <- rwl.stats(rwl)[rwl.stats(rwl)[ ,1] %in% names(ci), c(1, 4)]

  plot(mzuw ~ alter[ ,2], ylim = c(0, 4), col = ifelse(names(ci) %in% subset,
                                                       'green', subset.color), pch = 19, ylab = 'mean annual growth in common interval',
       xlab = 'treeage', main = main)
  abline(lm(mzuw ~ alter[ ,2]))
}

#-------------------------
#stand_depth
#-------------------------


#' @title stand_depth
#' @description function adds a column containing the "stand_depth" to an existing
#'   crn object
#' @param crn chronology object as from dplR::chron()
#' @param rwl rwl object
#' @param stand integer vector of length = 2, marking the start and end of
#'   site/stand specification within the series identifier e.g. c(1, 3) for
#'   the stand "Abc" in the series identifier "AbcPA01a"
#' @return a crn object with the column stand.depth added
#' @export
#' @examples # no example available in the development version
stand_depth <- function(crn, rwl, stand = c(1, 3)){
  if (missing (crn)){
    stop('crn is missing')
  }
  if (missing (rwl)){
    stop('rwl is missing')
  }
  if(!all(rownames(crn) %in% rownames(rwl))){
    stop('rwl provided has less years than crn')
  }

  rwl.sub <- rwl[rownames(rwl) %in% rownames(crn), ]
  tmp <- sapply(1:nrow(rwl.sub), FUN = function(x)
    length(unique(substr(colnames(rwl.sub)[!is.na(rwl.sub[x, ])], stand[1], stand[2]))))
  rng <- as.numeric(range(as.character(rownames(crn))))

  out <- crn
  out$stand.depth <- tmp
  return(out)
}

#-------------------------
#stand_depth.plot
#-------------------------
#' @title stand_depth.plot
#' @description plots a chronology with underlaying sample depth and
#'   "stand depth" information
#' @param stand_depth_object an object derived from \code{\link{stand_depth}}
#' @param chrono column name or integer specifying the column index where
#'   the chronology is stored in the stand_depth_object
#' @param sample.depth column name or integer specifying the column index where
#'   the sample depth is stored in the stand_depth_object
#' @param stand.depth column name or integer specifying the column index where
#'   the stand depth is stored in the stand_depth_object
#' @param main optional plot title
#' @param col1 optional color for sample depth
#' @param col2 optional color for stand depth
#' @export
#' @examples
#' #will be added later
stand_depth.plot <- function(stand_depth_object, chrono = "crn",
                             sample.depth = 'samp.depth',
                             stand.depth = 'stand.depth', main = '',
                             col1 = 'red', col2 = 'orange'){
  original_par <- par()
  on.exit(par(original_par))
  par(mar = c(5, 4, 4, 7))

  #check input

  if (!(is.data.frame(stand_depth_object) && ncol(stand_depth_object) >= 3)){
    stop('input object provided is either no data.frame or with wrong dimensions')
  }

  # this test throws an error even with correct input - fix later
  # for (arg in c(chrono, sample.depth, stand.depth)){
  #   if(!((arg %in% colnames(stand_depth_object)) || ifelse(is.numeric(arg),
  #                                                          is.wholenumber(arg),FALSE))){
  #     stop('arguments chrono, sample.depth, stand.depth must be either a column
  #          name in stand_depth_object or an integer')
  #   }
  # }

  #ranges
  rng <- range(as.numeric(rownames(stand_depth_object)))
  xstart <- round_down(rng[1], to = 1000)
  xend <- round_up(rng[2], to = 1000)
  rng2 <- range(stand_depth_object[ ,stand.depth], na.rm = TRUE)
  rng3 <- range(stand_depth_object[ ,sample.depth], na.rm = TRUE)
  rng4 <- range(stand_depth_object[ ,chrono], na.rm = TRUE)
  #empty plot
  plot(NULL, axes = F, ylim = c(rng3[1], rng3[2]), xlim = c(rng[1], rng[2]),
       ylab = '', xlab = '', main = main) # ylim for sample depth
  axis(1, at = seq(xstart, xend, 10), las = 2)
  abline(v = seq(xstart, xend, 1), col = 'gray80', lty = 3)
  abline(v = seq(xstart, xend, 10), col = 'gray80', lty = 1)

  #plot sample depth
  polygon(c(rng[1], as.numeric(rownames(stand_depth_object)), rng[2]),
          c(0,stand_depth_object[ ,sample.depth],0), border = F,
          col = scales::alpha(col1, 0.3))
  axis(4)
  mtext('sample depth', 4, 2, col = col1)


  # plot stand depth
  par(new = T)
  plot(NULL, axes = F, ylim = c(rng2[1], rng2[2] * 2), xlim = c(rng[1], rng[2]),
       ylab = '', xlab = '')
  polygon(c(rng[1], as.numeric(rownames(stand_depth_object)), rng[2]),
          c(0, stand_depth_object[ ,stand.depth], 0), border = F,
          col = scales::alpha(col2, 0.3))
  axis(4, line = 3.5)
  mtext('nr. of stands', 4, 5.5, col = col2)

  #plot chrono
  par(new = T)
  plot(NULL, ylim = c(rng4[1], rng4[2]), xlim=c(rng[1], rng[2]),
       ylab = 'tree ring index', xaxt = 'n', xlab = 'years')
  lines(rownames(stand_depth_object), stand_depth_object[ ,chrono])
}

