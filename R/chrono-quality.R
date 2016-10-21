#-------------------------
#agegrowthplot
#-------------------------
#' @title agegrowthplot
#' @description Plots mean annual growth rate in a common interval (shown in
#'   first plot) over tree age - a subset (eg. historic vs. recent trees,
#'   a different site ect.) can be specified to get a different color in the plot.
#' @param rwl an rwl object.
#' @param po a data frame with series names in the first and pith offset values
#'   (number of rings) in the second column.
#' @param subset provide a character string of names in rwl which get
#'   a different color in the last plot.
#' @param subset.color color of the subset.
#' @param main optional plot title.
#' @export
#' @examples
#' library("dplR")
#' data("gp.rwl")
#' data("gp.po")
#' gp.po[ ,1] <- as.character(gp.po[ ,1])
#' sub <- names(gp.rwl)[substr(names(gp.rwl), 3, 3) == "B"]
#' agegrowthplot(gp.rwl, gp.po, subset = sub, main = 'my age-growth-plot')
agegrowthplot <- function(rwl, po, subset = NULL, subset.color = 'violet', main = '') {
  #check arguments
  if (!is.data.frame(rwl)) {
    stop("'rwl' must be a data.frame")
  }

  if(!((dim(po)[2] == 2) && is.numeric(po[,2]) && is.character(po[,1]))) {
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
#' @description This function adds a column containing the "stand_depth" to
#'   an existing crn object.
#' @param crn chronology object as from dplR::chron().
#' @param rwl rwl object.
#' @param stand integer vector of length = 2, marking the start and end of
#'   site/stand specification within the series identifier e.g. c(1, 3) for
#'   the stand "Abc" in the series identifier "AbcPA01a".
#' @return a crn object with the column stand.depth added.
#' @export
#' @examples # no example available in the development version
stand_depth <- function(crn, rwl, stand = c(1, 3)) {
  if (missing (crn)) {
    stop('crn is missing')
  }

  if (!is.data.frame(crn)) {
    stop("'crn' must be a data.frame")
  }

  if (missing (rwl)) {
    stop('rwl is missing')
  }

  if (!is.data.frame(rwl)) {
    stop("'rwl' must be a data.frame")
  }

  if(!all(rownames(crn) %in% rownames(rwl))) {
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
#stand_depth_plot
#-------------------------
#' @title stand_depth_plot
#' @description Plots a chronology with underlaying sample depth and
#'   "stand depth" information.
#' @param stand_depth_object an object derived from \code{\link{stand_depth}}.
#' @param chrono column name or integer specifying the column index where
#'   the chronology is stored in the stand_depth_object.
#' @param sample.depth column name or integer specifying the column index where
#'   the sample depth is stored in the stand_depth_object.
#' @param stand.depth column name or integer specifying the column index where
#'   the stand depth is stored in the stand_depth_object.
#' @param main optional plot title.
#' @param col1 optional color for sample depth.
#' @param col2 optional color for stand depth.
#' @export
#' @examples
#' #will be added later
stand_depth_plot <- function(stand_depth_object, chrono = "crn",
                             sample.depth = 'samp.depth',
                             stand.depth = 'stand.depth', main = '',
                             col1 = 'red', col2 = 'orange') {
  original_par <- par()
  on.exit(par(original_par))
  par(mar = c(5, 4, 4, 7))

  #check input

  if (!(is.data.frame(stand_depth_object) && ncol(stand_depth_object) >= 3)) {
    stop('input object provided is either no data.frame or with wrong dimensions')
  }

  for (arg in c(chrono, sample.depth, stand.depth)) {
    if(!((arg %in% colnames(stand_depth_object)) || arg %in% seq_along(stand_depth_object))) {
      stop('arguments chrono, sample.depth, stand.depth must be either a column
          name in stand_depth_object or an integer')
    }
  }

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

#--------------------------
#radius_class_plot
#--------------------------

#' @title Plot median growth rate or other parameters per radius class
#' @description Plot median growth rate or other parameters per radius class to
#'   evaluate trends in tree ring data and data composition.
#' @param dat data of the investigated tree ring parameter (ring width,
#'   earlywood width, leatewood with, ring density, etc.).
#' @param rwdata ring width data to approximate the radius of individual trees.
#' @param cutoff cut the data at given year - if NULL the whole range is used.
#' @param classborders radii to set the borders of the radius classes.
#' @param main plot title.
#' @param unit y-axis label.
#' @export
#'
#' @examples #not available in development version.
radius_class_plot <- function (dat, rwdata, cutoff = NULL,
                               classborders = c(0, 50, 80, 100, 150, 200, Inf),
                               main = '', unit = '[mm]'){

  if (!is.data.frame(dat) || !is.data.frame(rwdata)) {
    stop('dat and rwdata have to be of class data.frame')
  }

  if(!is.numeric(classborders)){
    stop('please provide numeric classborders')
  }

  these_series <- intersect(names(dat), names(rwdata))
  series_not_in_rwdata <- setdiff(names(dat), names(rwdata))

  if(length(series_not_in_rwdata) > 0) {
    warning("Some series of dat cannot be found in rwdata and are therefore not
included in calculations:", series_not_in_rwdata, collapse = ', ')
  }

  if (is.null(cutoff)) {
    cutoff <- as.numeric(rownames(dat)[1] - 1)
  }

  dat <- dat[as.numeric(rownames(dat)) > as.numeric(cutoff[1]), these_series]
  rwdata <- rwdata[as.numeric(rownames(rwdata)) > as.numeric(cutoff[1]),
                   these_series]

  years <- rownames(dat)
  cum_rw <- radius_rwl(rwdata)
  growthrate <- expand_apply.data.frame(dat, 'median')

  #find radius class
  rclass <- lapply(seq_along(years), function(x)
    cut(as.numeric(cum_rw[x, ]), breaks = classborders))

  #calculate groth rate per class ans sample depth per class
  growth_class <- as.data.frame(
    Reduce('rbind', lapply(seq_along(years), function(i) {
      tapply(as.numeric(growthrate[i, ]), rclass[i], 'median')
    })))
  rownames(growth_class) <- years

  depth <- as.data.frame(
    Reduce('rbind', lapply(seq_along(years), function(i) {
      tapply(as.numeric(growthrate[i, ]), rclass[i], 'length')
    })))
  rownames(depth) <- years



  #plot median growth per class
  par(mfrow = c(2,1), mar = c(3,5,2,5))
  plot(NULL, xlim = range(as.numeric(years)),
       ylim = c(0, max(growth_class, na.rm = TRUE)), main = main, ylab = unit)

  lapply(seq_along(growth_class), FUN = function(x) {
    lines(years, growth_class[ , x], col = x, lwd = 2)
  })

  legend('top', lty = 1, lwd = 3,col = seq_along(growth_class),
         legend = names(growth_class), ncol = ncol(growth_class),
         bty = 'n', cex = 0.7)


  #plot sample depth per class
  plot(NULL, xlim = range(as.numeric(years)), ylim  = c(0, max(depth, na.rm=T)),
       ylab = 'count')
  title('sample depth', cex.main = 0.8)

  lapply(seq_along(depth), FUN = function(x) {
    lines(years, depth[ , x], col = x, lwd = 2)
  })

  legend('top', lty = 1, lwd = 3, col = seq_along(depth),
         legend = names(depth), ncol = ncol(depth), bty = 'n', cex = 0.7)
}

#--------------------------
#age_class_plot
#--------------------------

#' @title Plot median growth rate or other parameters per age class
#' @description Plot median growth rate or other parameters per age class to
#'   evaluate trends in tree ring data and data composition.
#' @param dat data of the investigated tree ring parameter (ring width,
#'   earlywood width, leatewood with, ring density, etc.).
#' @param cutoff cut the data at given year - if NULL the whole range is used.
#' @param classborders cambial ages to set the borders of the age classes.
#' @param main plot title.
#' @param unit y-axis label.
#' @export
#'
#' @examples #not available in development version.
age_class_plot <- function (dat, cutoff = NULL,
                               classborders = c(0, 60, 80, 100, 150, Inf),
                               main = '', unit = '[mm]'){

  if (!is.data.frame(dat)) {
    stop('dat has to be of class data.frame')
  }

  if(!is.numeric(classborders)){
    stop('please provide numeric classborders')
  }

  if (is.null(cutoff)) {
    cutoff <- as.numeric(rownames(dat)[1] - 1)
  }

  dat <- dat[as.numeric(rownames(dat)) > as.numeric(cutoff[1]), ]

  years <- rownames(dat)
  cambial_age <- age_rwl(dat)
  growthrate <- expand_apply.data.frame(dat, 'median')

  #find age class
  aclass <- lapply(seq_along(years), function(x)
    cut(as.numeric(cambial_age[x, ]), breaks = classborders))

  #calculate groth rate per class ans sample depth per class
  growth_class <- as.data.frame(
    Reduce('rbind', lapply(seq_along(years), function(i) {
      tapply(as.numeric(growthrate[i, ]), aclass[i], 'median')
    })))
  rownames(growth_class) <- years

  depth <- as.data.frame(
    Reduce('rbind', lapply(seq_along(years), function(i) {
      tapply(as.numeric(growthrate[i, ]), aclass[i], 'length')
    })))
  rownames(depth) <- years



  #plot median growth per class
  par(mfrow = c(2,1), mar = c(3,5,2,5))
  plot(NULL, xlim = range(as.numeric(years)),
       ylim = c(0, max(growth_class, na.rm = TRUE)), main = main, ylab = unit)

  lapply(seq_along(growth_class), FUN = function(x) {
    lines(years, growth_class[ , x], col = x, lwd = 2)
  })

  legend('top', lty = 1, lwd = 3,col = seq_along(growth_class),
         legend = names(growth_class), ncol = ncol(growth_class),
         bty = 'n', cex = 0.7)


  #plot sample depth per class
  plot(NULL, xlim = range(as.numeric(years)), ylim  = c(0, max(depth, na.rm=T)),
       ylab = 'count')
  title('sample depth', cex.main = 0.8)

  lapply(seq_along(depth), FUN = function(x) {
    lines(years, depth[ , x], col = x, lwd = 2)
  })

  legend('top', lty = 1, lwd = 3, col = seq_along(depth),
         legend = names(depth), ncol = ncol(depth), bty = 'n', cex = 0.7)
  }
