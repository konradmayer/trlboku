#' trlboku - tree ring lab boku - is a development version package for R
#'
#' This package holds a collection of functions and data used at the tree ring
#' lab at BOKU, University of Natural Resources an Life Sciences, for internal
#' use. The package is not intended to address an explicit topic but rather
#' gathers functions related to dendroclimatology, data manipulation,
#' dendrochronological dating and so on.
#'
#' @section Functions supporting the lab workflow:
#'   new_date_end(),
#'   rwl_subout(),
#'   plot_bericht()
#'
#' @section Functions related to chronology quality measures and plots:
#'   agegrowthplot(),
#'   stand_depth(),
#'   stand_depth_plot(),
#'   radius_class_plot(),
#'   age_class_plot(),
#'   scatter_rwl()
#'
#' @section Functions for dendroclimatology:
#'   po_transform(),
#'   po_find(),
#'   to_cambial_age(),
#'
#' @section other plotting functions:
#'   panel.cor(),
#'   init_barplot(),
#'   lines_barplot(),
#'   rect_barplot(),
#'   filled_contour(),
#'
#'
#' @docType package
#' @name trlboku
#'
#'
#' @import stats
#' @import graphics
#' @import grDevices
#' @import utils
#'
NULL
