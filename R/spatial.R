#------------------------------
#other_projection
#------------------------------
#' @title transform_projection
#' @description Transforms to different projection by supplying an EPSG code.
#'   If object has no proj4string defined, old projection can be specified by
#'   argument epsg_old.
#'
#'   EPSG of common projections:
#'   \itemize{
#'     \item{WG84: 4326}
#'     \item{Lambert Austria: 31287}
#'   }
#' @param spatial_object an object of class "Spatial".
#' @param epsg_new an EPSG code provided as character string indicating the
#'   desired projection for the returned object.
#' @param epsg_old optional, an EPSG code provided as character string
#'  specifying the projection before transformation if no proj4string is
#'  supplied with the object.
#'
#' @return an object of class "Spatial".
#' @export
transform_projection <- function(spatial_object, epsg_new, epsg_old = NULL){
  #add argument checks here - how to test for S4 class "Spatial"?
  stopifnot(is.character(epsg_new) &&
              (is.character(epsg_old) || is.null(epsg_old)))

  if(!is.null(epsg_old)){
    proj4string(spatial_object) <- CRS(paste0("+init=epsg:", epsg_old))
  }

  CRS.new <- CRS(paste0("+init=epsg:", epsg_new))
  out <- spTransform(spatial_object, CRS.new)
  return(out)
}
