#transform_projection-----------------------------------------------------------
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
#' @examples \dontrun{
#' #library('sp')
#' init_dataframe <- data.frame(Y = c(48.6476, 48.6447, 48.6709),
#'                              X = c(16.7958, 6.7962, 16.5370),
#'                              meta = c('a', 'b', 'c'))
#' spatpoints <- SpatialPointsDataFrame(init_dataframe[c('X', 'Y')],
#'                                        init_dataframe['meta'])
#'
#' ## using other_projection():
#' spatpoints_lambert <- transform_projection(spatial_object, '31287',
#' epsg_old = '4326')
#'
#' ## using "manual approach":
#' # specify current projstring as WGS 84
#' proj4string(spatpoints) <- CRS("+init=epsg:4326")
#' # creator function for CRS class - set as Lambert-projection Austria
#' CRS.new <- CRS("+init=epsg:31287")
#' #transform spatialpointsto new projection
#' spatpoints_lambert2 <- spTransform(spatpoints, CRS.new)
#'
#' #they are the same
#' identical(spatpoints_lambert, spatpoints_lambert2)
#' }

transform_projection <- function(spatial_object, epsg_new, epsg_old = NULL) {
  #add argument checks here - how to test for S4 class "Spatial"?
  stopifnot(is.character(epsg_new) &&
              (is.character(epsg_old) || is.null(epsg_old)))

  if(!is.null(epsg_old)) {
    sp::proj4string(spatial_object) <- sp::CRS(paste0("+init=epsg:", epsg_old))
  }

  CRS.new <- sp::CRS(paste0("+init=epsg:", epsg_new))
  out <- sp::spTransform(spatial_object, CRS.new)
  return(out)
}
