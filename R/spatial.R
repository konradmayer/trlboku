#transform_projection-----------------------------------------------------------
#' @title transform_projection
#' @description Transforms to different projection by supplying an EPSG code.
#'   If object has no proj4string defined, old projection can be specified by
#'   argument epsg_old.
#'
#'   EPSG of common projections:
#'   \itemize{
#'     \item{WGS84: 4326}
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


# catchment --------------------------------------------------------------------
#' @title find upstream catchments
#' @description find all upstream catchments given coordinates of a point within
#'   a catchment covered by digHAO (digital version of the hydrological atlas of
#'   austria)
#' @param rivers_digHAO a SpatialLinesDataFrame imported with
#'   \code{\link[maptools]{readShapeLines}}, data covered by the digital version
#'   of the hydrological atlas of austria
#' @param catchments_digHAO a SpatialPolygonsDataFrame imported with
#'   \code{\link[maptools]{readShapePoly}}, data covered by the digital version
#'   of the hydrological atlas of austria
#' @param lonlat a numeric vector of length 2 with the longitue and latitude of a mappoint
#' @param proj4string the proj4string of the provided lonlat
#' @param make.plot logic
#' @param ... additional plotting arguments such as plot title etc.
#' @references
#'   Fürst, J. (2008): Der digitale Hydrologische Atlas Österreichs.
#'   Der Hydrologische Atlas Österreichs: Anwendung in Wasserwirtschaft und
#'   Umweltschutz, 1. Juli 2008, Universität für Bodenkultur Wien
#' @return a list with index values as first and HYDROIDs as second member
#' @export
#'
#' @examples
#' \dontrun{
#'  path_digHAO <- 'ENTER PATH TO YOUR digHAO DIRECTORY HERE'
#'
#'  rivers <- maptools::readShapeLines(paste0(path_digHAO,
#'  '/Shape/Gemeinsam/gew1mio.shp')) # Lambert-projection Austria
#'  sp::proj4string(rivers) <- sp::CRS("+init=epsg:31287")
#'
#'  catchments <- maptools::readShapePoly(paste0(path_digHAO,
#'  '/Shape/1_3/wasserbilanz.shp')) # Lambert-projection Austria
#'  sp::proj4string(catchments) <- sp::CRS("+init=epsg:31287")
#'
#'  lonlat <- c(16, 48.5)
#'  catchment(catchments, lonlat, proj4string = sp::CRS("+init=epsg:4326"),
#'  make.plot = TRUE, rivers_digHAO = rivers)
#'  }

catchment <- function(catchments_digHAO, lonlat,
                      proj4string = sp::CRS("+init=epsg:4326"),
                      make.plot = TRUE, rivers_digHAO, ...) {

  if(length(catchments_digHAO) == 0 || class(catchments_digHAO) != "SpatialPolygonsDataFrame"){
    stop('please provide digHAO data of the catchments as SpatialPolygonsDataFrame')
  }

  if (!is.numeric(lonlat) || length(lonlat) != 2) {
    stop('please provide coordinates as a numeric vector of lenth 2')
  }

  if(!class(proj4string) == "CRS"){
    stop('please provide a valid CRS object as proj4string')
  }

  if(make.plot && (length(rivers_digHAO) == 0 || class(rivers_digHAO) != "SpatialLinesDataFrame")) {
    stop('to make a plot please provide digHAO data of the rivers as SpatialLinesDataFrame')
  }

  # transform point to Lambert-projection Austria
  point <- sp::SpatialPoints(t(as.matrix(lonlat)), proj4string = proj4string)
  point <- sp::spTransform(point, sp::CRS("+init=epsg:31287"))

  sub <- sp::over(point, catchments_digHAO)
  start <- sub$HYDROID

  x <- which(catchments_digHAO@data$NEXTDOWNID == start)
  startidx <- which(catchments_digHAO@data$HYDROID == start)
  if ( length(x) > 0 ){
    y <- x
    while (length(x) > 0){
      x <- which(catchments_digHAO@data$NEXTDOWNID %in% catchments_digHAO@data$HYDROID[x])
      y <- c(x, y)
    }

    hydids <- c(start, catchments_digHAO[y,]@data$HYDROID)
    idx <- c(startidx, y)

    if (make.plot){
      sp::plot(rivers_digHAO, col = ifelse(rivers_digHAO$STAAT == 4,scales::alpha('blue', 0.9),
                                           'transparent'),
               lwd = ifelse(rivers_digHAO$STAAT == 4, rivers_digHAO@data$BREITE, 0), ...)
      sp::plot(point, add = TRUE, pch = 20, col = 'red')
      sp::plot(catchments_digHAO[catchments_digHAO@data$HYDROID %in% hydids, ], add = TRUE,
               col = scales::alpha('red', 0.4), border = 'transparent')
    }

    return (list(idx = idx, HYDROID = hydids))

  }else{
    print('no preceding catchment found')
  }

}

# catchment_convenient ---------------------------------------------------------------
#' @title find upstream catchments
#' @description convenient, but slower, version of \code{\link{catchment}} where
#'   only the path to the digHAO folder needs to be supplied. If you want to find
#'   catchments for multiple points in a loop use \code{\link{catchment}} instead
#'   as catchment_convenient() loads the digHAO data at each function call.
#'
#' @param path_digHAO path to the digHAO directory as character string
#' @inheritParams catchment
#' @return a list with index values as first and HYDROIDs as second member
#' @export
#' @references
#'   Fürst, J. (2008): Der digitale Hydrologische Atlas Österreichs.
#'   Der Hydrologische Atlas Österreichs: Anwendung in Wasserwirtschaft und
#'   Umweltschutz, 1. Juli 2008, Universität für Bodenkultur Wien
#' @examples
#' \dontrun{
#'  path_digHAO <- 'ENTER PATH TO YOUR digHAO DIRECTORY HERE'
#'
#'  lonlat <- c(16, 48.5)
#'  catchment_convenient(path_digHAO, lonlat)
#'  }
catchment_convenient <- function(path_digHAO, lonlat,
                           proj4string = sp::CRS("+init=epsg:4326"),
                           make.plot = TRUE, ...) {

  if (!dir.exists(path_digHAO)) {
    stop('please provide valid path to digHAO')
  }

  if (!is.numeric(lonlat) || length(lonlat) != 2) {
    stop('please provide coordinates as a numeric vector of lenth 2')
  }

  if (make.plot) {
  rivers <- maptools::readShapeLines(paste0(path_digHAO,
                                            '/Shape/Gemeinsam/gew1mio.shp')) # Lambert-projection Austria
  sp::proj4string(rivers) <- sp::CRS("+init=epsg:31287")
  }

  catchments <- maptools::readShapePoly(paste0(path_digHAO,
                                               '/Shape/1_3/wasserbilanz.shp')) # Lambert-projection Austria
  sp::proj4string(catchments) <- sp::CRS("+init=epsg:31287")

  # transform point to Lambert-projection Austria
  point <- sp::SpatialPoints(t(as.matrix(lonlat)), proj4string = proj4string)
  point <- sp::spTransform(point, sp::CRS("+init=epsg:31287"))

  sub <- sp::over(point, catchments)
  start <- sub$HYDROID

  x <- which(catchments@data$NEXTDOWNID == start)
  startidx <- which(catchments@data$HYDROID == start)
  if ( length(x) > 0 ){
    y <- x
    while (length(x) > 0){
      x <- which(catchments@data$NEXTDOWNID %in% catchments@data$HYDROID[x])
      y <- c(x, y)
    }

    hydids <- c(start, catchments[y,]@data$HYDROID)
    idx <- c(startidx, y)

    if (make.plot){
      sp::plot(rivers, col = ifelse(rivers$STAAT == 4,scales::alpha('blue', 0.9),
                                    'transparent'),
               lwd = ifelse(rivers$STAAT == 4, rivers@data$BREITE, 0), ...)
      sp::plot(point, add = TRUE, pch = 20, col = 'red')
      sp::plot(catchments[catchments@data$HYDROID %in% hydids, ], add = TRUE,
               col = scales::alpha('red', 0.4), border = 'transparent')
    }

    return (list(idx = idx, HYDROID = hydids))

  }else{
    print('no preceding catchment found')
  }

}
