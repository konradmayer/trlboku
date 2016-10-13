#------------------------------
#new_epsg
#------------------------------
#' @title new_epsg
#' @description Set new proj4string by supplying a EPSG code.
#'
#' @param spatial_object an object of class "Spatial".
#' @param epsg_new an EPSG code provided as character string.
#' @param epsg_old optional, an EPSG code provided as character string.
#'
#' @return an object of class "Spatial".
#' @export
new_epsg <- function(spatial_object, epsg_new, epsg_old = NULL){
  #add argument checks here - how to test for S4 class "Spatial"?

  if(!is.null(epsg_old)){
    proj4string(spatial_object) <- CRS(paste0("+init=epsg:", epsg_old))
  }

  CRS.new <- CRS(paste0("+init=epsg:", epsg_new))
  out <- spTransform(spatial_object, CRS.new)
  return(out)
}
