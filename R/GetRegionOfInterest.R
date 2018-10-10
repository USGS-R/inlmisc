#' Get Region of Interest
#'
#' This function calculates the spatial polygon describing
#' the convex hull of a set of spatial points.
#'
#' @param obj 'SpatialPoints*'.
#'   Spatial points
#'
#' @return Returns an object of class 'SpatialPolygons'.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link[grDevices]{chull}}
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' pts <- sp::SpatialPoints(cbind(x = stats::runif(50), y = stats::runif(50)))
#' ply <- GetRegionOfInterest(pts)
#' sp::plot(pts, col = "red")
#' sp::plot(ply, add = TRUE)
#'

GetRegionOfInterest <- function(obj) {
  checkmate::assertClass(obj, "SpatialPoints")
  pts <- obj[grDevices::chull(sp::coordinates(obj)), ]
  ply <- sp::Polygons(list(sp::Polygon(pts)), ID=1)
  sp::SpatialPolygons(list(ply), proj4string=raster::crs(obj))
}
