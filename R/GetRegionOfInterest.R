#' Get Region of Interest
#'
#' Create a spatial polygon describing the convex hull of a set of spatial points.
#'
#' @param obj 'SpatialPoints*'.
#'   Spatial points
#' @param buffer 'numeric' number.
#'   Buffer distance from convex hull.
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
#' sp::plot(GetRegionOfInterest(pts, buffer = 0.05), lty = 2)
#' sp::plot(GetRegionOfInterest(pts), add = TRUE)
#' sp::plot(pts, col = "red", add = TRUE)
#'

GetRegionOfInterest <- function(obj, buffer=0) {
  checkmate::assertClass(obj, "SpatialPoints")
  checkmate::assertNumber(buffer, finite=TRUE)

  pts <- obj[grDevices::chull(sp::coordinates(obj)), ]
  ply <- sp::Polygons(list(sp::Polygon(pts)), ID=1)
  ply <- sp::SpatialPolygons(list(ply), proj4string=raster::crs(obj))
  ply <- rgeos::gBuffer(ply, width=buffer)
  ply
}
