#' Get Region of Interest
#'
#' Create a spatial polygon describing the convex hull of a set of spatial points.
#'
#' @param obj 'SpatialPoints*'.
#'   Spatial points
#' @param alpha 'numeric' number.
#'   Value of alpha, used to implement a generalization of the convex hull
#'   (Edelsbrunner and others, 1983).
#'   Requires that the \pkg{alphahull} package is available.
#' @param width 'numeric' number.
#'   Buffer distance from geometry of convex hull.
#' @param ...
#'   Additional arguments to be passed to the \code{\link[rgeos]{gBuffer}} function.
#'
#' @return Returns an object of class 'SpatialPolygons'.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @references
#'   Edelsbrunner, H., Kirkpatrick, D.G. and Seidel, R., 1983,
#'   On the shape of a set of points in the plane:
#'   IEEE Transactions on Information Theory, v. 29, no. 4, p. 551--559.
#'
#' @seealso \code{\link[grDevices]{chull}}, \code{\link[alphahull]{ashape}}
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' pts <- sp::SpatialPoints(cbind(x = stats::runif(50), y = stats::runif(50)))
#' sp::plot(GetRegionOfInterest(pts, width = 0.05), border = "red", lty = 2)
#' sp::plot(GetRegionOfInterest(pts), border = "red", add = TRUE)
#' sp::plot(GetRegionOfInterest(pts, alpha = 0.5), border = "blue", add = TRUE)
#' sp::plot(pts, add = TRUE)
#'

GetRegionOfInterest <- function(obj, alpha=NULL, width=0, ...) {
  checkmate::assertClass(obj, "SpatialPoints")
  checkmate::assertNumber(alpha, lower=0, finite=TRUE, null.ok=TRUE)
  checkmate::assertNumber(width, finite=TRUE)

  coords <- sp::coordinates(obj)
  if (is.null(alpha))
    pts <- obj[grDevices::chull(coords), ]
  else
    pts <- .GeneralizeConvexHull(coords, alpha)

  ply <- sp::Polygons(list(sp::Polygon(pts)), ID=1)
  ply <- sp::SpatialPolygons(list(ply), proj4string=raster::crs(obj))
  ply <- rgeos::gBuffer(ply, width=width, ...)
  ply
}


# Compute alpha-shape of points in plane

.GeneralizeConvexHull <- function(coords, alpha) {

  if (!requireNamespace("alphahull", quietly=TRUE))
    stop("alpha-shape computation requires the alphahull package")

  # code adapted from RPubs post by Barry Rowlingson,
  # accessed November 15, 2018 at https://rpubs.com/geospacedman/alphasimple

  shp <- alphahull::ashape(coords, alpha=alpha)
  el <- cbind(as.character(shp$edges[, "ind1"]), as.character(shp$edges[, "ind2"]))
  gr <- igraph::graph_from_edgelist(el, directed=FALSE)
  if (!igraph::is.connected(gr))    stop("disconnected polygon")
  if (any(igraph::degree(gr) != 2)) stop("non-circular polygon")
  if (igraph::clusters(gr)$no > 1)  stop("multiple polygons")
  grc <- gr - igraph::E(gr)[1]
  ends <- names(which(igraph::degree(grc) == 1))
  path <- igraph::shortest_paths(grc, ends[1], ends[2])$vpath[[1]]
  idxs <- as.numeric(igraph::V(gr)[path]$name)
  shp$x[c(idxs, idxs[1]), ]
}
