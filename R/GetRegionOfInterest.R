#' Get Region of Interest
#'
#' Create a spatial polygon describing the convex hull of a set of spatial points.
#'
#' @param obj 'matrix', 'data.frame', or 'SpatialPoints*'.
#'   Sample of points specified as either a 2-column numeric matrix with coordinates
#'   or a spatial object that coordinates can be retrieved from.
#' @param alpha 'numeric' number.
#'   Value of \eqn{\alpha}, used to implement a generalization of the convex hull
#'   (Edelsbrunner and others, 1983).
#'   As \eqn{\alpha} decreases, the shape shrinks.
#'   Requires that the \pkg{alphahull} and \pkg{maptools} packages are available.
#'   Note that the \href{https://CRAN.R-project.org/package=alphahull}{alphahull} package
#'   is released under a restrictive non-free software license.
#' @param width 'numeric' number.
#'   Buffer distance from geometry of convex hull.
#' @param ...
#'   Additional arguments to be passed to the \code{\link{gBuffer}} function.
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
#' @seealso \code{\link[grDevices]{chull}}, \code{\link[alphahull]{ashape}},
#'   \code{\link[maptools]{checkPolygonsHoles}}
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' set.seed(123)
#'
#' n <- 50
#' pts <- cbind(x = stats::runif(n), y = stats::runif(n))
#' sp::plot(GetRegionOfInterest(pts, width = 0.05), border = "blue", lty = 2)
#' sp::plot(GetRegionOfInterest(pts), border = "red", add = TRUE)
#' sp::plot(GetRegionOfInterest(pts, width = -0.05), lty = 2, add = TRUE)
#' points(pts, pch = 3)
#'
#' \dontrun{
#' n <- 300
#' theta <- stats::runif(n, 0, 2 * pi)
#' r <- sqrt(stats::runif(n, 0.25^2, 0.50^2))
#' pts <- sp::SpatialPoints(cbind(0.5 + r * cos(theta), 0.5 + r * sin(theta)),
#'                          proj4string = sp::CRS("+init=epsg:32610"))
#' sp::plot(GetRegionOfInterest(pts, alpha = 0.1, width = 0.05), col = "green")
#' sp::plot(GetRegionOfInterest(pts, alpha = 0.1), col = "yellow", add = TRUE)
#' sp::plot(pts, add = TRUE)
#' }
#'

GetRegionOfInterest <- function(obj, alpha=NULL, width=0, ...) {
  stopifnot(inherits(obj, c("matrix", "data.frame", "SpatialPoints")))
  checkmate::assertNumber(alpha, lower=0, finite=TRUE, null.ok=TRUE)
  checkmate::assertNumber(width, finite=TRUE)

  if (inherits(obj, "SpatialPoints")) {
    coords <- sp::coordinates(obj)
    crs <- raster::crs(obj)
  } else {
    coords <- data.matrix(obj[, 1:2])
    crs <- sp::CRS(as.character(NA))
  }

  if (is.null(alpha)) {
    pts <- coords[grDevices::chull(coords), ]
    ply <- sp::Polygons(list(sp::Polygon(pts)), ID=1)
  } else {
    ply <- .GeneralizeConvexHull(coords, alpha)
  }
  ply <- sp::SpatialPolygons(list(ply), proj4string=crs)

  rgeos::gBuffer(ply, width=width, ...)
}


# Compute alpha-shape of points in plane

.GeneralizeConvexHull <- function(coords, alpha) {
  checkmate::assertMatrix(coords, mode="numeric", any.missing=FALSE, min.cols=2)
  checkmate::assertNumber(alpha, lower=0, finite=TRUE)

  for (pkg in c("alphahull", "maptools")) {
    if (!requireNamespace(pkg, quietly=TRUE))
      stop(sprintf("alpha-shape computation requires the %s package", pkg))
  }

  # remove duplicate points
  coords <- unique(coords[, 1:2])

  # code adapted from RPubs document by Barry Rowlingson,
  # accessed November 15, 2018 at https://rpubs.com/geospacedman/alphasimple
  shp <- alphahull::ashape(coords, alpha=alpha)
  el <- cbind(as.character(shp$edges[, "ind1"]), as.character(shp$edges[, "ind2"]))
  gr <- igraph::graph_from_edgelist(el, directed=FALSE)
  clu <- igraph::components(gr, mode="strong")
  ply <- sp::Polygons(lapply(seq_len(clu$no), function(i) {
    vids <- igraph::groups(clu)[[i]]
    g <- igraph::induced_subgraph(gr, vids)
    if (any(igraph::degree(g) != 2))
      stop("non-circular polygon, try increasing alpha value", call.=FALSE)
    gcut <- g - igraph::E(g)[1]
    ends <- names(which(igraph::degree(gcut) == 1))
    path <- igraph::shortest_paths(gcut, ends[1], ends[2])$vpath[[1]]
    idxs <- as.integer(igraph::V(g)[path]$name)
    pts <- shp$x[c(idxs, idxs[1]), ]
    sp::Polygon(pts)
  }), ID=1)

  maptools::checkPolygonsHoles(ply)
}
