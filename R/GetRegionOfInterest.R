#' Get Region of Interest
#'
#' Create a spatial polygon describing the convex hull of a set of spatial points.
#'
#' @param obj 'SpatialPoints*'.
#'   Spatial points
#' @param alpha 'numeric' number.
#'   Value of alpha, used to implement a generalization of the convex hull
#'   (Edelsbrunner and others, 1983).
#'   Requires that the \pkg{alphahull} and \pkg{maptools} packages are available.
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
#' n <- 300
#' set.seed(321)
#' theta <- stats::runif(n, 0, 2 * pi)
#' r <- sqrt(stats::runif(n, 0.25^2, 0.5^2))
#' x <- cbind(0.5 + r * cos(theta), 0.5 + r * sin(theta))
#' pts <- sp::SpatialPoints(x)
#' sp::plot(GetRegionOfInterest(pts, alpha = 0.1, width = 0.05), col = "green")
#' sp::plot(GetRegionOfInterest(pts, alpha = 0.1), col = "yellow", add = TRUE)
#' sp::plot(pts, add = TRUE)
#'

GetRegionOfInterest <- function(obj, alpha=NULL, width=0, ...) {
  checkmate::assertClass(obj, "SpatialPoints")
  checkmate::assertNumber(alpha, lower=0, finite=TRUE, null.ok=TRUE)
  checkmate::assertNumber(width, finite=TRUE)

  coords <- sp::coordinates(obj)
  if (is.null(alpha)) {
    pts <- obj[grDevices::chull(coords), ]
    ply <- sp::Polygons(list(sp::Polygon(pts)), ID=1)
  } else {
    ply <- .GeneralizeConvexHull(coords, alpha)
  }
  ply <- sp::SpatialPolygons(list(ply), proj4string=raster::crs(obj))

  rgeos::gBuffer(ply, width=width, ...)
}


# Compute alpha-shape of points in plane

.GeneralizeConvexHull <- function(coords, alpha) {

  checkmate::assertMatrix(coords, mode="numeric", any.missing=FALSE,
                          min.rows=3, ncols=2)
  checkmate::assertNumber(alpha, lower=0, finite=TRUE)

  for (pkg in c("alphahull", "maptools")) {
    if (!requireNamespace(pkg, quietly=TRUE))
      stop(sprintf("alpha-shape computation requires the %s package", pkg))
  }

  # code adapted from RPubs post by Barry Rowlingson,
  # accessed November 15, 2018 at https://rpubs.com/geospacedman/alphasimple
  shp <- alphahull::ashape(coords, alpha=alpha)
  el <- cbind(as.character(shp$edges[, "ind1"]), as.character(shp$edges[, "ind2"]))
  gr <- igraph::graph_from_edgelist(el, directed=FALSE)
  clu <- igraph::components(gr, mode="strong")
  ply <- lapply(seq_len(clu$no), function(i) {
    vids <- igraph::groups(clu)[[i]]
    g <- igraph::induced_subgraph(gr, vids)
    if (any(igraph::degree(g) != 2)) return(NULL)
    gcut <- g - igraph::E(g)[1]
    ends <- names(which(igraph::degree(gcut) == 1))
    path <- igraph::shortest_paths(gcut, ends[1], ends[2])$vpath[[1]]
    idxs <- as.integer(igraph::V(g)[path]$name)
    pts <- shp$x[c(idxs, idxs[1]), ]
    sp::Polygon(pts)
  })

  ply <- ply[!(is <- vapply(ply, is.null, TRUE))]
  if (length(ply) == 0) stop("non-circular polygons")
  if (any(is)) warning("removed non-circular polygons")

  ply <- sp::Polygons(ply, ID=1)
  ply <- maptools::checkPolygonsHoles(ply)
  ply
}
