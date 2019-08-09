#' Overlay Multi-Polygon Objects
#'
#' Calculate the intersection or difference between two multi-polygon objects.
#'
#' @param x 'SpatialPolygons*'.
#'   Multi-polygon object
#' @param y 'SpatialPolygons*' or 'Extent'.
#'   Multi-polygon object
#' @param cmd 'character' string.
#'   Specifying "gIntersection", the default, cuts out portions of the
#'   \code{x} polygons that overlay the \code{y} polygons.
#'   If "gDifference" is specified, only those portions of the \code{x} polygons
#'   falling outside the \code{y} polygons are copied to the output polygons.
#' @param buffer.width 'numeric' number.
#'   Expands or contracts the geometry of \code{y} to include the area within
#'   the specified width, see \code{gBuffer}.
#'   Specifying \code{NA}, the default, indicates no buffer.
#'
#' @details This function tests if the resulting geometry is valid.
#' If invalid, an attempt is made to make the geometry valid by zero-width buffering.
#'
#' @return Returns an object of class 'SpatialPolygons*'.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{gIntersection}}, \code{\link{gDifference}}
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' m1a <- rbind(c(17.5, 55.1), c(24.7, 55.0), c(22.6, 61.1),
#'              c(16.5, 59.7), c(17.5, 55.1))
#' m1b <- m1a
#' m1b[, 1] <- m1b[, 1] + 11
#' p1 <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(m1a, FALSE),
#'                                                  sp::Polygon(m1b, FALSE)), 1)))
#' sp::plot(p1, col = "blue")
#'
#' m2a <- rbind(c(19.6, 60.0), c(35.7, 58.8), c(28.2, 64.4), c(19.6, 60.0))
#' m2b <- rbind(c(20.6, 56.2), c(30.9, 53.8), c(27.3, 51.4), c(20.6, 56.2))
#' p2 <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(m2a, FALSE),
#'                                                  sp::Polygon(m2b, FALSE)), 2)))
#' sp::plot(p2, col = "red", add = TRUE)
#'
#' p <- SetPolygons(p1, p2, "gIntersection")
#' sp::plot(p, col = "green", add = TRUE)
#'
#' p <- SetPolygons(p2, p1, "gDifference")
#' sp::plot(p, col = "purple", add = TRUE)
#'

SetPolygons <- function(x, y, cmd=c("gIntersection", "gDifference"), buffer.width=NA) {

  stopifnot(inherits(x, c("SpatialPolygons", "SpatialPolygonsDataFrame")))
  stopifnot(inherits(y, c("SpatialPolygons", "SpatialPolygonsDataFrame", "Extent")))
  cmd <- match.arg(cmd)
  checkmate::assertNumber(buffer.width, na.ok=TRUE, finite=TRUE)

  if (inherits(y, "Extent")) {
    crds <- cbind(c(y[1:2], y[2:1], y[1]), c(rep(y[3], 2), rep(y[4], 2), y[3]))
    y <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(crds)), "bbox")),
                             proj4string=raster::crs(x))
  }

  if (inherits(x, "SpatialPolygonsDataFrame")) {
    d <- x@data
    rownames(d) <- sapply(methods::slot(x, "polygons"), function(i) methods::slot(i, "ID"))
  } else {
    d <- NULL
  }

  x <- methods::as(x, "SpatialPolygons")
  y <- methods::as(y, "SpatialPolygons")
  y <- y[which(apply(rgeos::gIntersects(y, x, byid=TRUE), 2, any)), ]

  is_intersecting <- rgeos::gIntersects(x, y, byid=TRUE)

  z <- lapply(seq_along(x), function (i) {

    if (any(is_intersecting[, i])) {
      y_intersect <- y[is_intersecting[, i]]
      if (is.numeric(buffer.width))
        y_intersect <- rgeos::gBuffer(y_intersect, width=buffer.width)

      suppressMessages(suppressWarnings({
        spgeom2 <- rgeos::gUnaryUnion(y_intersect, checkValidity=2L)
        if (cmd == "gIntersection")
          x_geo <- rgeos::gIntersection(x[i], spgeom2, byid=TRUE, checkValidity=2L)
        else
          x_geo <- rgeos::gDifference(x[i], spgeom2, byid=TRUE, checkValidity=2L)
        if (inherits(x_geo, "SpatialCollections"))
          x_geo <- rgeos::gUnaryUnion(x_geo@polyobj, checkValidity=2L)
      }))

      p <- x_geo@polygons[[1]]
      methods::slot(p, "ID") <- methods::slot(x[i]@polygons[[1]], "ID")

    } else {
      p <- if (cmd == "gIntersection") NULL else x[i]@polygons[[1]]
    }

    p
  })

  is_retained <- !vapply(z, is.null, TRUE)
  z <- sp::SpatialPolygons(z[is_retained], proj4string=raster::crs(x))
  if (inherits(d, "data.frame")) {
    d <- d[is_retained, , drop=FALSE]
    z <- sp::SpatialPolygonsDataFrame(z, d, match.ID=TRUE)
  }

  z
}
