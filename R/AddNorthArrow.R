#' Add North Arrow to Plot
#'
#' This function can be used to add a north arrow to a plot.
#'
#' @param crs 'CRS', 'Raster*', or 'Spatial'.
#'   Coordinate reference system (CRS), or any object with a CRS attribute.
#' @param len 'numeric'.
#'   Arrow length specified as a fraction of the plot height, 5-percent by default.
#' @param lab 'character'.
#'   North label is \dQuote{N} by default.
#' @param cex 'numeric'.
#'   Character expansion factor for the north label.
#' @param ...
#'   Additional arguments to be passed to the \code{\link{GetInsetLocation}} function---used
#'   to position the north arrow in the main plot region.
#'
#' @return Used for the side-effect of a north arrow drawn on the current graphics device.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{PlotMap}}
#'
#' @keywords hplot
#'
#' @export
#'
#' @examples
#' m <- datasets::volcano
#' m <- m[nrow(m):1, ncol(m):1]
#' x <- seq(from = 2667405, length.out = ncol(m), by = 10)
#' y <- seq(from = 6478705, length.out = nrow(m), by = 10)
#' r <- raster::raster(m, xmn = min(x), xmx = max(x), ymn = min(y), ymx = max(y),
#'                     crs = "+init=epsg:27200")
#' PlotMap(r, pal = terrain.colors)
#' AddNorthArrow(raster::crs(r), loc = "center")
#' AddNorthArrow(raster::crs(r), inset = 0.1)
#' AddNorthArrow(raster::crs(r), loc = "topleft", inset = 0.1)
#'

AddNorthArrow <- function(crs, len=0.05, lab="N", cex=0.7, ...) {

  stopifnot(inherits(crs, c("CRS", "RasterLayer", "Spatial")))
  checkmate::assertNumber(len, lower=0, upper=1, finite=TRUE)
  checkmate::assertString(lab)
  checkmate::assertNumber(cex, lower=0, finite=TRUE)

  crs <- raster::crs(crs)
  crs.dd <- sp::CRS("+init=epsg:4326")
  usr <- graphics::par("usr")
  x.mid <- (usr[2] + usr[1]) / 2
  y.mid <- (usr[4] + usr[3]) / 2
  d <- (usr[4] - usr[3]) * len
  pts <- rbind(c(x.mid, y.mid), c(x.mid, y.mid + d))

  sp.dd <- sp::spTransform(sp::SpatialPoints(pts, proj4string=crs), crs.dd)
  dd <- sp.dd@coords
  d.dd <- sqrt((dd[2, 1] - dd[1, 1])^2 + (dd[2, 2] - dd[1, 2])^2)
  dd <- rbind(dd[1, ], c(dd[1, 1],  dd[1, 2] + d.dd))
  sp.xy <- sp::spTransform(sp::SpatialPoints(dd, proj4string=crs.dd), crs)
  pts <- sp.xy@coords

  dx <- abs(diff(pts[, 1]))
  dy <- abs(diff(pts[, 2]))
  xy <- GetInsetLocation(dx, dy, ...)
  x0 <- xy["x"]
  y0 <- xy["y"]

  x1 <- pts[2, 1] + x0 - pts[1, 1]
  y1 <- pts[2, 2] + y0 - pts[1, 2]

  graphics::arrows(x0, y0, x1, y1, length=0.1)

  a <- atan((y1 - y0) / (x1 - x0)) * (180 / pi)
  if (a > 45 && a <= 135) {
    pos <- 3
  } else if (a > 135 && a <= 225) {
    pos <- 2
  } else if (a > 225 && a <= 315) {
    pos <- 1
  } else {
    pos <- 4
  }


  graphics::text(x1, y1, labels=lab, pos=pos, offset=0.2, cex=cex)
}
