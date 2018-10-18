#' Add North Arrow to Plot
#'
#' Add a north arrow aligned to true north to a plot.
#'
#' @param crs 'CRS', 'Raster*', or 'Spatial'.
#'   Coordinate reference system (CRS), or any object with a CRS attribute
#'   that can be extracted using the \code{\link[raster:projection]{crs}} function.
#'   If missing (the default) the north arrow is point to the top of the plot
#'   unless the \code{rotate} argument is specified.
#' @param len 'numeric' number.
#'   Arrow length expressed as a fraction of the plot height, by default is 5-percent.
#' @param lab 'character' string.
#'   North label, by default is \dQuote{N}.
#' @param rotate 'numeric' number.
#'   Arrow offset-rotation in degrees, where positive values are taken to be clockwise.
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
#' PlotMap(r, pal = GetColors(scheme = "DEM screen"))
#' AddNorthArrow(raster::crs(r), loc = "center")
#' AddNorthArrow(raster::crs(r), inset = 0.1)
#' AddNorthArrow(raster::crs(r), loc = "topleft", inset = 0.1)
#'

AddNorthArrow <- function(crs=sp::CRS(), len=0.05, lab="N", rotate=0, ...) {

  stopifnot(inherits(crs, c("CRS", "RasterLayer", "Spatial")))
  checkmate::assertNumber(len, lower=0, upper=1, finite=TRUE)
  checkmate::assertString(lab)
  checkmate::assertNumber(rotate, lower=-360, upper=360, finite=TRUE)

  usr <- graphics::par("usr")
  len <- len * abs(diff(usr[3:4]))
  crs <- raster::crs(crs)

  if (is.na(crs)) {
    xy <- GetInsetLocation(0, len, ...)
    x1 <- xy["x"]
    y1 <- xy["y"] + len

  } else {
    crs.dd <- sp::CRS("+init=epsg:4326")
    x.mid <- (usr[2] + usr[1]) / 2
    y.mid <- (usr[4] + usr[3]) / 2
    pts <- rbind(c(x.mid, y.mid), c(x.mid, y.mid + len))
    pts <- sp::SpatialPoints(pts, proj4string=crs)

    sp.dd <- sp::spTransform(pts, sp::CRS("+init=epsg:4326"))
    dd <- sp.dd@coords
    len.dd <- sqrt((dd[2, 1] - dd[1, 1])^2 + (dd[2, 2] - dd[1, 2])^2)
    dd <- rbind(dd[1, ], c(dd[1, 1],  dd[1, 2] + len.dd))
    dd <- sp::SpatialPoints(dd, proj4string=sp::CRS("+init=epsg:4326"))
    sp.xy <- sp::spTransform(dd, crs)
    pts <- sp.xy@coords

    xy <- GetInsetLocation(abs(diff(pts[, 1])), abs(diff(pts[, 2])), ...)
    x1 <- pts[2, 1] + xy["x"] - pts[1, 1]
    y1 <- pts[2, 2] + xy["y"] - pts[1, 2]
  }

  rad <- (-1 * rotate * pi) / 180
  x2 <- cos(rad) * (x1 - xy["x"]) - sin(rad) * (y1 - xy["y"]) + xy["x"]
  y2 <- sin(rad) * (x1 - xy["x"]) + cos(rad) * (y1 - xy["y"]) + xy["y"]

  deg <- atan2(y2 - xy["y"], x2 - xy["x"]) * (180 / pi)
  if (deg > 45 && deg <= 135) {
    pos <- 3
  } else if (deg > 135 && deg <= 225) {
    pos <- 2
  } else if (deg > 225 && deg <= 315) {
    pos <- 1
  } else {
    pos <- 4
  }

  graphics::arrows(xy["x"], xy["y"], x2, y2, length=0.1)
  graphics::text(x2, y2, labels=lab, pos=pos, offset=0.2, cex=0.7)

  invisible()
}
