#' Add Inset Map to Plot
#'
#' This function can be used to add an inset map to a plot.
#'
#' @param p SpatialPolygons.
#'   Polygon describing the large map.
#' @param col list.
#'   Vector of length 2 giving the colors for filling the large map polygon \code{p} and the smaller plot extent rectangle.
#' @param main.label list.
#'   List with components \code{label} and \code{adj}.
#'   The text label and position (x and y adjustment of the label) for the large map, respectively.
#' @param sub.label list.
#'   Identical to the \code{main.label} argument but for the plot extent rectangle.
#' @param loc character.
#'   Position of the inset map in the main plot region:
#'   "bottomleft", "topleft", "topright", or "bottomright" to denote scale location.
#' @param inset numeric.
#'   Inset distance from the margins as a fraction of the main plot region.
#'   Defaults to 2 percent of the axis range.
#' @param width numeric.
#'   Width of the inset map in inches.
#'
#' @details The smaller axis-aligned rectangle (relative to the larger map polygon) is defined by
#'   the user coordinate extent of the main plot region, see \code{par("usr")}.
#'
#' @return Used for the side-effect of a inset map drawn on the current graphics device.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{PlotMap}}
#'
#' @keywords hplot
#'
#' @import sp
#' @import raster
#'
#' @export
#'
#' @examples
#' nc <- maptools::readShapePoly(system.file("shapes/sids.shp", package = "maptools")[1],
#'                               proj4string=sp::CRS("+proj=longlat +datum=NAD27"))
#' bb <- sp::bbox(nc[100, ])
#' xlim <- grDevices::extendrange(bb["x", ])
#' ylim <- grDevices::extendrange(bb["y", ])
#' PlotMap(raster::crs(nc), xlim = xlim, ylim = ylim, dms.tick = TRUE)
#' sp::plot(nc, add = TRUE)
#' AddInsetMap(nc, width = 3, main.label = list("North Carolina", adj = c(1.8, 3)),
#'             sub.label = list("Map area", adj = c(1.5, 0.5)), loc = "topright")
#'

AddInsetMap <- function(p, col=c("#D8D8D8", "#BFA76F"),
                        main.label=list(label=NA, adj=NULL),
                        sub.label=list(label=NA, adj=NULL),
                        loc=c("bottomleft", "topleft", "topright", "bottomright"),
                        inset=0.02, width=NULL) {

  op <- graphics::par(no.readonly=TRUE)
  on.exit(graphics::par(op))

  loc <- match.arg(loc)

  if (!inherits(p, c("SpatialPolygons", "SpatialPolygonsDataFrame")))
    stop("polygon 'p' is the incorrect class")

  usr <- graphics::par("usr")
  crds <- cbind(c(usr[1:2], usr[2:1], usr[1]),
                c(rep(usr[3], 2), rep(usr[4], 2), usr[3]))
  b <- SpatialPolygons(list(Polygons(list(Polygon(crds)), "bbox")),
                       proj4string=crs(p))

  if (length(rgeos::gIntersection(p, b)) == 0)
    stop("user coordinates of the plotting region do not intersect polygon")

  ext <- extent(rgeos::gUnion(p, b))

  if (is.null(width)) {
    dx  <- 0.2 * diff(usr[1:2])
  } else {
    dx <- width * (diff(usr[1:2]) / graphics::par("pin")[1])
  }
  dy <- dx * (diff(ext[3:4]) / diff(ext[1:2]))

  padx <- inset * diff(usr[1:2])
  pady <- inset * diff(usr[3:4])

  if (loc == "bottomleft") {
    loc <- c(usr[1] + padx, usr[3] + pady)
  } else if (loc == "topleft") {
    loc <- c(usr[1] + padx, usr[4] - pady - dy)
  } else if (loc == "topright") {
    loc <- c(usr[2] - padx - dx, usr[4] - pady - dy)
  } else if (loc == "bottomright") {
    loc <- c(usr[2] - padx - dx, usr[3] + pady)
  }

  graphics::rect(loc[1], loc[2], loc[1] + dx, loc[2] + dy, col="#FFFFFFE7", border=NA)

  plt <- c(graphics::grconvertX(c(loc[1], loc[1] + dx), "user", "nfc"),
           graphics::grconvertY(c(loc[2], loc[2] + dy), "user", "nfc"))
  graphics::par(plt=plt, bg="#FFFFFFCC", new=TRUE)

  xlim <- range(ext[1:2])
  ylim <- range(ext[3:4])
  graphics::plot.window(xlim=xlim, ylim=ylim)

  plot(p, col=col[1], border=NA,        lwd=0.25, add=TRUE)
  plot(b, col=col[2], border="#090909", lwd=0.25, add=TRUE)
  plot(p, col=NA,     border="#090909", lwd=0.25, add=TRUE)

  if (!is.na(main.label[[1]])) {
    x <- coordinates(rgeos::gUnaryUnion(p))[1, ]
    text(x[1], x[2], cex=0.7, main.label[[1]], adj=main.label$adj, font=2)
  }
  if (!is.na(sub.label[[1]])) {
    x <- coordinates(rgeos::gUnaryUnion(b))[1, ]
    text(x[1], x[2], cex=0.6, sub.label[[1]], adj=sub.label$adj)
  }

  graphics::box(lwd=0.5)

  invisible(NULL)
}
