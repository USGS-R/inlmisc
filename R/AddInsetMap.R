#' Add Inset Map to Plot
#'
#' This function can be used to add an inset map to a plot.
#'
#' @param p 'SpatialPolygons'.
#'   Polygon describing the large map.
#' @param col 'list'.
#'   Vector of length 2 giving the colors for filling the large map polygon \code{p} and the smaller plot extent rectangle.
#' @param main.label 'list'.
#'   List with components \code{label} and \code{adj}.
#'   The text label and position (x and y adjustment of the label) for the large map, respectively.
#' @param sub.label 'list'.
#'   Identical to the \code{main.label} argument but for the plot extent rectangle.
#' @param loc 'character'.
#'   Position of the inset map in the main plot region:
#'   "bottomleft", "topleft", "topright", "bottomright", or "center" to denote scale location.
#' @param inset 'numeric'.
#'   Inset distance(s) from the margins as a fraction of the main plot region.
#'   Defaults to 2 percent of the axis range.
#' @param width 'numeric'.
#'   Width of the inset map in inches.
#' @param e 'numeric'.
#'   Vector of length 4 describing the extent of the smaller axis-aligned rectangle (relative to the larger map polygon).
#'   Defaults to the user coordinate extent of the main plot region, see \code{par("usr")}.
#'
#' @return Used for the side-effect of a inset map drawn on the current graphics device.
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
#' nc <- rgdal::readOGR(system.file("shapes/sids.shp", package = "maptools")[1],
#'                      p4s = "+proj=longlat +datum=NAD27")
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
                        loc=c("bottomleft", "topleft", "topright", "bottomright", "center"),
                        inset=0.02, width=NULL, e=NULL) {

  checkmate::assertClass(p, "SpatialPolygons")
  checkmate::assertCharacter(col, any.missing=FALSE, len=2)
  checkmate::assertList(main.label)
  checkmate::assertList(sub.label)
  loc <- match.arg(loc)
  checkmate::assertNumeric(inset, finite=TRUE, min.len=1, max.len=2)
  checkmate::assertNumber(width, finite=TRUE, null.ok=TRUE)
  checkmate::assertNumeric(e, finite=TRUE, len=4, null.ok=TRUE)

  op <- graphics::par(no.readonly=TRUE)
  on.exit(graphics::par(op))

  usr <- graphics::par("usr")

  if (is.null(e)) e <- usr
  crds <- cbind(c(e[1:2], e[2:1], e[1]), c(rep(e[3], 2), rep(e[4], 2), e[3]))
  b <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(crds)), "bbox")), proj4string=raster::crs(p))
  if (length(rgeos::gIntersection(p, b)) == 0)
    stop("user coordinates of the plotting region do not intersect polygon")
  ext <- raster::extent(rgeos::gUnion(p, b))

  if (is.null(width)) {
    dx  <- 0.2 * diff(usr[1:2])
  } else {
    dx <- width * (diff(usr[1:2]) / graphics::par("pin")[1])
  }
  dy <- dx * (diff(ext[3:4]) / diff(ext[1:2]))

  if (length(inset) == 1) inset <- rep(inset, 2)
  padx <- inset[1] * diff(usr[1:2])
  pady <- inset[2] * diff(usr[3:4])

  if (loc == "bottomleft") {
    loc <- c(usr[1] + padx, usr[3] + pady)
  } else if (loc == "topleft") {
    loc <- c(usr[1] + padx, usr[4] - pady - dy)
  } else if (loc == "topright") {
    loc <- c(usr[2] - padx - dx, usr[4] - pady - dy)
  } else if (loc == "bottomright") {
    loc <- c(usr[2] - padx - dx, usr[3] + pady)
  } else if (loc == "center")  {
    loc <- c(usr[1] + diff(usr[1:2]) / 2 - dx / 2,
             usr[3] + diff(usr[3:4]) / 2 - dy / 2)
  }

  graphics::rect(loc[1], loc[2], loc[1] + dx, loc[2] + dy, col="#FFFFFFE7", border=NA)

  plt <- c(graphics::grconvertX(c(loc[1], loc[1] + dx), "user", "nfc"),
           graphics::grconvertY(c(loc[2], loc[2] + dy), "user", "nfc"))
  graphics::par(plt=plt, bg="#FFFFFFCC", new=TRUE)

  xlim <- range(ext[1:2])
  ylim <- range(ext[3:4])
  graphics::plot.window(xlim=xlim, ylim=ylim)

  sp::plot(p, col=col[1], border=NA,        lwd=0.25, add=TRUE)
  sp::plot(b, col=col[2], border="#090909", lwd=0.25, add=TRUE)
  sp::plot(p, col=NA,     border="#090909", lwd=0.25, add=TRUE)

  if (!is.na(main.label[[1]])) {
    x <- sp::coordinates(rgeos::gUnaryUnion(p))[1, ]
    graphics::text(x[1], x[2], labels=main.label[[1]], adj=main.label$adj, cex=0.7, font=2)
  }
  if (!is.na(sub.label[[1]])) {
    x <- sp::coordinates(rgeos::gUnaryUnion(b))[1, ]
    graphics::text(x[1], x[2], labels=sub.label[[1]], adj=sub.label$adj, cex=0.6)
  }

  graphics::box(lwd=0.5)

  invisible(NULL)
}
