#' Add Inset Map to Plot
#'
#' This function can be used to add an inset map to a plot.
#'
#' @param p 'SpatialPolygons'.
#'   Polygon describing the large map.
#' @param col 'character' vector of length 2.
#'   Colors for filling the large map polygon \code{p} and the smaller plot extent rectangle.
#' @param main.label 'list'.
#'   List with components \code{label} and \code{adj}.
#'   The text label and position (x and y adjustment of the label) for the large map, respectively.
#' @param sub.label 'list'.
#'   Identical to the \code{main.label} argument but for the plot extent rectangle.
#' @param loc 'character' string.
#'   Position of the inset map in the main plot region;
#'   see \code{\link{GetInsetLocation}} function for keyword descriptions.
#' @param inset 'numeric' vector of length 1 or 2.
#'   Inset distance(s) from the margins as a fraction of the main plot region.
#'   Defaults to 2 percent of the axis range.
#' @param width 'numeric' number.
#'   Width of the inset map in inches.
#' @param e 'numeric' vector of length 4.
#'   Extent of the smaller axis-aligned rectangle (relative to the larger map polygon).
#'   Defaults to the user coordinate extent of the main plot region, see \code{par("usr")}.
#' @param bty 'character' string.
#'   Type of box to be drawn about the inset map.
#'   A value of \code{"o"} (the default) results in a box and a value of \code{"n"} supresses the box.
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
#' file <- system.file("extdata/county.geojson", package = "inlmisc")[1]
#' county <- rgdal::readOGR(file)
#' ext <- c(-113.4005, -112.2764, 43.30, 44.11)
#' PlotMap(county, xlim = ext[1:2], ylim = ext[3:4], dms.tick = TRUE)
#' sp::plot(county, add = TRUE)
#' inlmisc::AddInsetMap(county, width = 2, main.label = list("IDAHO", adj = c(0, -10)),
#'                      sub.label=list("Map area", adj = c(0, -4)), loc = "topright")
#'

AddInsetMap <- function(p, col=c("#D8D8D8", "#BFA76F"),
                        main.label=list(label=NA, adj=NULL),
                        sub.label=list(label=NA, adj=NULL), loc="topright",
                        inset=0.02, width=NULL, e=NULL, bty=c("o", "n")) {

  checkmate::assertClass(p, "SpatialPolygons")
  checkmate::assertCharacter(col, any.missing=FALSE, len=2)
  checkmate::assertList(main.label)
  checkmate::assertList(sub.label)
  checkmate::assertNumeric(inset, finite=TRUE, min.len=1, max.len=2)
  checkmate::assertNumber(width, finite=TRUE, null.ok=TRUE)
  checkmate::assertNumeric(e, finite=TRUE, len=4, null.ok=TRUE)
  bty <- match.arg(bty)

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
  xy <- GetInsetLocation(dx, dy, loc=loc, inset=inset)
  graphics::rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, col="#FFFFFFE7", border=NA)

  plt <- c(graphics::grconvertX(c(xy[1], xy[1] + dx), "user", "nfc"),
           graphics::grconvertY(c(xy[2], xy[2] + dy), "user", "nfc"))
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

  if (bty != "n") graphics::box(lwd=0.5)

  invisible()
}
