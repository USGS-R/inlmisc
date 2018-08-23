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
#'   Position of the inset map in the main plot region;
#'   see \code{\link{GetInsetLocation}} function for keyword descriptions.
#' @param inset 'numeric'.
#'   Inset distance(s) from the margins as a fraction of the main plot region.
#'   Defaults to 2 percent of the axis range.
#' @param width 'numeric'.
#'   Width of the inset map in inches.
#' @param e 'numeric'.
#'   Vector of length 4 describing the extent of the smaller axis-aligned rectangle (relative to the larger map polygon).
#'   Defaults to the user coordinate extent of the main plot region, see \code{par("usr")}.
#' @param bty 'character'.
#'   The type of box to be drawn about the inset map.
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

  invisible(NULL)
}


#' Get Inset Location
#'
#' This function determines the location for an inset in the main plot region.
#'
#' @param dx,dy 'numeric'.
#'   Width and height of the inset graphics, respectively.
#' @param loc 'character'.
#'   Single keyword used to specify the position of the inset in the main plot region:
#'   \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"},
#'   \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"},
#'   or \code{"center"} to denote inset location.
#' @param inset 'numeric'.
#'   Inset distance(s) from the margins as a fraction of the main plot region.
#'   Defaults to 2 percent of the axis range.
#' @param padx,pady 'numeric'.
#'   Padding distance in the \emph{x} and \emph{y} direction, respectively.
#'
#' @return Returns a 'numeric' vector of length 2 giving the user coordinates
#'   for the bottom-left corner of the inset.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#' graphics::plot(NA, NA, xlim = c(0, 100), ylim = c(0, 100),
#'                xlab = "x", ylab = "y", xaxs = "i", yaxs = "i")
#' dx <- 20; dy <- 10
#' xy <- GetInsetLocation(dx, dy)
#' graphics::rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, border = "red")
#' graphics::points(xy[1], xy[2], pch = 16)
#'
#' xy <- GetInsetLocation(dx, dy, inset = 0.05)
#' graphics::rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, border = "pink")
#' graphics::points(xy[1], xy[2], pch = 16)
#'
#' xy <- GetInsetLocation(dx, dy, loc = "topright")
#' graphics::rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, border = "blue")
#'
#' xy <- GetInsetLocation(dx, dy, loc = "left", padx = 5)
#' graphics::rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, border = "green")
#'
#' xy <- GetInsetLocation(dx, dy, loc = "center")
#' graphics::rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, border = "brown")
#'

GetInsetLocation <- function(dx, dy, loc="bottomleft", inset=0.02, padx=0, pady=0) {

  if (grDevices::dev.cur() == 1) stop("no active device")
  usr <- graphics::par("usr")
  w <- diff(usr[1:2])
  h <- diff(usr[3:4])

  choices <- c("bottomright", "bottom", "bottomleft", "left",
               "topleft", "top", "topright", "right", "center")
  loc <- match.arg(loc, choices)
  dx <- checkmate::assertNumber(dx, lower=0, upper=w, finite=TRUE)
  dy <- checkmate::assertNumber(dy, lower=0, upper=h, finite=TRUE)
  inset <- checkmate::assertNumeric(inset, lower=0, upper=1, finite=TRUE,
                                    any.missing=FALSE, min.len=1, max.len=2)
  padx <- checkmate::assertNumber(padx, finite=TRUE)
  pady <- checkmate::assertNumber(pady, finite=TRUE)

  inset <- rep(inset, length.out=2)
  padx <- padx + inset[1] * w
  pady <- pady + inset[2] * h

  center <- c(usr[1] + w / 2 - dx / 2, usr[3] + h / 2 - dy / 2)

  if (loc == "bottomright") {
    xy <- c(usr[2] - padx - dx, usr[3] + pady)
  } else if (loc == "bottom") {
    xy <- c(center[1], usr[3] + pady)
  } else if (loc == "bottomleft") {
    xy <- c(usr[1] + padx, usr[3] + pady)
  } else if (loc == "left") {
    xy <- c(usr[1] + padx, center[2])
  } else if (loc == "topleft") {
    xy <- c(usr[1] + padx, usr[4] - pady - dy)
  } else if (loc == "top") {
    xy <- c(center[1], usr[4] - pady - dy)
  } else if (loc == "topright") {
    xy <- c(usr[2] - padx - dx, usr[4] - pady - dy)
  } else if (loc == "right") {
    xy <- c(usr[2] - padx - dx, center[2])
  } else if (loc == "center") {
    xy <- center
  }
  names(xy) <- c("x", "y")

  return(xy)
}
