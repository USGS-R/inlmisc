
#' Add Scale Bar to Plot
#'
#' This function can be used to add a scale bar to a plot.
#'
#' @param asp 'numeric'.
#'   \emph{y/x} aspect ratio for spatial axes.
#' @param unit 'character'.
#'   Axis label for unit of measurement, for example "METERS".
#' @param is.lonlat 'logical'.
#'   Whether plot coordinates are in longitude and latitude.
#'   If true, scale values are in units of kilometers.
#' @param loc 'character'.
#'   Position of the scale bar in the plot region:
#'   \code{"bottomleft"}, \code{"topleft"}, \code{"topright"}, or
#'   \code{"bottomright"} to denote scale location.
#' @param offset 'numeric'.
#'   The x and y adjustments of the scale bar, in inches.
#' @param lab.vert.exag 'logical'.
#'   Whether a label is drawn specifying the vertical exaggeration.
#'
#' @return Used for the side-effect of a scale bar drawn on the current graphics device.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{PlotCrossSection}}, \code{\link{PlotMap}}
#'
#' @keywords hplot
#'
#' @export
#'
#' @examples
#' plot(-100:100, -100:100, type = "n", xlab = "x", ylab = "y", asp = 2)
#' AddScaleBar(2, unit = "FEET", loc = "topleft")
#' AddScaleBar(2, unit = "METERS", loc = "bottomright", offset = c(-0.2, 0))
#'

AddScaleBar <- function(asp=1, unit=NULL, is.lonlat=FALSE,
                        loc=c("bottomleft", "topleft", "topright", "bottomright"),
                        offset=c(0, 0), lab.vert.exag=NULL) {

  checkmate::assertNumber(asp, finite=TRUE, null.ok=TRUE)
  checkmate::assertString(unit, null.ok=TRUE)
  checkmate::assertFlag(is.lonlat)
  loc <- match.arg(loc)
  checkmate::assertNumeric(offset, any.missing=FALSE, len=2)
  checkmate::assertFlag(lab.vert.exag, null.ok=TRUE)

  usr  <- graphics::par("usr")   # extremes of the plotting region (x1, x2, y1, y2)
  pin  <- graphics::par("pin")   # plot dimensions in inches (width, height)
  xaxp <- graphics::par("xaxp")  # extreme tick marks and number of intervals (x1, x2, n)

  if (is.null(asp)) asp <- pin[2] / pin[1]  # aspect ratio

  # calculate length of scale bar
  if (is.lonlat) {
    y <- mean(usr[3:4])  # y/latitude at center of plot
    dx1 <- diff(xaxp[1:2]) / xaxp[3]  # distance between tick marks in x/longitude direction
    dm1 <- sp::spDistsN1(cbind(0, y), c(dx1, y), longlat=TRUE)  # euclidean distance between tick marks
    dm2 <- diff(range(pretty(c(0, dm1))))  # pretty euclidean distance
    d <- (dx1 * dm2) / dm1  # pretty x/longitude length of scale bar
  } else {
    d <- diff(pretty(usr[1:2]))[1]  # pretty x length of scale bar
  }

  # create scale-bar labels
  label <- c("0", paste(c(format(d, big.mark=","), unit), collapse=" "))

  # determine plot coordinate for 0 on scale bar
  padx <- 0.05 * (usr[2] - usr[1])
  pady <- 0.05 * (usr[2] - usr[1]) / asp
  if (loc == "bottomleft") {
    xy <- c(usr[1] + padx, usr[3] + pady)
  } else if (loc == "topleft") {
    xy <- c(usr[1] + padx, usr[4] - pady)
  } else if (loc == "topright") {
    xy <- c(usr[2] - padx - d, usr[4] - pady)
  } else if (loc == "bottomright") {
    xy <- c(usr[2] - padx - d, usr[3] + pady)
  }
  xy <- c(xy[1] + offset[1] * (diff(usr[1:2]) / pin[1]),
          xy[2] + offset[2] * (diff(usr[3:4]) / pin[2]))

  # draw axis and tick marks
  xat <- xy[1] + pretty(c(0, d))  # x of tick marks
  tcl <- (diff(usr[1:2]) * 0.01) / asp  # y length of tick marks
  graphics::lines(rbind(c(xy[1], xy[2]), c(xy[1] + d, xy[2])), lwd=0.5)
  for (i in xat) graphics::lines(rbind(c(i, xy[2]), c(i, xy[2] + tcl)), lwd=0.5)

  # draw label strings at extremes of scale bar
  graphics::text(xy[1], xy[2] + tcl, label[1], adj=c(0.5, -0.5), cex=0.7)
  graphics::text(xy[1] + d, xy[2] + tcl, label[2], adj=c(0.3, -0.5), cex=0.7)

  # draw vertical exaggeration label
  if (is.null(lab.vert.exag)) lab.vert.exag <- asp > 20
  if (lab.vert.exag) {
    txt <- sprintf("VERTICAL EXAGGERATION x%s", asp)
    graphics::text(xy[1] + d / 2, xy[2], txt, cex=0.7, pos=1)
  }
}
