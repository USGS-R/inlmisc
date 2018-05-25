
#' Add Scale Bar to Plot
#'
#' This function can be used to add a scale bar to a plot.
#' This shows directly on the map the corresponding ground distance.
#'
#' @param asp 'numeric'.
#'   \emph{y/x} aspect ratio for spatial axes.
#' @param unit 'character'.
#'   Axis label for ground unit, for example "METERS".
#' @param is.lonlat 'logical'.
#'   Whether plot coordinates are in longitude and latitude.
#'   If true, ground units are in kilometers and distances are
#'   calculated using the Great Circle (WGS84 ellipsoid) distance method.
#' @param loc 'character'.
#'   Position of the scale bar in the plot region:
#'   \code{"bottomleft"}, \code{"topleft"}, \code{"topright"}, or
#'   \code{"bottomright"} to denote scale location.
#' @param offset 'numeric'.
#'   A vector of length 2 indicating the \emph{x} and \emph{y} adjustments
#'   of the scale bar, in inches.
#' @param lab.vert.exag 'logical'.
#'   Whether to draw a label indicating the vertical exaggeration.
#' @param conv.fact 'numeric'.
#'   A conversion factor for changing the ground units.
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
#' plot(-100:100, -100:100, type = "n", xlab = "x in meters", ylab = "y in meters", asp = 2)
#' AddScaleBar(unit = "METERS", loc = "topleft")
#' AddScaleBar(asp = 2, unit = "METERS", loc = "topright", offset = c(-0.2, 0),
#'             lab.vert.exag = TRUE)
#' AddScaleBar(unit = "FEET", loc = "bottomleft", conv.fact = 3.28)
#'
#' plot(c(-38.31, -35.5), c(40.96, 37.5), type = "n",
#'      xlab = "longitude", ylab = "latitude", asp = 1)
#' AddScaleBar(unit = "km", is.lonlat = TRUE)
#' AddScaleBar(unit = "mi", is.lonlat = TRUE, loc = "topright", conv.fact = 0.621371)
#'

AddScaleBar <- function(asp=1, unit=NULL, is.lonlat=FALSE,
                        loc=c("bottomleft", "topleft", "topright", "bottomright"),
                        offset=c(0, 0), lab.vert.exag=NULL, conv.fact=NULL) {

  checkmate::assertNumber(asp, finite=TRUE, null.ok=TRUE)
  checkmate::assertString(unit, null.ok=TRUE)
  checkmate::assertFlag(is.lonlat)
  loc <- match.arg(loc)
  checkmate::assertNumeric(offset, any.missing=FALSE, len=2)
  checkmate::qassert(lab.vert.exag, c("B1", "n1", "0"))
  checkmate::assertNumber(conv.fact, finite=TRUE, null.ok=TRUE)

  usr  <- graphics::par("usr")   # extremes of the plotting region (x1, x2, y1, y2)
  pin  <- graphics::par("pin")   # plot dimensions in inches (width, height)
  xaxp <- graphics::par("xaxp")  # extreme tick marks and number of intervals (x1, x2, n)

  if (is.null(asp)) asp <- pin[2] / pin[1]  # aspect ratio
  if (is.null(lab.vert.exag)) lab.vert.exag <- asp > 20
  if (is.null(conv.fact)) conv.fact <- 1

  # calculate length of scale bar axis
  dx <- diff(xaxp[1:2]) / xaxp[3]  # x distance between tick marks
  if (is.lonlat) {
    y <- mean(usr[3:4])  # y at center of plot
    dm1 <- sp::spDistsN1(cbind(0, y), c(dx, y), longlat=TRUE) * conv.fact  # ground distance between tick marks
    bp <- pretty(c(0, dm1))  # pretty breakpoints
    dm2 <- diff(range(bp))  # pretty ground distance
    lab_val <- dm2  # label value

    d <- dx * dm2 / dm1  # x length of scale bar
    at <- d * bp / max(bp)  # tick-mark locations on scale-bar axis

  } else {
    at <- pretty(c(0, dx * conv.fact))  # tick-mark locations on scale-bar axis
    d <- diff(range(at))
    lab_val <- d  # label value
    at <- at / conv.fact
    d <- d / conv.fact
  }

  # create scale-bar labels
  label <- c("0", paste(c(format(lab_val, big.mark=","), unit), collapse=" "))

  # determine plot coordinate at 0 on scale bar
  sh <- graphics::strheight("M", units="user", cex=1)  # x string width
  sw <- graphics::strwidth("M", units="user", cex=1)  # y string height
  pad <- c(sw * ifelse(grepl("left$", loc), 1, 2),
           sh * ifelse(grepl("^top",  loc), 2, 1))
  if (loc == "bottomleft") {
    xy <- c(usr[1] + pad[1], usr[3] + pad[2])
  } else if (loc == "topleft") {
    xy <- c(usr[1] + pad[1], usr[4] - pad[2])
  } else if (loc == "topright") {
    xy <- c(usr[2] - pad[1] - d, usr[4] - pad[2])
  } else if (loc == "bottomright") {
    xy <- c(usr[2] - pad[1] - d, usr[3] + pad[2])
  }
  xy <- c(xy[1] + offset[1] * diff(usr[1:2]) / pin[1],
          xy[2] + offset[2] * diff(usr[3:4]) / pin[2])

  # draw axis and tick marks
  xat <- xy[1] + at  # x of tick marks
  tcl <- sh * 0.4  # y length of tick marks

  graphics::lines(rbind(c(xy[1], xy[2]), c(xy[1] + d, xy[2])), lwd=0.5)
  for (i in xat) graphics::lines(rbind(c(i, xy[2]), c(i, xy[2] + tcl)), lwd=0.5)

  # draw label strings at extremes of scale bar
  graphics::text(xy[1], xy[2] + tcl, label[1], adj=c(0.5, -0.6), cex=0.7)
  graphics::text(xy[1] + d, xy[2] + tcl, label[2], adj=c(0.3, -0.6), cex=0.7)

  # draw vertical exaggeration label
  if (lab.vert.exag) {
    txt <- sprintf("VERTICAL EXAGGERATION x%s", asp)
    graphics::text(xy[1] + d / 2, xy[2], txt, cex=0.7, pos=1)
  }
}
