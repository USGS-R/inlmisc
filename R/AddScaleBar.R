
#' Add Scale Bar to Plot
#'
#' This function can be used to add a scale bar to a plot.
#' This shows directly on the map the corresponding ground distance.
#'
#' @param unit 'character'.
#'   Axis label for ground unit, for example "METERS".
#' @param conv.fact 'numeric'.
#'   A conversion factor for changing the ground units.
#' @param vert.exag 'logical', 'character', or 'numeric'.
#'   A logical value indicating whether to include a vertical exaggeration label;
#'   or a custom \emph{y/x} aspect ratio to include in this label.
#' @param longlat 'logical'.
#'   If true, plot coordinates are in longitude and latitude,
#'   and scale-bar units are in kilometers.
#'   Scale distances are calculated at the maps latitude midpoint
#'   using the Great Circle distance (WGS84 ellipsoid) method.
#' @param loc 'character'.
#'   Position of the scale bar in the plot region:
#'   \code{"bottomleft"}, \code{"topleft"}, \code{"topright"}, or
#'   \code{"bottomright"} to denote scale location.
#' @param offset 'numeric'.
#'   A vector of length 2 indicating the \emph{x} and \emph{y} adjustments
#'   of the scale bar, in inches.
#'
#' @return Used for the side-effect of a scale bar drawn on the current graphics device.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{PlotMap}}, \code{\link{PlotCrossSection}}
#'
#' @keywords hplot
#'
#' @export
#'
#' @examples
#' plot(-100:100, -100:100, type = "n", xlab = "x in meters", ylab = "y in meters", asp = 2)
#' AddScaleBar()
#' AddScaleBar(unit = "METERS", loc = "topleft")
#' AddScaleBar(unit = "METERS", vert.exag = TRUE, loc = "topright", offset = c(-0.2, 0))
#' AddScaleBar(unit = "FEET", conv.fact = 3.28084, loc = "bottomright")
#'
#' plot(c(-38.31, -35.5), c(40.96, 37.5), type = "n",
#'      xlab = "longitude", ylab = "latitude", asp = 1)
#' AddScaleBar(unit = "km", longlat = TRUE)
#' AddScaleBar(unit = "mi", conv.fact = 0.621371, longlat = TRUE, loc = "topright")
#'

AddScaleBar <- function(unit=NULL, conv.fact=NULL, vert.exag=NULL, longlat=FALSE,
                        loc=c("bottomleft", "topleft", "topright", "bottomright"),
                        offset=NULL) {

  checkmate::assertString(unit, null.ok=TRUE)
  checkmate::assertFlag(longlat)
  loc <- match.arg(loc)
  checkmate::assertNumeric(offset, finite=TRUE, min.len=1, max.len=2, null.ok=TRUE)
  checkmate::qassert(vert.exag, c("B1", "S1", "N1", "0"))
  checkmate::assertNumber(conv.fact, finite=TRUE, null.ok=TRUE)

  usr  <- graphics::par("usr")   # extremes of the plotting region (x1, x2, y1, y2)
  pin  <- graphics::par("pin")   # plot dimensions in inches (width, height)
  xaxp <- graphics::par("xaxp")  # extreme tick marks and number of intervals (x1, x2, n)
  asp <- (diff(usr[1:2]) / pin[1]) / (diff(usr[3:4]) / pin[2])  # y/x aspect ratio

  if (is.null(conv.fact)) conv.fact <- 1
  if (is.null(vert.exag)) {
    vert.exag <- ifelse(asp > 20, TRUE, FALSE)
  } else if (!is.logical(vert.exag)) {
    asp <- vert.exag
    vert.exag <- TRUE
  }
  if (is.null(offset)) offset <- 0; offset <- rep_len(offset, 2)

  # calculate length of scale-bar axis
  dx <- diff(xaxp[1:2]) / xaxp[3]  # x distance between tick marks
  if (longlat) {
    y <- mean(usr[3:4])  # y at center of plot
    dm1 <- sp::spDistsN1(cbind(0, y), c(dx, y), longlat=TRUE) * conv.fact  # ground distance between tick marks
    bp <- pretty(c(0, .Round(dm1)))  # pretty breakpoints
    dm2 <- diff(range(bp))  # pretty ground distance
    lab_val <- dm2  # label value
    d <- dx * dm2 / dm1  # x length of scale bar
    at <- d * bp / max(bp)  # tick-mark locations on scale-bar axis
  } else {
    at <- pretty(c(0, .Round(dx * conv.fact)))  # tick-mark locations on scale-bar axis
    d <- diff(range(at))
    lab_val <- d  # label value
    at <- at / conv.fact
    d <- d / conv.fact
  }

  # create scale-bar labels
  lab_val <- format(lab_val, big.mark=",")
  label <-  paste(c(lab_val, unit), collapse=" ")

  # determine plot coordinate at 0 on scale bar
  sw <- graphics::strwidth("M", units="user", cex=1)  # x string width
  sh <- graphics::strheight("M", units="user", cex=1)  # y string height
  pad <- c(sw * ifelse(grepl("left$", loc), 2, 3),
           sh * ifelse(grepl("^top",  loc), 3, 2))
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
  sw_val <- graphics::strwidth(lab_val, units="user", cex=0.7)
  graphics::text(xy[1], xy[2] + tcl, "0", adj=c(0.5, -0.6), cex=0.7)
  graphics::text(xy[1] + d - sw_val / 2, xy[2] + tcl, label, adj=c(0, -0.6), cex=0.7)

  # draw vertical exaggeration label
  if (vert.exag) {
    txt <- sprintf("VERTICAL EXAGGERATION x%s", asp)
    graphics::text(xy[1] + d / 2, xy[2], txt, cex=0.7, pos=1)
  }
}


.Round <- function(x, base=c(1, 2, 5, 10)) {
  checkmate::assertNumeric(x, finite=TRUE, min.len=1)
  checkmate::assertIntegerish(base, lower=1, upper=10, any.missing=FALSE,
                              min.len=2, unique=TRUE, sorted=TRUE)
  n <- floor(log10(abs(x)))
  m <- x / 10^n
  vec <- c(0, utils::head(base, -1) + diff(base) / 2)
  m_new <- base[findInterval(m, vec)]
  return(m_new * 10^n)
}
