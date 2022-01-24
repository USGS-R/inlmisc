#' Add Scale Bar to Plot
#'
#' Add a scale bar (also known as a rake scale) to a plot.
#'
#' @param unit 'character' vector of length 1 or 2, value is recycled as necessary.
#'   Label(s) describing the unit of measurement of scale distances, such as "METERS".
#' @param conv.fact 'numeric' vector of length 1 or 2, value is recycled as necessary.
#'   Conversion factor(s) for changing the unit of measurement for scale distances.
#'   For example, if user coordinates of the plotting region are in meters,
#'   specify \code{3.28084} to display scale distances in feet.
#'   A dual-unit scale bar is created by specifying a second conversion factor.
#' @param vert.exag 'logical' flag, 'numeric' vector, or 'character' vector.
#'   Either a logical value indicating whether to include a vertical exaggeration label;
#'   or a custom \emph{y/x} aspect ratio to include in this label.
#' @param longlat 'logical' flag.
#'   Whether user coordinates of the plotting region are in longitude and latitude;
#'   if true, scale distances are in kilometers.
#'   Scale distances are calculated at the maps latitude midpoint
#'   using the Great Circle distance (WGS84 ellipsoid) method.
#' @param loc 'character' string.
#'   Position of the scale bar in the main plot region;
#'   see \code{\link{GetInsetLocation}} function for keyword descriptions.
#' @param ...
#'   Additional arguments to be passed to the \code{\link{GetInsetLocation}} function---used
#'   to position the scale bar in the main plot region.
#'
#' @return Invisible \code{NULL}
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
#' plot(-100:100, -100:100, type = "n", xlab = "x in meters",
#'      ylab = "y in meters", asp = 2)
#' AddScaleBar()
#' AddScaleBar(loc = "center")
#' AddScaleBar(unit = "METERS", loc = "topleft", padin = 0.2)
#' AddScaleBar(unit = c("METERS", "FEET"), conv.fact = c(1, 3.28084),
#'             loc = "topright", padin = c(0.5, 0))
#' AddScaleBar(unit = c("METERS", "FEET"), conv.fact = c(1, 3.28084),
#'             vert.exag = TRUE, loc = "bottomright", inset = 0.1)
#'
#' plot(c(-38.31, -35.5), c(40.96, 37.5), type = "n",
#'      xlab = "longitude", ylab = "latitude")
#' AddScaleBar(unit = "KILOMETERS", longlat = TRUE)
#' AddScaleBar(unit = "MILES", conv.fact = 0.621371, longlat = TRUE,
#'             loc = "topright", padin = c(0.4, 0))
#' AddScaleBar(unit = c("KILOMETERS", "MILES"),
#'             conv.fact = c(1, 0.621371), longlat = TRUE,
#'             loc = "topleft", inset = 0.05)
#'

AddScaleBar <- function(unit=NULL, conv.fact=NULL, vert.exag=NULL, longlat=FALSE,
                        loc="bottomleft", ...) {

  checkmate::assertCharacter(unit, all.missing=FALSE, min.len=1, max.len=2, null.ok=TRUE)
  checkmate::assertNumeric(conv.fact, all.missing=FALSE, min.len=1, max.len=2, null.ok=TRUE)
  checkmate::qassert(vert.exag, c("B1", "S1", "N1", "0"))
  checkmate::assertFlag(longlat)
  checkmate::assertString(loc)
  if (!is.null(list(...)$offset))
    stop("offset argument has been removed, use '...' instead.")

  pin  <- graphics::par("pin")  # plot dimensions in inches (width, height)
  xaxp <- graphics::par("xaxp")  # extreme tick marks and number of intervals (x1, x2, n)
  usr  <- graphics::par("usr")  # extremes of the plotting region (x1, x2, y1, y2)
  pusr <- c(diff(usr[1:2]), diff(usr[3:4]))  # plot dimensions in user units (width, height)
  asp <- (pusr[1] / pin[1]) / (pusr[2] / pin[2])  # y/x aspect ratio

  if (is.null(unit)) unit <- rep(as.character(NA), 2)
  if (is.null(conv.fact)) conv.fact <- c(1, NA)
  if (is.null(vert.exag)) {
    vert.exag <- ifelse(asp > 20, TRUE, FALSE)
  } else if (!is.logical(vert.exag)) {
    asp <- vert.exag
    vert.exag <- TRUE
  }

  # calculate length of scale-bar axis in user units
  dx <- diff(xaxp[1:2]) / xaxp[3]  # x distance between plot tick marks
  if (longlat) {
    y <- mean(usr[3:4])  # latitude at center of plot
    ds1 <- sp::spDistsN1(cbind(0, y), c(dx, y), longlat=TRUE) # distance between plot tick marks in kilometers
    ds1 <- ds1 * conv.fact[1]  # distance between plot tick marks in scale units
    bp <- pretty(c(0, .Round(ds1)), min.n=2)  # pretty breakpoints
    ds2 <- diff(range(bp))  # pretty distance between plot tick marks in scale units
    val <- ds2  # number label for last tick mark on scale
    len <- dx * ds2 / ds1  # length of scale bar in user units
    at <- len * bp / max(bp)  # scale tick-mark distances in user units
  } else {
    at <- pretty(c(0, .Round(dx * conv.fact[1])), min.n=2)  # scale tick-mark locations in scale units
    len <- diff(range(at))  # length of scale bar in user units
    val <- len  # number label for last tick mark on scale
    len <- len / conv.fact[1]  # length of scale bar in user units
    at <- at / conv.fact[1]  # scale tick-mark distances in user units
  }

  # create scale-bar labels
  val <- format(val, big.mark=",")
  lab <- ifelse(is.na(unit[1]), val, paste(val, unit[1]))

  # determine user coordinate at origin of scale
  sh <- graphics::strheight("M") # string height in user units
  tcl <- sh * 0.4  # y length of tick marks in user coordinates
  pady <- sh * 0.3
  dx <- sum(graphics::strwidth("0", cex=0.7) / 2, len,
            graphics::strwidth(val, cex=0.7) / 2)
  dy <- sum(tcl, pady, graphics::strheight("0", cex=0.7), pady)

  # get insert location
  xy <- GetInsetLocation(dx, dy, loc=loc, ...)

  # draw axis and tick marks
  xat <- xy[1] + at  # x of tick marks in user coordinates
  graphics::lines(rbind(c(xy[1], xy[2]), c(xy[1] + len, xy[2])), lwd=0.5)
  for (i in xat) graphics::lines(rbind(c(i, xy[2]), c(i, xy[2] + tcl)), lwd=0.5)

  # draw tick-mark labels at extremes of scale bar
  sw <- graphics::strwidth(val, units="user", cex=0.7)
  graphics::text(xy[1], xy[2] + tcl + pady, "0", adj=c(0.5, 0), cex=0.7, xpd=TRUE)
  graphics::text(xy[1] + len - sw / 2, xy[2] + tcl + pady,
                 lab, adj=c(0, 0), cex=0.7, xpd=TRUE)

  # add scale for dual units
  if (!is.na(conv.fact[2])) {
    at <- grDevices::axisTicks(c(0, len * conv.fact[2]), log=FALSE, nint=4)
    val <- format(utils::tail(at, 1), big.mark=",")
    lab <- ifelse(is.na(unit[2]), val, paste(val, unit[2]))
    xat <- xy[1] + at / conv.fact[2]
    sw <- graphics::strwidth(val, units="user", cex=0.7)
    for (i in xat) graphics::lines(rbind(c(i, xy[2]), c(i, xy[2] - tcl)), lwd=0.5)
    graphics::text(xy[1], xy[2] - tcl - pady, "0", adj=c(0.5, 1), cex=0.7, xpd=TRUE)
    graphics::text(utils::tail(xat, 1) - sw / 2, xy[2] - tcl - pady,
                   lab, adj=c(0, 1), cex=0.7, xpd=TRUE)
  }

  # draw label explaining vertical exaggeration beneath scale bar
  if (vert.exag) {
    txt <- sprintf("VERTICAL EXAGGERATION x%s", asp)
    if (is.na(conv.fact[2]))
      y <- xy[2] - pady
    else
      y <- xy[2] - tcl - pady - graphics::strheight("0", cex=0.7) - pady
    graphics::text(xy[1] + len / 2, y, txt, cex=0.7, adj=c(0.5, 1), xpd=TRUE)
  }

  invisible()
}


.Round <- function(x, base=c(1, 2, 5, 10)) {
  checkmate::assertNumeric(x, finite=TRUE, min.len=1)
  checkmate::assertIntegerish(base, lower=1, upper=10, any.missing=FALSE,
                              min.len=2, unique=TRUE, sorted=TRUE)
  n <- floor(log10(abs(x)))
  m1 <- x / 10^n
  vec <- c(0, utils::head(base, -1) + diff(base) / 2)
  m2 <- base[findInterval(m1, vec)]
  m2 * 10^n
}
