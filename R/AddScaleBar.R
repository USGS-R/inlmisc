
#' Add Scale Bar to Plot
#'
#' This function can be used to add a scale bar to a plot.
#'
#' @param asp 'numeric'.
#'   \emph{y/x} aspect ratio for spatial axes.
#' @param unit 'character'.
#'   Axis unit of measurement, for example "METERS".
#' @param is.lonlat 'logical'.
#'   If true, plot coordinates are in longitude and latitude.
#' @param loc 'character'.
#'   Position of the scale bar in the plot region:
#'   "bottomleft", "topleft", "topright", or "bottomright" to denote scale location.
#' @param offset 'numeric'.
#'   The x and y adjustments of the scale bar, in inches.
#' @param lab.vert.exag 'logical'.
#'   If true, a label is drawn specifying the vertical exaggeration.
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
  loc <- match.arg(loc)
  usr <- graphics::par("usr")
  pin <- graphics::par("pin")

  if (is.null(asp)) asp <- pin[2] / pin[1]

  if (is.lonlat) {
    y <- (usr[3] + usr[4]) / 2
    xaxp <- graphics::par("xaxp")
    dx1 <- diff(xaxp[1:2]) / xaxp[3]
    dm1 <- sp::spDistsN1(cbind(0, y), c(dx1, y), longlat=TRUE)
    dm2 <- diff(pretty(c(0, dm1), 1)[1:2])
    d <- (dx1 * dm2) / dm1
    label <- paste(format(dm2), "km")
  } else {
    d <- diff(pretty(usr[1:2]))[1]
    label <- format(d, big.mark=",")
    if (!is.null(unit)) label <- paste(label, unit)
  }

  padx <- 0.05 * (usr[2] - usr[1])
  pady <- 0.05 * (usr[2] - usr[1]) / asp
  if (loc == "bottomleft") {
    loc <- c(usr[1] + padx, usr[3] + pady)
  } else if (loc == "topleft") {
    loc <- c(usr[1] + padx, usr[4] - pady)
  } else if (loc == "topright") {
    loc <- c(usr[2] - padx - d, usr[4] - pady)
  } else if (loc == "bottomright") {
    loc <- c(usr[2] - padx - d, usr[3] + pady)
  }
  loc[1] <- loc[1] + offset[1] * (diff(usr[1:2]) / pin[1])
  loc[2] <- loc[2] + offset[2] * (diff(usr[3:4]) / pin[2])

  divs <- 1L
  for (i in 5:3) {
    if (d %% i == 0) {
      divs <- i
      break
    }
  }
  xat <- seq(loc[1], loc[1] + d, length.out=(divs + 1L))
  tcl <- (diff(usr[1:2]) * 0.01) / asp

  graphics::lines(rbind(c(loc[1], loc[2]), c(loc[1] + d, loc[2])), lwd=0.5)
  for (i in xat) graphics::lines(rbind(c(i, loc[2]), c(i, loc[2] + tcl)), lwd=0.5)
  graphics::text(loc[1], loc[2] + tcl, "0", adj=c(0.5, -0.5), cex=0.7)
  graphics::text(loc[1] + d, loc[2] + tcl, label, adj=c(0.3, -0.5), cex=0.7)

  if (is.logical(lab.vert.exag)) {
    add.label <- lab.vert.exag
  } else {
    add.label <- if (asp > 20) TRUE else FALSE
  }
  if (add.label) {
    txt <- sprintf("VERTICAL EXAGGERATION x%s", asp)
    graphics::text(loc[1] + d / 2, loc[2], txt, cex=0.7, pos=1)
  }
}
