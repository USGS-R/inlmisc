#' Get Location for Inset in Plot
#'
#' Calculate \emph{x} and \emph{y} co-ordinates that can be used to
#' position an inset in a plot frame at a specified keyword location.
#'
#' @param dx,dy 'numeric' number.
#'   Width and height of the inset, respectively.
#' @param loc 'character' string.
#'   Single keyword used to specify the position of the inset in the main plot region:
#'   \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"},
#'   \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"},
#'   or \code{"center"} to denote inset location.
#' @param inset 'numeric' vector of length 1 or 2, value is recycled as necessary.
#'   Inset distance from the margins as a fraction of the main plot region.
#' @param pad 'numeric' vector of length 1 or 2, value is recycled as necessary.
#'   Padding distance from the margins in user coordinate units.
#' @param padin 'numeric' vector of length 1 or 2, value is recycled as necessary.
#'   Padding distance from the margins in inches.
#'
#' @return A 'numeric' vector of length 2 giving the user coordinates
#'   for the bottom-left corner of the inset.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords hplot
#'
#' @export
#'
#' @examples
#' plot(NA, NA, xlim = c(0, 100), ylim = c(0, 1),
#'      xlab = "x", ylab = "y", xaxs = "i", yaxs = "i")
#' dx <- 20; dy <- 0.2
#' xy <- GetInsetLocation(dx, dy, loc = "bottomleft")
#' rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, border = "red")
#' points(xy[1], xy[2], pch = 16, xpd = TRUE)
#' print(xy)
#'
#' xy <- GetInsetLocation(dx, dy, loc = "bottomleft", inset = 0.05)
#' rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, border = "pink")
#' points(xy[1], xy[2], pch = 16)
#' print(xy)
#'
#' xy <- GetInsetLocation(dx, dy, loc = "topright", padin = 0.5)
#' rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, border = "blue")
#'
#' xy <- GetInsetLocation(dx, dy, loc = "left", pad = c(5, 0))
#' rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, border = "green")
#'
#' xy <- GetInsetLocation(dx, dy, loc = "center")
#' rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, border = "brown")
#'

GetInsetLocation <- function(dx, dy, loc="bottomright", inset=0, pad=0, padin=0) {

  if (grDevices::dev.cur() == 1) stop("no active device")
  pin <- graphics::par("pin")  # plot dimensions in inches (width, height)
  usr <- graphics::par("usr")  # extremes of the plotting region (x1, x2, y1, y2)
  w <- diff(usr[1:2])
  h <- diff(usr[3:4])

  choices <- c("bottomright", "bottom", "bottomleft", "left",
               "topleft", "top", "topright", "right", "center")
  loc <- match.arg(loc, choices)
  checkmate::assertNumber(dx, lower=0, upper=w, finite=TRUE)
  checkmate::assertNumber(dy, lower=0, upper=h, finite=TRUE)
  checkmate::assertNumeric(inset, lower=0, upper=1, finite=TRUE,
                           any.missing=FALSE, min.len=1, max.len=2)
  checkmate::assertNumeric(pad, finite=TRUE, any.missing=FALSE, min.len=1, max.len=2)
  checkmate::assertNumeric(padin, finite=TRUE, any.missing=FALSE, min.len=1, max.len=2)

  inset <- rep(inset, length.out=2)
  pad <- rep(pad, length.out=2)
  padin <- rep(padin, length.out=2)

  padx <- sum(inset[1] * w, pad[1], padin[1] * w / pin[1])
  pady <- sum(inset[2] * h, pad[2], padin[2] * h / pin[2])

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
  xy
}
