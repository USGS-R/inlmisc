#' Determine Location for Inset Graphics
#'
#' This function determines the location that may be used to
#' position an inset graphics in the main plot region.
#'
#' @param dx,dy 'numeric'.
#'   Width and height of the inset graphics, respectively.
#' @param loc 'character'.
#'   Single keyword used to specify the position of the inset in the main plot region:
#'   \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"},
#'   \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"},
#'   or \code{"center"} to denote inset location.
#' @param inset 'numeric'.
#'   Vector of length 2 giving the inset distance from the margins as a fraction of the main plot region.
#'   Value is recycled as necessary.
#' @param pad 'numeric'.
#'   Vector of length 2 giving the padding distance from the margins in user coordinate units.
#'   Value is recycled as necessary.
#' @param padin 'numeric'.
#'   Vector of length 2 giving the padding distance from the margins in inches.
#'   Value is recycled as necessary.
#'
#' @return Returns a 'numeric' vector of length 2 giving the user coordinates
#'   for the bottom-left corner of the inset.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords hplot
#'
#' @export
#'
#' @examples
#' graphics::plot(NA, NA, xlim = c(0, 100), ylim = c(0, 1),
#'                xlab = "x", ylab = "y", xaxs = "i", yaxs = "i")
#' dx <- 20; dy <- 0.2
#' xy <- GetInsetLocation(dx, dy, loc = "bottomleft")
#' graphics::rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, border = "red")
#' graphics::points(xy[1], xy[2], pch = 16, xpd = TRUE)
#' print(xy)
#'
#' xy <- GetInsetLocation(dx, dy, loc = "bottomleft", inset = 0.05)
#' graphics::rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, border = "pink")
#' graphics::points(xy[1], xy[2], pch = 16)
#' print(xy)
#'
#' xy <- GetInsetLocation(dx, dy, loc = "topright", padin = 0.5)
#' graphics::rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, border = "blue")
#'
#' xy <- GetInsetLocation(dx, dy, loc = "left", pad = c(5, 0))
#' graphics::rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, border = "green")
#'
#' xy <- GetInsetLocation(dx, dy, loc = "center")
#' graphics::rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, border = "brown")
#'

GetInsetLocation <- function(dx, dy, loc="bottomright", inset=0, pad=0, padin=0) {

  if (grDevices::dev.cur() == 1) stop("no active device")
  usr <- graphics::par("usr")  # extremes of the plotting region (x1, x2, y1, y2)
  pin <- graphics::par("pin")  # plot dimensions in inches (width, height)

  w <- diff(usr[1:2])
  h <- diff(usr[3:4])

  choices <- c("bottomright", "bottom", "bottomleft", "left",
               "topleft", "top", "topright", "right", "center")
  loc <- match.arg(loc, choices)
  dx <- checkmate::assertNumber(dx, lower=0, upper=w, finite=TRUE)
  dy <- checkmate::assertNumber(dy, lower=0, upper=h, finite=TRUE)
  inset <- checkmate::assertNumeric(inset, lower=0, upper=1, finite=TRUE,
                                    any.missing=FALSE, min.len=1, max.len=2)
  pad <- checkmate::assertNumeric(pad, finite=TRUE, any.missing=FALSE,
                                  min.len=1, max.len=2)
  padin <- checkmate::assertNumeric(padin, finite=TRUE, any.missing=FALSE,
                                    min.len=1, max.len=2)

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

  return(xy)
}
