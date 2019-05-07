#' Add Vertical Intervals to Plot
#'
#' Add vertical interval symbols (also known as error bars) to plots.
#'
#' @param x 'numeric' or 'Date' vector.
#'   \emph{x} coordinate of interval symbols.
#' @param y0 'numeric' vector.
#'   \emph{y} coordinate of points from which to draw.
#' @param y1 'numeric' vector.
#'   \emph{y} coordinate of points to which to draw.
#' @param length 'numeric' number.
#'   Horizontal length of the interval head, in inches.
#' @param code 'integer' number.
#'   Determines the type of interval to be drawn, see \sQuote{Details} section for code descriptions.
#' @param col,lty,lwd,xpd graphical parameters; see \code{\link{par}} for details.
#'   \code{NA} values in \code{col} cause the interval to be omitted.
#'
#' @details For each \code{i}, an interval is drawn between the
#'   point \code{(x[i], y0[i])} and the point \code{(x[i], y1[i])}.
#'
#'   Code descriptions are as follows:
#'   \code{code = 0}, no interval head is drawn;
#'   \code{code = 1}, a head is drawn at \code{(x[i], y0[i])};
#'   \code{code = 2}, a head is drawn at \code{(x[i], y1[i])}; and
#'   \code{code = 3}, a head is drawn at both ends of the interval.
#'
#' @return Used for the side-effect of interval symbols drawn on the current graphics device.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords hplot
#'
#' @export
#'
#' @examples
#' set.seed(2)
#' x <- stats::runif(12)
#' y <- stats::rnorm(12)
#' plot(x, y)
#'
#' dy <- sort(y) / 5
#' AddIntervals(x, y - dy, y + dy, col = "red", xpd = TRUE)
#'

AddIntervals <- function(x, y0, y1, length=0.03, code=3,
                         col=graphics::par("fg"),
                         lty=graphics::par("lty"),
                         lwd=graphics::par("lwd"),
                         xpd=graphics::par("xpd")) {

  x <- as.numeric(x)
  checkmate::assertNumeric(y0, len=length(x))
  checkmate::assertNumeric(y1, len=length(x))
  checkmate::assertNumber(length, lower=0, finite=TRUE)
  checkmate::assertInt(code, lower=0, upper=3)

  is <- is.finite(x) & is.finite(y0) & is.finite(y1)
  if (any(is)) {
    x <- x[is]
    y0 <- y0[is]
    y1 <- y1[is]
  } else {
    return(invisible(NULL))
  }

  n <- length(x)
  col <- rep_len(col, n)
  lty <- rep_len(lty, n)
  lwd <- rep_len(lwd, n)

  units <- graphics::par(c("usr", "pin"))
  x_to_inches <- with(units, pin[1] / diff(usr[1:2]))
  y_to_inches <- with(units, pin[2] / diff(usr[3:4]))

  dx <- length / x_to_inches
  m <- cbind(x - dx / 2, x + dx / 2)

  idx <- which(abs(y1 - y0) * y_to_inches > 0.001)
  if (length(idx) > 0)
    graphics::segments(x[idx], y0[idx], y1=y1[idx],
                       col=col[idx], lty=lty[idx], lwd=lwd[idx], xpd=xpd, lend=2)

  if (code == 0L) return(invisible(NULL))
  if (code %in% c(1L, 3L))
    graphics::segments(m[, 1], y0, x1=m[, 2], col=col, lwd=lwd, xpd=xpd, lend=0)
  if (code %in% c(2L, 3L))
    graphics::segments(m[, 1], y1, x1=m[, 2], col=col, lwd=lwd, xpd=xpd, lend=0)

  invisible()
}
