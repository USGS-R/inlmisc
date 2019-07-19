#' Add Interval Symbols to Plot
#'
#' Add interval symbols (also known as error bars) to plots.
#'
#' @param x 'numeric' or 'Date' vector.
#'   \emph{x} coordinate of interval symbols.
#' @param y0 'numeric' vector.
#'   \emph{y} coordinate of points from which to draw.
#' @param y1 'numeric' vector.
#'   \emph{y} coordinate of points to which to draw.
#' @param hin 'numeric' number.
#'   Horizontal length of an interval head, in inches.
#' @param col,lty,lwd,cex,xpd graphical parameters; see \code{\link[graphics]{par}} for details.
#'   \code{NA} values in \code{col} cause the interval to be omitted.
#' @param ...
#'   Additional graphical parameters to the \code{\link[graphics]{points}} function.
#' @param nondetects 'list'.
#'   If specified, graphical parameters used for left- and right-censored data.
#'   Passed arguments include \code{col}, \code{lty}, and \code{lwd}.
#'
#' @details For each observation \code{i}, the data type is identified using
#'     \code{(y0[i], Inf)} for right-censored,
#'     \code{y0[i] = y1[i]} for exact, and
#'     \code{(-Inf, y1[i])} for left-censored, and
#'     \code{(y0[i], y1[i])} for interval-censored.
#'   Where infinity may be represented with either \code{Inf} or \code{NA}.
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
#' dy <- sort.int(y) / 5
#' AddIntervals(x, y - dy, y + dy, col = "red", xpd = TRUE)
#'
#' x <- sort.int(stats::runif(12, max = 100))
#' y0 <- stats::runif(12, max = 100)
#' y1 <- y0
#' plot(NA, xlim = range(x), ylim = range(c(y0, y1)), xlab = "x", ylab = "y")
#' y0[1:4] <- -Inf
#' y1[5:8] <-  Inf
#' AddIntervals(x, y0, y1, col = "blue", xpd = TRUE)
#' print(cbind(x, y0, y1))
#'

AddIntervals <- function(x, y0, y1, hin=NULL, col="black", lty=1, lwd=0.7,
                         cex=1, xpd=FALSE, ..., nondetects=NULL) {

  x <- as.numeric(x)
  checkmate::assertNumeric(x, finite=TRUE, min.len=1)
  checkmate::assertNumeric(y0, len=length(x))
  checkmate::assertNumeric(y1, len=length(x))
  checkmate::assertNumber(hin, lower=0, finite=TRUE, null.ok=TRUE)
  checkmate::assertList(nondetects, max.len=3, null.ok=TRUE)

  is_y0 <- is.finite(y0)
  is_y1 <- is.finite(y1)
  if (all(!is_y0 & !is_y1)) return(invisible(NULL))

  event <- rep(as.integer(NA), length(x))
  is0 <-  is_y0 & !is_y1
  is1 <-  is_y0 &  is_y1 & y0 == y1
  is2 <- !is_y0 &  is_y1
  is3 <-  is_y0 &  is_y1 & y0 != y1
  y1[is0] <- graphics::par("usr")[4]
  y0[is2] <- graphics::par("usr")[3]
  event[is0] <- 0L  # right censored
  event[is1] <- 1L  # exact
  event[is2] <- 2L  # left censored
  event[is3] <- 3L  # interval censored
  if (anyNA(event)) stop("problem identifying data type")

  col   <- rep_len(col,   length(x))
  lty   <- rep_len(lty,   length(x))
  lwd   <- rep_len(lwd,   length(x))
  event <- rep_len(event, length(x))

  if (is.list(nondetects)) {
    is <- is0 | is2
    if (!is.null(nondetects$col)) col[is] <- nondetects$col
    if (!is.null(nondetects$lty)) lty[is] <- nondetects$lty
    if (!is.null(nondetects$lwd)) lwd[is] <- nondetects$lwd
  }

  units <- graphics::par(c("usr", "pin"))
  x_to_inches <- with(units, pin[1] / diff(usr[1:2]))
  y_to_inches <- with(units, pin[2] / diff(usr[3:4]))

  if (is.null(hin)) hin <- graphics::par("cin")[2] * 0.75 / 4 * cex
  dx <- hin / x_to_inches
  m <- cbind(x - dx / 2, x + dx / 2)

  idx <- which(abs(y1 - y0) * y_to_inches > 0.001 & event %in% c(0L, 2L, 3L))
  if (length(idx) > 0)
    graphics::segments(x[idx], y0[idx], y1=y1[idx], col=col[idx], lty=lty[idx],
                       lwd=lwd[idx], xpd=xpd, lend=2)

  if (any(is <- event %in% c(0L, 3L)))
    graphics::segments(m[is, 1], y0[is], x1=m[is, 2], col=col[is], lwd=lwd[is],
                       xpd=xpd, lend=0)
  if (any(is <- event %in% c(2L, 3L)))
    graphics::segments(m[is, 1], y1[is], x1=m[is, 2], col=col[is], lwd=lwd[is],
                       xpd=xpd, lend=0)

  if (any(is <- event == 1L))
    graphics::points(x[is], y0[is], col=col[is], cex=cex, lwd=lwd[is], xpd=xpd, ...)

  invisible()
}
