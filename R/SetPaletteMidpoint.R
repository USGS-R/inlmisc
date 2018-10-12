#' Set Color Palette Midpoint
#'
#' Move the critical midpoint in a diverging color scheme.
#'
#' @param lim 'numeric' vector of length 2 or more.
#'   Data range or values that will be used to calculate
#'   the interval between the smallest and largest values.
#' @param mid 'numeric' number.
#'   Critical midpoint of data range.
#' @param scheme 'character' string.
#'   Diverging color scheme name: specify \code{"sunset"}, \code{"BuRd"}, or \code{"PRGn"}.
#' @param buffer 'numeric' number [0, 0.5).
#'   Color level buffer around critical midpoint.
#'
#' @return Returns a 'function' that takes an 'integer' argument (the required number of colors)
#'   and returns a 'character' vector of colors.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{GetTolColors}}
#'
#' @keywords color
#'
#' @export
#'
#' @examples
#' Fun <- SetPaletteMidpoint(lim = c(-3, 7))
#' inlmisc:::plot.Tol(Fun(10))
#'
#' # Data range (lim)
#' op <- par(mfrow = c(3, 1), oma = c(0, 0, 0, 0))
#' inlmisc:::plot.Tol(SetPaletteMidpoint(lim = c( -5,  5))(10))
#' inlmisc:::plot.Tol(SetPaletteMidpoint(lim = c(-10,  0))(10))
#' inlmisc:::plot.Tol(SetPaletteMidpoint(lim = c(  0, 10))(10))
#' par(op)
#'
#' # Diverging color schemes (scheme)
#' lim <- c(-5, 5)
#' op <- par(mfrow = c(3, 1), oma = c(0, 0, 0, 0))
#' inlmisc:::plot.Tol(SetPaletteMidpoint(lim, scheme = "sunset")(255))
#' inlmisc:::plot.Tol(SetPaletteMidpoint(lim, scheme = "BuRd")(255))
#' inlmisc:::plot.Tol(SetPaletteMidpoint(lim, scheme = "PRGn")(255))
#' par(op)
#'
#' # Midpoint of data range (mid)
#' lim <- c(-5, 5)
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' inlmisc:::plot.Tol(SetPaletteMidpoint(lim, mid = -6)(255))
#' inlmisc:::plot.Tol(SetPaletteMidpoint(lim, mid = -2)(255))
#' inlmisc:::plot.Tol(SetPaletteMidpoint(lim, mid =  0)(255))
#' inlmisc:::plot.Tol(SetPaletteMidpoint(lim, mid =  2)(255))
#' inlmisc:::plot.Tol(SetPaletteMidpoint(lim, mid =  6)(255))
#' par(op)
#'
#' # Color level buffer (buffer)
#' lim <- c(-5, 5)
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' inlmisc:::plot.Tol(SetPaletteMidpoint(lim, buffer = 0.0)(10))
#' inlmisc:::plot.Tol(SetPaletteMidpoint(lim, buffer = 0.1)(10))
#' inlmisc:::plot.Tol(SetPaletteMidpoint(lim, buffer = 0.2)(10))
#' inlmisc:::plot.Tol(SetPaletteMidpoint(lim, buffer = 0.3)(10))
#' inlmisc:::plot.Tol(SetPaletteMidpoint(lim, buffer = 0.4)(10))
#' par(op)
#'

SetPaletteMidpoint <- function(lim, mid=0, scheme=c("sunset", "BuRd", "PRGn"), buffer=0) {

  checkmate::assertNumeric(lim, all.missing=FALSE, min.len=2)
  checkmate::assertNumber(mid, finite=TRUE)
  scheme <- match.arg(scheme)
  checkmate::qassert(buffer, "N1[0, 0.5)")

  lim <- range(lim, na.rm=TRUE)
  if (lim[1] == lim[2]) stop()

  if (lim[1] < mid & lim[2] > mid) {
    ratio <- diff(c(lim[1], mid)) / diff(lim)
  } else if (lim[1] < mid) {
    ratio <- 1
  } else {
    ratio <- 0
  }

  Fun <- function(...) {
    Pal1 <- GetTolColors(scheme=scheme, start=0, end=0.5 - buffer)
    Pal2 <- GetTolColors(scheme=scheme, start=0.5 + buffer, end=1)
    n1 <- round(... * ratio)
    n2 <- ... - n1
    c(Pal1(n1), Pal2(n2))
  }
  Fun
}
