#' Move Midpoint of Color Palette
#'
#' Move the critical midpoint of a diverging color palette.
#'
#' @param x 'numeric' object that can be passed to the \code{\link{range}} function
#'   with \code{NA}'s removed.
#' @param midpoint 'numeric' number.
#'   Critical midpoint of data range, defaults to zero.
#' @param buffer 'numeric' number.
#'   Color level buffer around the critical midpoint measured as a fraction of the color range.
#' @param bias 'logical' flag.
#'   Whether to allow bias in the color spacing.
#' @param scheme 'character' string.
#'   Diverging color scheme name: specify \code{"sunset"}, \code{"BuRd"}, or \code{"PRGn"}.
#'   Schemes are provided by the \code{\link{GetTolColors}} function.
#'   Partial string matching is supported so argument may be abbreviated.
#' @inheritParams GetTolColors
#'
#' @return Returns a 'function' that takes an 'integer' argument (the required number of colors)
#'   and returns a 'character' vector of colors.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords color
#'
#' @export
#'
#' @examples
#' Fun <- MovePaletteMidpoint(c(-3, 7))
#' inlmisc:::plot.Tol(Fun(255))
#'
#' # Range of a set of data (x)
#' n <- 20
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(x = c(-10,  0))(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(x = c( -7,  3))(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(x = c( -5,  5))(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(x = c( -3,  7))(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(x = c(  0, 10))(n))
#' par(op)
#'
#' # Midpoint of data range (midpoint)
#' x <- c(-5, 5); n <- 255
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(x, midpoint = -6)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(x, midpoint = -2)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(x, midpoint =  0)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(x, midpoint =  2)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(x, midpoint =  6)(n))
#' par(op)
#'
#' # Buffer around midpoint (buffer)
#' x <- c(-5, 5); n <- 20
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(x, buffer = 0.0)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(x, buffer = 0.2)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(x, buffer = 0.4)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(x, buffer = 0.6)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(x, buffer = 0.8)(n))
#' par(op)
#'
#' # Bias in color spacing (bias)
#' x <- c(-3, 7); n <- 20
#' op <- par(mfrow = c(2, 1), oma = c(0, 0, 0, 0))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(x, bias = TRUE)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(x, bias = FALSE)(n))
#' par(op)
#'

MovePaletteMidpoint <- function(x, midpoint=0, buffer=0, bias=TRUE,
                                scheme=c("sunset", "BuRd", "PRGn"), alpha=NULL) {

  x <- range(x, na.rm=TRUE)
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE, len=2,
                           unique=TRUE, sorted=TRUE)
  checkmate::assertNumber(midpoint, finite=TRUE)
  checkmate::qassert(buffer, "N1[0, 1)")
  checkmate::assertFlag(bias)
  scheme <- match.arg(scheme)
  checkmate::assertNumber(alpha, lower=0, upper=1, finite=TRUE, null.ok=TRUE)

  buf <- buffer / 2

  if (x[1] < midpoint & x[2] > midpoint) {
    ratio <- diff(c(x[1], midpoint)) / diff(x)
  } else if (x[1] < midpoint) {
    ratio <- 1
  } else {
    ratio <- 0
  }

  if (bias || ratio %in% c(0, 0.5, 1)) {
    adj <- c(0, 0)
  } else {
    d1 <- diff(c(x[1], midpoint))
    d2 <- diff(c(midpoint, x[2]))
    if (d1 < d2)
      adj <- c((0.5 - buf) * (d1 / d2), 0)
    else
      adj <- c(0, (0.5 - buf) * (d2 / d1))
  }

  Fun <- function(...) {
    n1 <- round(... * ratio)
    n2 <- ... - n1
    c(GetTolColors(n1, scheme=scheme, alpha=alpha, start=adj[1], end=0.5 - buf),
      GetTolColors(n2, scheme=scheme, alpha=alpha, start=0.5 + buf, end=1 - adj[2]))
  }
  Fun
}
