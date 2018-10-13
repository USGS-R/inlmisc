#' Move Midpoint of Color Palette
#'
#' Move the critical midpoint of a diverging color palette.
#'
#' @param ran 'numeric' vector of length 2 or more.
#'   Data range or values that will be used to calculate the range.
#' @param mid 'numeric' number.
#'   Critical midpoint of data range, defaults to 0.
#' @param buffer 'numeric' number.
#'   Color level buffer around the critical midpoint measured as a fraction of the data range.
#' @param bias 'logical' flag.
#'   Whether to allow bias in the color spacing.
#' @param scheme 'character' string.
#'   Diverging color scheme name: specify \code{"sunset"}, \code{"BuRd"}, or \code{"PRGn"}.
#'   Partial string matching is supported so argument may be abbreviated.
#' @inheritParams GetTolColors
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
#' Fun <- MovePaletteMidpoint(ran = c(-3, 7))
#' inlmisc:::plot.Tol(Fun(255))
#'
#' # Data range (ran)
#' n <- 10
#' op <- par(mfrow = c(3, 1), oma = c(0, 0, 0, 0))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(ran = c( -5,  5))(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(ran = c(-10,  0))(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(ran = c(  0, 10))(n))
#' par(op)
#'
#' # Midpoint of data range (mid)
#' ran <- c(-5, 5); n <- 255
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(ran, mid = -6)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(ran, mid = -2)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(ran, mid =  0)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(ran, mid =  2)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(ran, mid =  6)(n))
#' par(op)
#'
#' # Buffer around midpoint (buffer)
#' ran <- c(-5, 5); n <- 10
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(ran, buffer = 0.0)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(ran, buffer = 0.2)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(ran, buffer = 0.4)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(ran, buffer = 0.6)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(ran, buffer = 0.8)(n))
#' par(op)
#'
#' # Bias in color spacing (bias)
#' ran <- c(-3, 7); n <- 20
#' op <- par(mfrow = c(2, 1), oma = c(0, 0, 0, 0))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(ran, bias = TRUE)(n))
#' inlmisc:::plot.Tol(MovePaletteMidpoint(ran, bias = FALSE)(n))
#' par(op)
#'

MovePaletteMidpoint <- function(ran, mid=0, buffer=0, bias=TRUE,
                                scheme=c("sunset", "BuRd", "PRGn"), alpha=NULL) {

  checkmate::assertNumeric(ran, all.missing=FALSE, min.len=2)
  checkmate::assertNumber(mid, finite=TRUE)
  checkmate::qassert(buffer, "N1[0, 1)")
  checkmate::assertFlag(bias)
  scheme <- match.arg(scheme)
  checkmate::assertNumber(alpha, lower=0, upper=1, finite=TRUE, null.ok=TRUE)

  ran <- range(ran, na.rm=TRUE)
  if (ran[1] == ran[2]) stop()

  buf <- buffer / 2

  if (ran[1] < mid & ran[2] > mid) {
    ratio <- diff(c(ran[1], mid)) / diff(ran)
  } else if (ran[1] < mid) {
    ratio <- 1
  } else {
    ratio <- 0
  }

  if (bias || ratio %in% c(0, 0.5, 1)) {
    adj <- c(0, 0)
  } else {
    d1 <- diff(c(ran[1], mid))
    d2 <- diff(c(mid, ran[2]))
    if (d1 < d2)
      adj <- c((0.5 - buf) * (d1 / d2), 0)
    else
      adj <- c(0, (0.5 - buf) * (d2 / d1))
  }

  Fun <- function(...) {
    n1 <- round(... * ratio)
    n2 <- ... - n1
    c(GetTolColors(n1, scheme=scheme, alpha=alpha, start=0 + adj[1], end=0.5 - buf),
      GetTolColors(n2, scheme=scheme, alpha=alpha, start=0.5 + buf, end=1 - adj[2]))
  }
  Fun
}
