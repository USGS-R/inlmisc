#' Set Hinge Location in Color Palette
#'
#' The \emph{hinge} indicates a dramatic color change in a palette and
#' is typically located at the midpoint of the color range.
#' Data ranges that are asymmetrical can result in a hinge location that does not
#' necessarily coincide with the desired break-point in the user's data (such as, at sea-level).
#' This function is used to establish a new hinge location in the color palette.
#'
#' @param x 'numeric' object that can be passed to the \code{\link{range}}
#'   function with \code{NA}'s removed.
#'   User's data range
#' @param hinge 'numeric' number.
#'   Hinge value in data units.
#' @param scheme 'character' string.
#'   Name of color scheme that is suitable for continuous data types and allows for color interpolation.
#'   See \code{\link{GetColors}} function for options.
#'   Argument choices may be abbreviated as long as there is no ambiguity.
#' @param buffer 'numeric' number.
#'   Buffer around hinge value measured as a fraction of the full color range.
#' @param bias 'logical' flag.
#'   Whether to allow bias in the color spacing.
#' @inheritParams GetColors
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
#' Fun <- SetHinge(c(-3, 7), 0)
#' Plot <- inlmisc:::plot.inlcol
#' Plot(Fun(255))
#'
#' # Data range (x)
#' hinge <- 0; n <- 20
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' Plot(SetHinge(c(-10,  0), hinge)(n))
#' Plot(SetHinge(c( -7,  3), hinge)(n))
#' Plot(SetHinge(c( -5,  5), hinge)(n))
#' Plot(SetHinge(c( -3,  7), hinge)(n))
#' Plot(SetHinge(c(  0, 10), hinge)(n))
#' par(op)
#'
#' # Hinge value (hinge)
#' x <- c(-5, 5); n <- 255
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' Plot(SetHinge(x, hinge = -6)(n))
#' Plot(SetHinge(x, hinge = -2)(n))
#' Plot(SetHinge(x, hinge =  0)(n))
#' Plot(SetHinge(x, hinge =  2)(n))
#' Plot(SetHinge(x, hinge =  6)(n))
#' par(op)
#'
#' # Color scheme (scheme)
#' x <- c(-10, 10); hinge <- -3; n <- 255
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' Plot(SetHinge(x, hinge, scheme = "oleron")(n))
#' Plot(SetHinge(x, hinge, scheme = "BuRd")(n))
#' Plot(SetHinge(x, hinge, scheme = "PRGn")(n))
#' Plot(SetHinge(x, hinge, scheme = "roma")(n))
#' Plot(SetHinge(x, hinge, scheme = "vik")(n))
#' par(op)
#'
#' # Buffer around hinge (buffer)
#' x <- c(-5, 5); hinge <- 0; n <- 20
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' Plot(SetHinge(x, hinge, buffer = 0.0)(n))
#' Plot(SetHinge(x, hinge, buffer = 0.2)(n))
#' Plot(SetHinge(x, hinge, buffer = 0.4)(n))
#' Plot(SetHinge(x, hinge, buffer = 0.6)(n))
#' Plot(SetHinge(x, hinge, buffer = 0.8)(n))
#' par(op)
#'
#' # Bias in color spacing (bias)
#' x <- c(-3, 7); hinge <- 0; n <- 20
#' op <- par(mfrow = c(2, 1), oma = c(0, 0, 0, 0))
#' Plot(SetHinge(x, hinge, bias = TRUE)(n))
#' Plot(SetHinge(x, hinge, bias = FALSE)(n))
#' par(op)
#'

SetHinge <- function(x, hinge, scheme="sunset", buffer=0, bias=TRUE, alpha=NULL) {

  x <- range(x, na.rm=TRUE)
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE, len=2,
                           unique=TRUE, sorted=TRUE)
  checkmate::assertNumber(hinge, finite=TRUE)
  nm <- names(schemes)[vapply(schemes, function(x) x$nmax == Inf, FALSE)]
  scheme <- match.arg(scheme, nm)
  checkmate::qassert(buffer, "N1[0, 1)")
  checkmate::assertFlag(bias)
  checkmate::assertNumber(alpha, lower=0, upper=1, finite=TRUE, null.ok=TRUE)

  buf <- buffer / 2

  if (x[1] < hinge & x[2] > hinge) {
    ratio <- diff(c(x[1], hinge)) / diff(x)
  } else if (x[1] < hinge) {
    ratio <- 1
  } else {
    ratio <- 0
  }

  if (bias || ratio %in% c(0, 0.5, 1)) {
    adj <- c(0, 0)
  } else {
    d1 <- diff(c(x[1], hinge))
    d2 <- diff(c(hinge, x[2]))
    if (d1 < d2)
      adj <- c((0.5 - buf) * (d1 / d2), 0)
    else
      adj <- c(0, (0.5 - buf) * (d2 / d1))
  }

  FUN <- function(...) {
    n1 <- round(... * ratio)
    n2 <- ... - n1
    c(GetColors(n1, scheme=scheme, alpha=alpha, start=adj[1], end=0.5 - buf),
      GetColors(n2, scheme=scheme, alpha=alpha, start=0.5 + buf, end=1 - adj[2]))
  }
  FUN
}
