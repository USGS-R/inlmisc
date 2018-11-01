#' Set Hinge Location in Color Palette
#'
#' The \emph{hinge} indicates a dramatic color change in a palette
#' that is typically located at the midpoint of the color range.
#' Data ranges that are asymmetrical can result in a undesired hinge location,
#' a location that does not necessarily coincide with the break-point in the user's data.
#' This function can be used to set the hinge location based on your data.
#'
#' @param x 'numeric' object that can be passed to the \code{\link{range}}
#'   function with \code{NA}'s removed.
#'   User's data range (such as, at sea-level).
#' @param hinge 'numeric' number.
#'   Hinge value in data units.
#' @param scheme 'character' vector of length 1 or 2.
#'   Name of color scheme(s) that are suitable for continuous data types
#'   and allow for color interpolation.
#'   The color palette is derived from one or two color schemes.
#'   See \code{\link{GetColors}} function for a list of scheme names.
#'   Argument choices may be abbreviated as long as there is no ambiguity.
#' @param alpha 'numeric' vector of length 1 or 2.
#'   Alpha transparency, values range from 0 (fully transparent) to 1 (fully opaque).
#'   Specify as \code{NULL} to exclude the alpha channel value from colors.
#' @param reverse 'logical' vector of length 1 or 2.
#'   Whether to reverse the order of colors in the original scheme(s).
#' @param allow_bias 'logical' flag.
#'   Whether to allow bias in the color spacing.
#'
#' @inheritParams GetColors
#'
#' @return Returns a 'function' that takes an 'integer' argument
#'   (the required number of colors) and returns a 'character' vector of colors.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords color
#'
#' @export
#'
#' @examples
#' Pal <- SetHinge(x = c(-3, 7), hinge = 0)
#' Plot <- inlmisc:::plot.inlcol
#' Plot(Pal(255))
#'
#' x <- datasets::volcano
#' Pal <- SetHinge(x, hinge = 140, scheme = c("abyss", "dem1"))
#' filled.contour(x, color.palette = Pal, nlevels = 50)
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
#' op <- par(mfrow = c(3, 1), oma = c(0, 0, 0, 0))
#' Plot(SetHinge(x, hinge, scheme = "oleron")(n))
#' Plot(SetHinge(x, hinge, scheme = "BuRd")(n))
#' Plot(SetHinge(x, hinge, scheme = c("ocean", "copper"))(n))
#' par(op)
#'
#' # Alpha transparency (alpha)
#' x <- c(-5, 5); hinge <- 0; scheme <- c("drywet", "hawaii"); n <- 255
#' op <- par(mfrow = c(4, 1), oma = c(0, 0, 0, 0))
#' Plot(SetHinge(x, hinge, scheme, alpha = 1.0)(n))
#' Plot(SetHinge(x, hinge, scheme, alpha = 0.5)(n))
#' Plot(SetHinge(x, hinge, scheme, alpha = c(1.0, 0.5))(n))
#' Plot(SetHinge(x, hinge, scheme, alpha = c(0.5, 1.0))(n))
#' par(op)
#'
#' # Reverse colors (reverse)
#' x <- c(-10, 10); hinge <- -3; n <- 255
#' op <- par(mfrow = c(6, 1), oma = c(0, 0, 0, 0))
#' Plot(SetHinge(x, hinge, "roma", reverse = FALSE)(n))
#' Plot(SetHinge(x, hinge, "roma", reverse = TRUE)(n))
#' Plot(SetHinge(x, hinge, c("davos", "hawaii"), reverse = FALSE)(n))
#' Plot(SetHinge(x, hinge, c("davos", "hawaii"), reverse = TRUE)(n))
#' Plot(SetHinge(x, hinge, c("davos", "hawaii"), reverse = c(TRUE, FALSE))(n))
#' Plot(SetHinge(x, hinge, c("davos", "hawaii"), reverse = c(FALSE, TRUE))(n))
#' par(op)
#'
#' # Allow bias in color spacing (allow_bias)
#' x <- c(-3, 7); n <- 20
#' op <- par(mfrow = c(4, 1), oma = c(0, 0, 0, 0))
#' Plot(SetHinge(x, hinge = 0, allow_bias = TRUE)(n))
#' Plot(SetHinge(x, hinge = 0, allow_bias = FALSE)(n))
#' Plot(SetHinge(x, hinge = 4, allow_bias = TRUE)(n))
#' Plot(SetHinge(x, hinge = 4, allow_bias = FALSE)(n))
#' par(op)
#'

SetHinge <- function(x, hinge, scheme="sunset", alpha=NULL,
                     reverse=FALSE, allow_bias=TRUE) {

  x <- range(x, na.rm=TRUE)
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE, len=2,
                           unique=TRUE, sorted=TRUE)
  checkmate::assertNumber(hinge, finite=TRUE)
  checkmate::assertCharacter(scheme, min.chars=1, any.missing=FALSE, min.len=1, max.len=2)
  nm <- names(schemes)[vapply(schemes, function(x) x$nmax == Inf, FALSE)]
  scheme <- match.arg(scheme, nm, several.ok=TRUE)
  checkmate::assertNumeric(alpha, lower=0, upper=1, finite=TRUE, any.missing=FALSE,
                           min.len=1, max.len=2, null.ok=TRUE)
  checkmate::assertLogical(reverse, any.missing=FALSE, min.len=1, max.len=2)
  checkmate::assertFlag(allow_bias)

  scheme <- rep(scheme, length.out=2)
  if (!is.null(alpha)) alpha <- rep(alpha, length.out=2)
  reverse <- rep(reverse, length.out=2)

  if (x[1] < hinge & x[2] > hinge) {
    ratio <- diff(c(x[1], hinge)) / diff(x)
  } else if (x[1] < hinge) {
    ratio <- 1
  } else {
    ratio <- 0
  }

  ran <- ifelse(identical(scheme[1], scheme[2]), 0.5, 1)

  if (allow_bias || ratio %in% c(0, 0.5, 1)) {
    adj <- c(0, 0)
  } else {
    d1 <- diff(c(x[1], hinge))
    d2 <- diff(c(hinge, x[2]))
    if (d1 < d2)
      adj <- c(ran * (1 - d1 / d2), 0)
    else
      adj <- c(0, ran * (1 - d2 / d1))
  }

  r1 <- c(adj[1], ran)
  r2 <- c(1 - ran, 1 - adj[2])

  FUN <- function(...) {
    n1 <- round(... * ratio)
    n2 <- ... - n1
    is <- identical(scheme[1], scheme[2]) & n1 > 0 & n2 > 0
    if (is) n2 <- n2 + 1
    p1 <- GetColors(n1, scheme[1], alpha[1], start=r1[1], end=r1[2], reverse=reverse[1])
    p2 <- GetColors(n2, scheme[2], alpha[2], start=r2[1], end=r2[2], reverse=reverse[2])
    if (is) p2 <- p2[-1]
    c(p1, p2)
  }
  FUN
}
