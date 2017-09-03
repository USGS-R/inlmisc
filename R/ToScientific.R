#' Format for Scientific Notation
#'
#' This function formats numbers in scientific notation \eqn{m \times 10^{n}}.
#'
#' @param x 'numeric'.
#'   Vector of numbers
#' @param digits 'integer'.
#'   Number of digits after the decimal point for the mantissa.
#' @param lab.type 'character'.
#'   By default, LaTeX formatted strings for labels are returned.
#'   Alternatively, \code{lab.type = "plotmath"} returns plotmath-compatible expressions.
#' @param na 'character'.
#'   String to be used for missing values.
#'   By default, no character string substitution is made for missing values.
#' @param inline.delimiter 'character'.
#'   Delimiter for LaTeX inline mathematical mode.
#' @param scipen 'integer'.
#'   A penalty to be applied when deciding to print numeric values in scientific or fixed notation.
#'   By default all numbers are formatted using scientific notation.
#' @param ...
#'   Arguments passed to the \code{\link{formatC}} function.
#'   Only applies to fixed formatted values.
#'
#' @return For the default \code{lab.type = "latex"}, a 'character' vector of the same length as argument \code{x}.
#'   And for \code{lab.type = "plotmath"}, an expression of the same length as \code{x},
#'   typically with elements of the form \code{m x 10^n}.
#'   In order to comply with \href{https://www.section508.gov}{Section 508},
#'   an "x" is used as the label separator for the \code{plotmath} type---rather than
#'   the more common "\%*\%" seperator.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' x <- c(-1e+09, 0, NA, pi * 10^(-5:5))
#' ToScientific(x, digits = 5L, na = "---")
#' ToScientific(x, digits = 2L, scipen = 0L)
#'
#' x <- exp(log(10) * 1:6)
#' i <- seq_along(x)
#' plot(i, i, type = "n", xaxt = "n", yaxt = "n", ann = FALSE)
#' lab <- ToScientific(x, digits = 0L, lab.type = "plotmath", scipen = 0L, big.mark = ",")
#' axis(1, i, labels = lab)
#' axis(2, i, labels = lab)
#'

ToScientific <- function(x, digits=NULL, lab.type=c("latex", "plotmath"),
                         na=as.character(NA), inline.delimiter="$",
                         scipen=NULL, ...) {

  lab.type <- match.arg(lab.type)
  is.zero <- x == 0
  x[is.zero] <- NA
  is.num <- which(is.finite(x))

  # find the exponent (n) and significand (m) for scientific notation
  m <- rep(NA, length(x))
  n <- m
  n[is.num] <- floor(log(abs(x[is.num]), 10))
  m[is.num] <- x[is.num] / 10^n[is.num]
  if (is.null(digits)) digits <- format.info(m[is.num])[2]
  m[is.num] <- sprintf("%0.*f", digits, m[is.num])

  # fixed notation
  if (!is.null(scipen)) {
    op <- options(scipen=scipen); on.exit(options(op))
    s.fixed <- formatC(x, ...)
    is.fixed <- !grepl("e", s.fixed) & is.finite(x)
  }

  # latex formatted strings
  if (lab.type == "latex") {
    s <- rep(na, length(x))
    s[is.num] <- sprintf("%s%s \\times 10^{%d}%s",
                         inline.delimiter, m[is.num], n[is.num], inline.delimiter)
    s[is.zero] <- "0"
    if (!is.null(scipen)) s[is.fixed] <- s.fixed[is.fixed]

  # plotmath-compatible expressions
  } else {
    FUN <- function(i) {
      if (is.na(x[i])) {
        return(substitute(X, list(X=na)))
      } else if (is.zero[i]) {
        return(quote(0))
      } else if (!is.null(scipen) && is.fixed[i]) {
        return(substitute(X, list(X=s.fixed[i])))
      } else {
        return(substitute(paste(M, " x ", 10^N), list(M=m[i], N=n[i])))
      }
    }
    s <- lapply(seq_along(x), FUN)
    s <- do.call("expression", s)
  }

  return(s)
}
