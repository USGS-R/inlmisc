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
#' @param inline.delimiter 'character'.
#'   Delimiter for LaTeX inline mathematical mode.
#' @param scipen 'integer'.
#'   A penalty to be applied when deciding to print numeric values in scientific or fixed notation.
#'   By default all numbers are formatted using scientific notation.
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
#' ToScientific(x, digits = 2)
#' ToScientific(x, digits = 2, scipen = 0L)
#' ToScientific(x, digits = 2, lab.type = "plotmath")
#'
#' x <- seq(0, 2e+06, length.out = 5)
#' ToScientific(x)
#'

ToScientific <- function(x, digits=NULL, lab.type=c("latex", "plotmath"),
                         inline.delimiter="$", scipen=NULL) {

  lab.type <- match.arg(lab.type)
  x[is.zero <- x == 0] <- NA
  idxs <- which(is.finite(x))

  m <- rep(NA, length(x))
  n <- m
  n[idxs] <- floor(log(abs(x[idxs]), 10))
  m[idxs] <- x[idxs] / 10^n[idxs]

  if (is.null(digits)) digits <- format.info(m[idxs])[2]

  m[idxs] <- sprintf("%0.*f", digits, m[idxs])
  if (lab.type == "latex") {
    s <- rep(NA, length(x))
    if (!is.character(inline.delimiter)) inline.delimiter <- ""
    s[idxs] <- sprintf("%s%s \\times 10^{%d}%s",
                       inline.delimiter, m[idxs], n[idxs], inline.delimiter)
    s[is.zero] <- "0"
  } else {
    FUN <- function(i) {
      if (i %in% which(is.zero)) return(quote(0))
      if (is.na(x[i])) return("")
      return(substitute(paste(M, " x ", 10^N), list(M=m[i], N=n[i])))
    }
    s <- lapply(seq_along(x), FUN)
    s <- do.call("expression", s)
  }

  # fixed notation
  if (!is.null(scipen) & is.integer(scipen)) {
    op <- options(scipen=scipen)
    on.exit(options(op))
    ss <- formatC(x, big.mark=",")
    is <- !grepl("e", ss) & is.finite(x)
    s[is] <- ss[is]
  }

  return(s)
}
