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
#' ToScientific(x, digits = 2, lab.type = "plotmath")
#'

ToScientific <- function(x, digits=format.info(as.numeric(x))[2],
                         lab.type=c("latex", "plotmath")) {

  lab.type <- match.arg(lab.type)
  x[is.zero <- x == 0] <- NA
  idxs <- which(is.finite(x))

  m <- rep(NA, length(x))
  n <- m
  n[idxs] <- floor(log(abs(x[idxs]), 10))
  m[idxs] <- sprintf("%0.*f", digits, x[idxs] / 10^n[idxs])
  if (lab.type == "latex") {
    s <- rep(NA, length(x))
    s[idxs] <- sprintf("$%s \\times 10^{%d}$", m[idxs], n[idxs])
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
  return(s)
}
