#' Format for Scientific Notation
#'
#' This function formats numbers in scientific notation \eqn{m \times 10^{n}}{m x 10^n}.
#'
#' @param x 'numeric'.
#'   Vector of numbers
#' @param digits 'integer'.
#'   Number of digits after the decimal point for the coefficent part
#'   of the number in scientific notation (also known as the significand).
#'   The default, \code{NULL}, is 2 for integer and 4 for real numbers.
#' @param type 'character'.
#'   Specify \code{"latex"} to return numbers in the LaTeX markup language (default),
#'   or \code{"plotmath"} to return as \code{\link[grDevices]{plotmath}} expressions.
#' @param na 'character'.
#'   String to be used for missing values (\code{NA}).
#'   By default, no string substitution is made for missing values.
#' @param delimiter 'character'.
#'   Delimiter for LaTeX mathematical mode, inline (\code{$...$}) by default.
#' @param scipen 'integer'.
#'   A penalty to be applied when deciding to format numeric values in scientific or fixed notation.
#'   Positive values bias towards fixed and negative towards scientific notation:
#'   fixed notation will be preferred unless it is more than \code{scipen} digits wider.
#'   By default, all numbers, with the exception of zero, are formatted in scientific notation.
#' @param big.mark 'character'.
#'   Mark inserted between every big interval before the decimal point.
#'   By default, commas are placed every 3 decimal places for numbers larger than 999.
#' @param ...
#'   Not used
#'
#' @return For \code{type = "latex"}, returns a 'character' vector of the same length as argument \code{x}.
#'   And for \code{type = "plotmath"}, returns an 'expression' vector of the same length as \code{x}.
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
#'
#' ToScientific(x, digits = 2L, scipen = 0L)
#'
#' x <- exp(log(10) * 1:6)
#' i <- seq_along(x)
#' plot(i, i, type = "n", xaxt = "n", yaxt = "n", ann = FALSE)
#' lab <- ToScientific(x, 0L, type = "plotmath", scipen = 0L)
#' axis(1, i, labels = lab)
#' axis(2, i, labels = lab)
#'

ToScientific <- function(x, digits=NULL, type=c("latex", "plotmath"),
                         na=as.character(NA), delimiter="$", scipen=NULL,
                         big.mark=",", ...) {

  # check arguments
  checkmate::assertNumeric(x)
  checkmate::assertCount(digits, null.ok=TRUE)
  checkmate::assertString(na, na.ok=TRUE)
  checkmate::assertInt(scipen, na.ok=TRUE, null.ok=TRUE)

  if (missing(type) && methods::hasArg("lab.type"))
    type <- list(...)$lab.type  # included for backward compatibility
  else
    type <- match.arg(type)

  x[is.zero <- x == 0] <- NA
  is.num <- which(is.finite(x))

  # scientific notation
  m <- rep(NA, length(x))
  n <- m
  n[is.num] <- floor(log(abs(x[is.num]), 10))
  m[is.num] <- x[is.num] / 10^n[is.num]
  if (is.null(digits)) digits <- if (is.integer(x)) 2 else 4
  m[is.num] <- round(m[is.num], digits)

  # fixed notation
  if (!is.null(scipen)) {
    op <- options(scipen=scipen); on.exit(options(op))
    s.fix <- formatC(x, digits, width=1, format="f", big.mark=",")
    is.fix <- !grepl("e", formatC(x)) & is.finite(x)
  }

  # latex markup
  if (type == "latex") {
    s <- rep(na, length(x))
    m[is.num] <- formatC(m[is.num], digits, width=1, format="f")
    s[is.num] <- sprintf("%s%s \\times 10^{%d}%s",
                         delimiter, m[is.num], n[is.num], delimiter)
    s[is.zero] <- "0"
    if (!is.null(scipen)) s[is.fix] <- s.fix[is.fix]

  # plotmath expression
  } else if (type == "plotmath") {

    FUN <- function(i) {
      if (is.zero[i]) {
        return(quote(0))
      } else if (is.na(x[i])) {
        return(substitute(X, list(X=na)))
      } else if (!is.null(scipen) && is.fix[i]) {
        return(substitute(X, list(X=s.fix[i])))
      } else {
        return(substitute(M%*%10^N, list(M=m[i], N=n[i])))
      }
    }
    s <- do.call("expression", lapply(seq_along(x), FUN))
  }

  return(s)
}
