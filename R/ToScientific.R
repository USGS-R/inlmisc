#' Format for Scientific Notation
#'
#' This function formats numbers in scientific notation \eqn{m \times 10^{n}}{m x 10^n}.
#'
#' @param x 'numeric'.
#'   Vector of numbers
#' @param digits 'integer'.
#'   Desired number of digits after the decimal point.
#' @param type 'character'.
#'   Specify \code{"latex"} to return numbers in the LaTeX markup language (default),
#'   or \code{"plotmath"} to return as \code{\link[grDevices]{plotmath}} expressions.
#' @param na 'character'.
#'   String to be used for missing values (\code{NA}).
#'   By default, no string substitution is made for missing values.
#' @param delimiter 'character'.
#'   Delimiter for LaTeX mathematical mode, inline (\code{$...$}) by default.
#'   Does not apply to missing value strings.
#' @param scipen 'integer'.
#'   A penalty to be applied when deciding to format numeric values in scientific or fixed notation.
#'   Positive values bias towards fixed and negative towards scientific notation:
#'   fixed notation will be preferred unless it is more than \code{scipen} digits wider.
#'   Specify \code{NULL} to format all numbers, with the exception of zero, in scientific notation.
#' @param big.mark 'character'.
#'   Mark inserted between every big interval before the decimal point.
#'   By default, commas are placed every 3 decimal places for numbers larger than 999.
#' @param ...
#'   Not used
#'
#' @return
#'   For \code{type = "latex"}, returns a 'character' vector of the same length as argument \code{x}.
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
#' ToScientific(x, digits = 2L, na = "---")
#'
#' ToScientific(x, digits = 2L, scipen = 0L)
#'
#' x <- seq(0, 20000, by = 4000)
#' ToScientific(x, scipen = 0)
#'
#' lab <- ToScientific(x, type = "plotmath", scipen = 0L)
#' i <- seq_along(x)
#' plot(i, type = "n", xaxt = "n", yaxt = "n", ann = FALSE)
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
  checkmate::assertString(delimiter)
  checkmate::assertInt(scipen, na.ok=TRUE, null.ok=TRUE)
  checkmate::assertString(big.mark)

  if (missing(type) && methods::hasArg("lab.type"))
    type <- list(...)$lab.type  # include for backward compatibility
  else
    type <- match.arg(type)

  x <- as.numeric(x)

  is_zero <- x %in% 0
  x[is_zero] <- NA

  is_finite <- is.finite(x)
  if (is.null(scipen)) {
    is_sci <- is.finite(x)
  } else {
    op <- options(scipen=scipen); on.exit(options(op))
    is_sci <- grepl("e", formatC(x)) & is.finite(x)
  }
  is_fix <- !is_sci & is_finite

  # scientific notation
  if (any(is_sci)) {
    m <- rep(as.numeric(NA), length(x))
    n <- m
    n[is_sci] <- floor(log(abs(x[is_sci]), 10))
    m[is_sci] <- x[is_sci] / 10^n[is_sci]
  }

  # decimal places
  digits_fix <- if (is.null(digits) && any(is_fix)) format.info(x[is_fix])[2] else digits
  digits_sci <- if (is.null(digits) && any(is_sci)) format.info(m[is_sci])[2] else digits

  # latex markup
  if (type == "latex") {
    s <- rep(na, length(x))
    s[is_zero] <- "0"
    if (any(is_fix))
      s[is_fix] <- formatC(x[is_fix], digits_fix, width=1, format="f", big.mark=big.mark)
    if (any(is_sci)) {
      m[is_sci] <- formatC(m[is_sci], digits_sci, width=1, format="f")
      s[is_sci] <- sprintf("%s \\times 10^{%d}", m[is_sci], n[is_sci])
    }
    is <-  is_zero | is_fix | is_sci
    s[is] <- sprintf("%s%s%s", delimiter, s[is], delimiter)

  # plotmath expression
  } else {
    FUN <- function(i) {
      if (is_zero[i]) {
        return("0")
      } else if (is.na(x[i])) {
        return(substitute(X, list(X=na)))
      } else if (is_fix[i]) {
        v <- formatC(x[i], digits_fix, width=1, format="f", big.mark=big.mark,
                     drop0trailing=TRUE)
        return(substitute(X, list(X=v)))
      } else {
        v <- round(m[i], digits_sci)
        return(substitute(M%*%10^N, list(M=v, N=n[i])))
      }
    }
    s <- do.call("expression", lapply(seq_along(x), FUN))
  }

  return(s)
}
