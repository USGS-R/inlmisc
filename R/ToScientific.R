#' Format for Scientific Notation
#'
#' Format numbers in scientific notation \eqn{m \times 10^{n}}{m x 10^n}.
#'
#' @param x 'numeric' vector.
#'   Numbers
#' @param digits 'integer' count.
#'   Desired number of digits after the decimal point.
#' @param type 'character' string.
#'   Specify \code{"latex"} to return numbers in the LaTeX markup language (default),
#'   or \code{"plotmath"} to return as \code{\link[grDevices]{plotmath}} expressions.
#' @param na 'character' string.
#'   Value to use for missing values (\code{NA}).
#'   By default, no string substitution is made for missing values.
#' @param zero 'character' string.
#'   Value to use for zero values.
#'   Specify as \code{NULL} to prevent string substitution.
#' @param delimiter 'character' string.
#'   Delimiter for LaTeX mathematical mode, inline (\code{$...$}) by default.
#'   Does not apply to missing value strings.
#' @param scipen 'integer' count.
#'   Penalty to be applied when deciding to format numeric values in scientific or fixed notation.
#'   Positive values bias towards fixed and negative towards scientific notation:
#'   fixed notation will be preferred unless it is more than \code{scipen} digits wider.
#'   Specify \code{NULL} to format all numbers, with the exception of zero, in scientific notation.
#' @param big.mark 'character' string.
#'   Mark inserted between every big interval before the decimal point.
#'   By default, commas are placed every 3 decimal places for numbers larger than 999.
#' @param ...
#'   Not used
#'
#' @return
#'   When \code{type = "latex"} returns a 'character' vector of the same length as argument \code{x}.
#'   And when \code{type = "plotmath"} returns a 'expression' vector of the same length as \code{x}.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' x <- c(-1e+09, 0, NA, pi * 10^(-5:5))
#' ToScientific(x, digits = 2, na = "---")
#'
#' ToScientific(x, digits = 2, scipen = 0)
#'
#' x <- seq(0, 20000, by = 4000)
#' ToScientific(x, scipen = 0)
#'
#' lab <- ToScientific(x, type = "plotmath", scipen = 0)
#' i <- seq_along(x)
#' plot(i, type = "n", xaxt = "n", yaxt = "n", ann = FALSE)
#' axis(1, i, labels = lab)
#' axis(2, i, labels = lab)
#'

ToScientific <- function(x, digits=NULL, type=c("latex", "plotmath"),
                         na=as.character(NA), zero="0", delimiter="$",
                         scipen=NULL, big.mark=",", ...) {

  # check arguments
  checkmate::assertNumeric(x)
  checkmate::assertCount(digits, null.ok=TRUE)
  if (missing(type) && methods::hasArg("lab.type"))  # backward compatibility
    type <- list(...)$lab.type
  else
    type <- match.arg(type)
  checkmate::assertString(na, na.ok=TRUE)
  checkmate::assertString(zero, null.ok=TRUE)
  checkmate::assertString(delimiter)
  checkmate::assertInt(scipen, na.ok=TRUE, null.ok=TRUE)
  checkmate::assertString(big.mark)

  x <- as.numeric(x)

  is_zero <- if (is.null(zero)) rep(FALSE, length(x)) else x %in% 0
  x[is_zero] <- NA

  is_finite <- is.finite(x)
  if (is.null(scipen)) {
    is_sci <- is.finite(x)
  } else {
    fmt <- prettyNum(x, scientific=scipen)
    is_sci <- grepl("e", fmt) & is.finite(x)
  }
  is_fix <- !is_sci & is_finite

  # scientific notation
  if (any(is_sci)) {
    m <- rep(0, length(x))
    n <- m
    is <- is_sci & !x %in% 0
    n[is] <- floor(log(abs(x[is]), 10))
    m[is] <- x[is] / 10^n[is]
  }

  # decimal places
  digits_fix <- if (is.null(digits) && any(is_fix)) format.info(x[is_fix])[2] else digits
  digits_sci <- if (is.null(digits) && any(is_sci)) format.info(m[is_sci])[2] else digits

  # latex markup
  if (type == "latex") {
    s <- rep(na, length(x))
    s[is_zero] <- zero
    if (any(is_fix))
      s[is_fix] <- formatC(x[is_fix], digits_fix, width=1, format="f", big.mark=big.mark)
    if (any(is_sci)) {
      m[is_sci] <- formatC(m[is_sci], digits_sci, width=1, format="f")
      s[is_sci] <- sprintf("{%s} \\times 10^{%d}", m[is_sci], n[is_sci])
    }
    is <-  is_zero | is_fix | is_sci
    s[is] <- sprintf("%s%s%s", delimiter, s[is], delimiter)

  # plotmath expression
  } else {
    s <- do.call("expression", lapply(seq_along(x), function(i) {
      if (is_zero[i]) {
        return(zero)
      } else if (is.na(x[i])) {
        return(substitute(X, list("X"=na)))
      } else if (is_fix[i]) {
        v <- formatC(x[i], digits_fix, width=1, format="f", big.mark=big.mark,
                     drop0trailing=TRUE)
        return(substitute(X, list("X"=v)))
      } else {
        v <- round(m[i], digits_sci)
        return(substitute(M%*%10^N, list("M"=v, "N"=n[i])))
      }
    }))
  }

  s
}
