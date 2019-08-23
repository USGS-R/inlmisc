#' Format P Values
#'
#' Format \emph{p}-values for pretty printing.
#'
#' @param x 'numeric' vector.
#'   \emph{p}-values
#' @param digits 'integer' count.
#'   Number of significant digits to be used.
#' @param eps 'numeric' number.
#'   Numerical tolerance,
#'   values less than \code{eps} are formatted as \code{"< [eps]"}.
#' @param na.form 'character' string.
#'   Value used for missing values.
#' @param scientific 'logical' flag.
#'   Whether values should be encoded in scientific format using LaTeX notation.
#'   A missing value lets \R decide whether fixed or scientific notation is used.
#'
#' @return A 'character' vector of formatted \emph{p}-values.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{ToScientific}}
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' x <- c(stats::runif(5), pi^-100, NA)
#' FormatPval(x)
#' format.pval(x)
#'
#' x <- c(0.1, 0.0001, 1e-27)
#' FormatPval(x, scientific = TRUE)
#' FormatPval(x, digits = 3, eps = 0.001)
#'

FormatPval <- function(x, digits=max(1, getOption("digits") - 2),
                       eps=.Machine$double.eps, na.form="NA", scientific=NA) {

  checkmate::assertNumeric(x)
  checkmate::assertCount(digits, positive=TRUE, null.ok=TRUE)
  checkmate::assertNumber(eps)
  checkmate::assertString(na.form)
  checkmate::assertFlag(scientific, na.ok=TRUE)

  p <- format(round(x, digits), nsmall=digits, scientific=FALSE)

  is <- if (is.na(scientific)) grepl("e", formatC(x)) else rep(scientific, length(x))
  p[is] <- ToScientific(x[is], digits=0)

  lim <- ifelse(grepl("e", formatC(eps)), ToScientific(eps, digits=0), format(eps))
  p[x < eps] <- sprintf("< %s", lim)

  p[is.na(x)] <- as.character(na.form)
  p
}
