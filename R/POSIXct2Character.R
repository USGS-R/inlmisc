#' Convert from POSIXct to Character
#'
#' Convert objects from '\link{POSIXct}' class to '\link{character}' class.
#'
#' @param x 'POSIXct' vector.
#'   Calendar date and time
#' @param fmt 'character' string.
#'   Conversion specification format
#'
#' @return A 'character' vector representing time.
#'
#' @note R incorrectly formats objects of calss '\link{POSIXct}' with fractional seconds.
#' For example, a 'POSIXct' time with fractional part \code{.3} seconds (stored as \code{0.29999})
#' is printed as \code{.2} when represented with one decimal digit.
#' The fractional part on outputs is not rounded.
#' Decimal precision is down to milliseconds on Windows,
#' and down to (almost) microseconds on the other operating systems.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords manip
#'
#' @export
#'
#' @examples
#' txt <- c("11/10/2011 07:49:36.3",
#'          "04/01/2013 17:22:08.123",
#'          "01/06/2013 01:02:16.123",
#'          "12/14/2038 15:42:04.123456")
#' date.time <- as.POSIXct(txt, format = "%m/%d/%Y %H:%M:%OS")
#'
#' options("digits.secs" = 3)
#' format(date.time, fmt = "%d/%m/%Y %H:%M:%OS")
#' format(date.time, fmt = "%d/%m/%Y %H:%M:%OS3")
#'
#' POSIXct2Character(date.time, fmt = "%d/%m/%Y %H:%M:%OS3")
#' POSIXct2Character(date.time, fmt = "%d/%m/%Y %H:%M:%OS4")
#' POSIXct2Character(date.time, fmt = "%d/%m/%Y %H:%M:%OS2")
#'
#' POSIXct2Character(date.time, fmt = "%H:%M:%OS3 %Y-%m-%d")
#'

POSIXct2Character <- function(x, fmt="%Y-%m-%d %H:%M:%OS3") {

  checkmate::assertClass(x, "POSIXt")
  checkmate::assertString(fmt)

  # https://stackoverflow.com/questions/7726034
  # https://stackoverflow.com/questions/15383057

  pos <- gregexpr("%OS[[:digit:]]+", fmt)[[1]]
  if (pos > 0) {
    pos <- pos + c(3, attr(pos, "match.length"))
    dec.digits <- as.integer(substr(fmt, pos[1], pos[2]))
    x <- as.POSIXlt(x, tz=attr(x, "tzone"))
    x$sec <- round(x$sec, dec.digits) + 10^(-dec.digits - 1)
  }
  format(x, format=fmt)
}
