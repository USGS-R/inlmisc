#' Get Number of Days in a Year and Month
#'
#' Calculate the number of days in a year and month.
#'
#' @param x 'integer' vector.
#'   Year and month, with a required date format of \code{YYYYMM}.
#'
#' @return A 'integer' vector indicating the number of days
#'   for each year and month value in \code{x}.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' GetDaysInMonth(c("199802", "199804", "200412"))
#'

GetDaysInMonth <- function(x) {
  checkmate::assertCharacter(x, pattern="^\\d{6}$", any.missing=FALSE)
  d <- as.Date(paste0(x, "28"), format="%Y%m%d")
  m <- format(d, format="%m")
  for (i in seq_along(d)) {
    while (format(d[i], format="%m") == m[i]) {
      d[i] <- d[i] + 1
    }
  }
  as.integer(format(d - 1, format="%d"))
}
