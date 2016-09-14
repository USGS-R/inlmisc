#' Number of Days in a Year and Month
#'
#' This function determines the number of days in a year and month.
#'
#' @param x integer.
#'   Vector of year and month values, with a required date format of \code{YYYYMM}.
#'
#' @return Returns an integer vector indicating the number of days
#'   in each year and month value specified in \code{x}.
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
  d <- as.Date(paste0(x, "28"), format="%Y%m%d")
  m <- format(d, format="%m")
  for (i in seq_along(d)) {
    while (format(d[i], format="%m") == m[i]) {
      d[i] <- d[i] + 1L
    }
  }
  return(as.integer(format(d - 1L, format="%d")))
}
