#' Deprecated Functions in the inlmisc Package
#'
#' Functions that are about to be removed from the \pkg{inlmisc} package.
#'
#' @param ...
#'   Arguments to be passed to the functions
#'   (obscured to enforce the usage of new functions).
#'
#' @rdname Deprecated
#' @keywords internal
#' @name inlmisc-deprecated
#'

NULL

#' @rdname Deprecated
#' @export

GetTolColors <- function(...) {
  .Deprecated("GetColors", package="inlmisc")
  GetColors(...)
}
