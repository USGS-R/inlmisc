#' Build Package Vignettes
#'
#' Build package vignettes from their sources.
#'
#' @param pkg 'character' string.
#'   Package path, defaults to the working directory.
#' @param quiet 'logical' flag.
#'   Whether to supress most output.
#' @param gs_quality 'character' string.
#'   Quailty to use when compacting PDF files,
#'   see \code{\link[tools]{compactPDF}} function for details.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link[tools]{buildVignettes}}
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' \dontrun{
#' BuildVignettes("<path/to/package>", gs_quality = "ebook")
#' }
#'

BuildVignettes <- function(pkg=".", quiet=TRUE, gs_quality=NULL) {

  checkmate::assertFileExists(file.path(pkg, "DESCRIPTION"))
  checkmate::assertFlag(quiet)
  if (!is.null(gs_quality))
    gs_quality <- match.arg(gs_quality, c("none", "printer", "ebook", "screen"))

  tools::buildVignettes(dir=pkg, quiet=quiet, tangle=TRUE)

  v <- tools::pkgVignettes(dir=pkg, output=TRUE, source=TRUE)
  if (length(v) == 0) return(invisible(NULL))
  out <- c(unique(unlist(v$sources, use.names=FALSE)), v$outputs)

  doc <- file.path(pkg, "inst/doc")

  dir.create(doc, showWarnings=FALSE, recursive=TRUE)
  file.copy(c(v$docs, out), doc, overwrite=TRUE)
  file.remove(out)

  if (!is.null(gs_quality))
    tools::compactPDF(paths=doc, gs_quality=gs_quality)

  invisible(TRUE)
}
