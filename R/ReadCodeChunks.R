#' Read Knitr Code Chunks
#'
#' Read \bold{knitr} code chunks into the current session.
#'
#' @param path 'character' string.
#'   Path name of the \bold{knitr} source document (\file{.Rnw} or \file{.Rmd}),
#'   or R code that has been extracted from a \bold{knitr} source document (\file{.R}).
#'
#' @details If the source document is \file{.Rnw} or \file{.Rmd} the \code{purl} function is used to extract the R code.
#'   The R code is read into the current session using a chunk separator of the from \code{## ---- chunk-name}
#'   (at least four dashes before the chunk name) in the script.
#'   Unnamed chunks (that is, \code{chunk-name} is missing) will be assigned
#'   names like \code{unnamed-chunk-i} where \code{i} is the chunk number.
#'
#' @return A 'list' of length equal to the number of code chunks in \code{path}.
#'   Each list component is named after its corresponding chunk name (\code{chunk-name}).
#'   The returned object includes the value of the \code{path} argument as an attribute.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' file <- system.file("misc/knitr-markdown.Rmd",
#'                     package = "inlmisc")
#' chunks <- ReadCodeChunks(file)
#' print(chunks)
#' attr(chunks, "path")
#'
#' txt <- chunks[c("unnamed-chunk-3", "named-chunk-4")]
#' eval(parse(text = unlist(txt)))
#'

ReadCodeChunks <- function(path) {

  checkmate::assertFileExists(path, extension=c("rnw", "rmd", "r"))

  ext <- tools::file_ext(path)
  src <- readLines(path)

  # extract r code from knitr source documents
  if (tolower(ext) %in% c("rnw", "rmd"))
    src <- strsplit(knitr::purl(text=src, quiet=TRUE), "\n")[[1]]

  lin <- grep("## ----", src)  # chunk header lines
  nam <- gsub("^## ---+(-)+| |\\,.*|+(-)+$", "", src[lin])  # chunk names

  # account for missing names
  is.unnamed <- which(grepl("=", nam) | nam == "")
  nam[is.unnamed] <- paste0("unnamed-chunk-", is.unnamed)

  # extract code chunks
  m <- cbind(from = lin, to = c(lin[-1] - 1, length(src)))
  chunks <- apply(m, 1, function(x) src[x[1]:x[2]])

  # remove trailing blank lines
  chunks <- lapply(chunks, function(x) x[1:max(which(x != ""))])

  names(chunks) <- nam  # chunk names
  attr(chunks, "path") <- path  # file path attribute
  chunks
}
