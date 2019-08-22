#' Print Package Help Pages in HTML Format
#'
#' Print the HTML code associated with help pages of an add-on package.
#'
#' @param pkg 'character' string.
#'   Package name
#' @param file 'character' string.
#'   A connection, or a character string naming the file to append output to.
#'   Prints to the standard output connection by default.
#' @param toc 'logical' flag.
#'   Whether to format the title of each help topic as a level 1 header.
#'   The table of contents (toc) option in R Markdown requires Markdown headers.
#' @param hr 'logical' flag.
#'   Whether to add a horizontal rule or line to separate help pages.
#' @param links 'character' vector (experimental).
#'   Names of packages searched when creating internal hyperlinks to help topics.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cat("---",
#'     "title: Help Topics",
#'     "output:",
#'     "  html_document:",
#'     "    toc: true",
#'     "    toc_float: true",
#'     "---",
#'     "", sep = "\n", file = "help-example.Rmd")
#' PrintHelpPages("inlmisc", file = "help-example.Rmd", toc = TRUE)
#' rmarkdown::render("help-example.Rmd")
#' url <- file.path("file:/", getwd(), "help-example.html")
#' utils::browseURL(url)
#'
#' file.remove("help-example.Rmd", "help-example.html")
#' }
#'

PrintHelpPages <- function(pkg, file="", toc=FALSE, hr=TRUE, links=NULL) {

  checkmate::assertString(pkg)
  checkmate::assertFlag(toc)
  checkmate::assertFlag(hr)
  checkmate::assertCharacter(links, unique=TRUE, null.ok=TRUE)

  stopifnot(require(pkg, character.only=TRUE))

  if (!is.null(links)) {
    nm <- do.call("c", lapply(links, function(x) ls(paste0("package:", x))))
    links <- paste0("#", nm)
    names(links) <- nm
  }

  nm <- ls(paste0("package:", pkg))
  for (i in seq_along(nm)) {
    x <- .GetHelpFile(utils::help(nm[i], package=eval(pkg)))
    if (grepl("\\keyword\\{internal\\}", paste(as.character(x), collapse=""))) next
    x <- utils::capture.output(tools::Rd2HTML(x, Links=links, Links2=links))

    # print horizontal seperator
    if (hr) cat("<hr />", "\n", file=file, append=TRUE)

    # edit and print first header for table-of-contents
    idx <- pmatch("<h2>", x)
    if (toc)
      cat(sprintf("## %s", nm[i]),
          sprintf("*%s*\n", gsub("<.*?>", "", x[idx])),
          file=file, sep="\n\n", append=TRUE)

    # remove extraneous lines at beginning and end
    x <- x[-c(seq_len(idx - !toc), length(x))]

    # edit code chunk tags
    xtrim <- trimws(x)
    x[xtrim == "</pre>"] <- "</code></pre>"
    idx <- which(xtrim == "<pre>")
    x[idx + 1L] <- sprintf("<pre class=\"lang-r\"><code class=\"lang-r\">%s",
                           x[idx + 1L])
    x[idx] <- ""

    # remove empty lines everywhere but the examples section
    is <- nzchar(x)
    if (!all(is)) {
      from <- grep("^<h3>Examples</h3>", x)
      if (length(from) > 0) {
        to <- utils::tail(grep("</code></pre>" , x), 1)
        idxs <- seq(from + 1L, to - 1L, by=1)
        lim <- range(which(nzchar(x[idxs])))
        idxs <- idxs[lim[1]:lim[2]]
        is[idxs] <- TRUE
      }
      x <- x[is]
    }

    # encode images as a base64 string
    is <- grepl("<p><img src=\"", x)
    if (any(is)) {
      src <- as.character(vapply(x[is], function(y) strsplit(y, "\"")[[1]][2], ""))
      src <- sub("..", system.file(package=pkg), src)
      for (f in src) checkmate::assertFileExists(f, access="r")
      uri <- vapply(src, function(f) knitr::image_uri(f), "")
      x[is] <- sprintf("<p><img src=\"%s\" alt=\"%s\" />", uri, basename(src))
    }

    # preserve html
    txt <- htmltools::htmlPreserve(c("\n", x, "\n"))

    # print help topic
    cat(txt, "\n", file=file, fill=TRUE, append=TRUE)
  }

  # print horizontal seperator
  if (hr) cat("<hr />", "\n", file=file, append=TRUE)

  invisible()
}

# copied from utils:::.getHelpFile to avoid CRAN warning, accessed on 2019-07-03
.GetHelpFile <- function(file) {
  path <- dirname(file)
  dirpath <- dirname(path)
  if (!file.exists(dirpath))
    stop(gettextf("invalid %s argument", sQuote("file")), domain=NA)
  pkgname <- basename(dirpath)
  rddb <- file.path(path, pkgname)
  if (!file.exists(paste0(rddb, ".rdx")))
    stop(gettextf("package %s exists but was not installed under R >= 2.10.0 so help cannot be accessed",
                  sQuote(pkgname)), domain=NA)
  .FetchRdDB(rddb, basename(file))
}

# copied from tools:::fetchRdDB to avoid CRAN warning, accessed on 2019-07-03
.FetchRdDB <- function(filebase, key=NULL) {
  FUN <- function(db) {
    vals       <- db$vals
    vars       <- db$vars
    datafile   <- db$datafile
    compressed <- db$compressed
    envhook    <- db$envhook
    Fetch <- function(key) {
      lazyLoadDBfetch(vals[key][[1]], datafile, compressed, envhook)
    }
    if (length(key)) {
      if (!key %in% vars)
        stop(gettextf("No help on %s found in RdDB %s",
                      sQuote(key), sQuote(filebase)), domain=NA)
      Fetch(key)
    }
    else {
      res <- lapply(vars, Fetch)
      names(res) <- vars
      res
    }
  }
  res <- lazyLoadDBexec(filebase, FUN)
  if (length(key)) res else invisible(res)
}
