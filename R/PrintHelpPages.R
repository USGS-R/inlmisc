#' Print Package Help Pages in HTML Format
#'
#' Print the HTML code associated with help pages of one or more add-on packages.
#'
#' @param pkg 'character' vector.
#'   Package name(s)
#' @param file 'connection' or 'character' string.
#'   Names the file to append output to.
#'   Prints to the standard output connection by default.
#' @param internal 'logical' flag.
#'   Whether to print help topics flagged with the keyword \code{internal}.
#' @param toc 'logical' flag.
#'   Whether to format level-2 headers (help-topic titles) using a Markdown syntax,
#'   a requirement when specifying the table-of-contents (toc) format option in R Markdown,
#'   see \code{\link[rmarkdown]{render}} function for details.
#' @param hr 'logical' flag.
#'   Whether to add horizontal lines separating help topics.
#' @param links 'character' vector (experimental).
#'   Names of packages searched when creating internal hyperlinks to help topics.
#'
#' @return Invisible \code{NULL}
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
#'     "output:",
#'     "  html_document:",
#'     "    toc: true",
#'     "    toc_float: true",
#'     "---",
#'     sep = "\n", file = "help-example.Rmd")
#' PrintHelpPages("inlmisc", file = "help-example.Rmd", toc = TRUE)
#' rmarkdown::render("help-example.Rmd")
#' url <- file.path("file:/", getwd(), "help-example.html")
#' utils::browseURL(url)
#'
#' file.remove("help-example.Rmd", "help-example.html")
#' }
#'

PrintHelpPages <- function(pkg, file="", internal=FALSE, toc=FALSE, hr=TRUE,
                           links=NULL) {

  checkmate::assertCharacter(pkg, unique=TRUE)
  checkmate::assertFlag(internal)
  checkmate::assertFlag(toc)
  checkmate::assertFlag(hr)
  checkmate::assertCharacter(links, unique=TRUE, null.ok=TRUE)

  # get help-topic information
  info <- .GetHelpInfo(pkg)

  # parse contents of help files
  rd <- lapply(seq_len(nrow(info)), function(i) .GetHelpFile(info$file[i]))
  names(rd) <- info$name

  # get keywords
  info$keyword <- vapply(rd, function(x) {
    x <- as.character(x)
    idx <- which(x == "\\keyword")
    if (length(idx)) x[idx + 2L] else as.character(NA)
  }, "")

  # remove hidden help topics
  if (!internal) {
    is <- !(info$keyword %in% "internal")
    info <- info[is, , drop=FALSE]
    rd <- rd[is]
    names(rd) <- info$name
  }

  # identify links
  if (!is.null(links)) {
    d <- .GetHelpInfo(links)
    links <- paste0("#", d$name)
    names(links) <- d$name
  }

  # print horizontal seperator in markdown format
  if (hr) cat("\n---\n\n", file=file, append=TRUE)

  # loop through each of the help items
  for (i in seq_along(rd)) {

    # convert Rd to html
    htm <- utils::capture.output(tools::Rd2HTML(rd[[i]],
                                                no_links=is.null(links),
                                                Links=links,
                                                Links2=links))

    # convert level-2 header from html to markdown
    idx <- pmatch("<h2>", htm)
    if (toc)
      cat(sprintf("## %s", names(rd)[i]),
          sprintf("*%s*\n", gsub("<.*?>", "", htm[idx])),
          file=file, sep="\n\n", append=TRUE)

    # remove extraneous lines at the beginning and end of help page
    htm <- htm[-c(seq_len(idx - !toc), length(htm))]

    # edit code chunk tags for syntax highlighting
    htm_trim <- trimws(htm)
    htm[htm_trim == "</pre>"] <- "</code></pre>"
    idx <- which(htm_trim == "<pre>")
    htm[idx + 1L] <- sprintf("<pre class=\"lang-r\"><code class=\"lang-r\">%s",
                             htm[idx + 1L])
    htm[idx] <- ""

    # remove empty lines everywhere but in the examples section
    is <- nzchar(htm)
    if (!all(is)) {
      from <- grep("^<h3>Examples</h3>", htm)
      if (length(from) > 0) {
        to <- utils::tail(grep("</code></pre>" , htm), 1)
        idxs <- seq(from + 1L, to - 1L, by=1)
        lim <- range(which(nzchar(htm[idxs])))
        idxs <- idxs[lim[1]:lim[2]]
        is[idxs] <- TRUE
      }
      htm <- htm[is]
    }

    # encode images as a base64 string
    is <- grepl("<p><img src=\"", htm)
    if (any(is)) {
      src <- as.character(vapply(htm[is], function(x) {
        strsplit(x, "\"")[[1]][2]
      }, ""))
      src <- sub("..", system.file(package=info$package[i]), src)
      for (f in src) checkmate::assertFileExists(f, access="r")
      uri <- vapply(src, function(f) knitr::image_uri(f), "")
      htm[is] <- sprintf("<p><img src=\"%s\" alt=\"%s\" />",
                         uri, basename(src))
    }

    # add horizontal seperator
    if (hr) htm <- c(htm, "\n<hr>\n")

    # preserve html
    htm <- htmltools::htmlPreserve(c("", htm))

    # print help topic in html format
    cat(htm, "\n", file=file, fill=TRUE, append=TRUE)
  }

  invisible()
}


# get help-topic information
.GetHelpInfo <- function(pkg) {
  for (x in pkg) stopifnot(require(x, character.only=TRUE))
  l <- lapply(pkg, function(x) {
    paths <- tools::findHTMLlinks(pkgDir=system.file(package=x), level=0)
    unique(tools::file_path_sans_ext(basename(paths)))
  })
  names(l) <- pkg
  d <- data.frame("name"=do.call("c", l), stringsAsFactors=FALSE)
  d$package <- do.call("c", lapply(pkg, function(x) rep(x, length(l[[x]]))))
  d$file <- vapply(seq_len(nrow(d)), function(i) {
    x <- as.character(utils::help(d[i, 1], package=d[i, 2], help_type="html"))
    if (length(x) > 0) x else as.character(NA)
  }, "")
  d <- d[!is.na(d$file), , drop=FALSE]
  d <- d[order(d$name, d$package), ]
  rownames(d) <- NULL
  d
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
