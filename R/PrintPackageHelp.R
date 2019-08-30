#' Print Package Help Documentation
#'
#' Print the HTML code associated with the help documentation of one or more add-on packages.
#'
#' @param pkg 'character' vector.
#'   Package name(s)
#' @param file 'connection' or 'character' string.
#'   Names the file to append output to.
#'   Prints to the standard output connection by default.
#' @param internal 'logical' flag.
#'   Whether to print help topics flagged with the keyword "internal".
#' @param toc 'logical' flag.
#'   Whether to format level-2 headers (help-topic titles) using a Markdown syntax.
#'   This is required when specifying the table-of-contents (toc) format option in R Markdown,
#'   see \code{\link[rmarkdown:render]{rmarkdown::render}} function for details.
#' @param replace_title 'logical' flag.
#'   Whether to replace the help-topic \dQuote{title} with its \dQuote{name}.
#' @param sep 'character' string.
#'   HTML to separate help topics, a horizontal line by default.
#' @param links 'character' vector (experimental).
#'   Names of packages searched when creating internal hyperlinks to help topics.
#' @param ...
#'   Not used
#'
#' @return Invisible \code{NULL}
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords documentation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cat("---",
#'     "title: \"Help Documentation\"",
#'     "output:",
#'     "  html_document:",
#'     "    toc: true",
#'     "    toc_float: true",
#'     "---",
#'     sep = "\n", file = "help-example.Rmd")
#' PrintPackageHelp("inlmisc", file = "help-example.Rmd",
#'                  toc = TRUE, replace_title = TRUE)
#' rmarkdown::render("help-example.Rmd")
#' url <- file.path("file:/", getwd(), "help-example.html")
#' utils::browseURL(url)
#'
#' file.remove("help-example.Rmd", "help-example.html")
#' }
#'

PrintPackageHelp <- function(pkg, file="", internal=FALSE,
                             toc=FALSE, replace_title=FALSE,
                             sep="<hr>", links=NULL, ...) {

  checkmate::assertCharacter(pkg, unique=TRUE)
  checkmate::assertFlag(internal)
  checkmate::assertFlag(toc)
  checkmate::assertFlag(replace_title)
  checkmate::assertString(sep, null.ok=TRUE)
  checkmate::assertCharacter(links, unique=TRUE, null.ok=TRUE)

  # get metadata for help topics
  meta <- .GetHelpMeta(pkg)

  # parse contents of help files
  rd <- lapply(seq_len(nrow(meta)), function(i) {
    .GetHelpFile(meta$file[i])
  })
  names(rd) <- meta$name

  # get keywords
  meta$keyword <- vapply(rd, function(x) {
    x <- as.character(x)
    idx <- which(x == "\\keyword")
    if (length(idx)) x[idx + 2L] else as.character(NA)
  }, "")

  # remove hidden help topics
  if (!internal) {
    is <- !(meta$keyword %in% "internal")
    meta <- meta[is, , drop=FALSE]
    rd <- rd[is]
    names(rd) <- meta$name
  }

  # identify links
  if (!is.null(links)) {
    d <- .GetHelpMeta(links)
    links <- paste0("#", d$name)
    names(links) <- d$name
  }

  # loop through help items
  for (i in seq_along(rd)) {

    # convert rd to html
    htm <- utils::capture.output(tools::Rd2HTML(rd[[i]],
                                                no_links=is.null(links),
                                                Links=links,
                                                Links2=links))

    # update level-2 header (title of help documentation)
    idx <- pmatch("<h2>", htm)
    ti <- gsub("<.*?>", "", htm[idx])
    nm <- names(rd)[i]
    if (toc) {
      if (replace_title)
        txt <- sprintf("\n## %s\n\n*%s*", nm, ti)
      else
        txt <- sprintf("\n## %s {#%s}", ti, nm)
      cat(txt, file=file, sep="\n\n", append=TRUE)
    } else if (replace_title) {
      htm[idx] <- sprintf("<h2>%s</h2>\n\n<em>%s</em>\n", nm, ti)
    }

    # remove extraneous lines at beginning and end of help page
    htm <- htm[-c(seq_len(idx - !toc), length(htm))]

    # edit code-chunk tags to use syntax highlighting
    htm_trim <- trimws(htm)
    htm[htm_trim == "</pre>"] <- "</code></pre>"
    idx <- which(htm_trim == "<pre>")
    htm[idx + 1L] <- sprintf("<pre class=\"lang-r\"><code class=\"lang-r\">%s",
                             htm[idx + 1L])
    htm[idx] <- ""

    # remove empty lines everywhere but in examples section
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

    # encode images as base64 strings
    is <- grepl("<p><img src=\"", htm)
    if (any(is)) {
      src <- as.character(vapply(htm[is], function(x) {
        strsplit(x, "\"")[[1]][2]
      }, ""))
      src <- sub("..", system.file(package=meta$package[i]), src)
      for (f in src) checkmate::assertFileExists(f, access="r")
      uri <- vapply(src, function(f) knitr::image_uri(f), "")
      htm[is] <- sprintf("<p><img src=\"%s\" alt=\"%s\" />", uri, basename(src))
    }

    # add seperator
    if (!is.null(sep) && i < nrow(meta)) htm <- c(htm, sprintf("\n%s\n", sep))

    # preserve html
    htm <- htmltools::htmlPreserve(htm)

    # print help documentation
    cat("", htm, file=file, sep="\n", fill=TRUE, append=TRUE)
  }

  invisible()
}


# get help-topic metadata
.GetHelpMeta <- function(pkg) {
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
