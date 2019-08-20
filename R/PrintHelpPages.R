#' Print Package Help Pages in HTML Format
#'
#' Print the HTML code associated with help pages of a loaded R package.
#'
#' @param pkg 'character' string.
#'   Package name
#' @param file 'character' string.
#'   A connection, or a character string naming the file to print to.
#'   Prints to the standard output connection by default.
#' @param toc 'logical' flag.
#'   Whether to format the initial HTML header of each help page in Markdown.
#'   The table of contents (toc) option in R Markdown requires Markdown headers.
#' @param hr 'logical' flag.
#'   Whether to add a horizontal rule or line to separate help pages.
#' @param links 'character' vector.
#'   Names of packages searched when creating internal hyperlinks to help topics
#'   (an experimental feature).
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' \dontrun{
#' PrintHelpPages("inlmisc", file = "help-example.Rmd", toc = TRUE)
#' rmarkdown::render("help-example.Rmd")
#' utils::browseURL(sprintf("file://%s", file.path(getwd(), "help-example.html")))
#'
#' file.remove("help-example.Rmd", "help-example.html")
#' }
#'

PrintHelpPages <- function(pkg, file="", toc=FALSE, hr=TRUE, links=NULL) {

  checkmate::assertString(pkg)
  checkmate::assertFlag(toc)
  checkmate::assertFlag(hr)
  checkmate::assertCharacter(links, unique=TRUE, null.ok=TRUE)

  if (!paste0("package:", pkg) %in% search())
    stop("package needs to be loaded")

  if (!is.null(links)) {
    nm <- do.call("c", lapply(links, function(x) ls(paste0("package:", x))))
    links <- paste0("#", nm)
    names(links) <- nm
  }

  nm <- ls(paste0("package:", pkg))
  for (i in seq_along(nm)) {
    x <- .GetHelpFile(utils::help(nm[i], package=eval(pkg)))
    x <- utils::capture.output(tools::Rd2HTML(x, Links=links, Links2=links))

    # edit first header
    idx <- pmatch("<h2>", x)
    txt <- sprintf("## %s (%s) {#%s}\n\n",
                   gsub("<.*?>", "", x[idx]), nm[i], nm[i])
    if (toc) cat(txt, file=file, append=TRUE)

    # remove extraneous lines at beginning and end
    x <- x[-c(seq_len(idx - !toc), length(x))]

    # edit code chunk tags
    xtrim <- trimws(x)
    x[xtrim == "</pre>"] <- "</code></pre>"
    idx <- which(xtrim == "<pre>")
    x[idx + 1L] <- sprintf("<pre class=\"lang-r\"><code class=\"lang-r\">%s", x[idx + 1L])
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

    # add separator between help topics
    sep <- if (hr & i < length(nm)) "<hr />" else ""
    x <- c(x, sep)

    txt <- htmltools::htmlPreserve(x)
    cat(txt, "\n", file=file, sep="\n", fill=TRUE, append=TRUE)
  }

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
