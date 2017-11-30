#' Print as LaTeX Table
#'
#' This function prints the LaTeX code associated with the supplied data frame.
#'
#' @param d 'data.frame'.
#'   Something.
#' @param colheadings 'character'.
#'   Something.
#' @param align 'character'.
#'   Something.
#' @param label 'character'.
#'   Something.
#' @param title 'character'.
#'   Something.
#' @param headnotes 'character'.
#'   Something.
#' @param footnotes 'character'.
#'   Something.
#' @param nrec 'integer'.
#'   Something.
#' @param na 'character'.
#'   Something.
#' @param rm_dup 'character' or 'integer'.
#'   Something.
#'
#' @details
#'   Requires \code{\\usepackage{caption}}, \code{\\usepackage{booktabs}},
#'   and \code{\\usepackage{makecell}} in the LaTeX preamble.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link[xtable]{xtable}}
#'
#' @keywords print
#'
#' @export
#'
#' @examples
#' d <- datasets::iris
#' d <- d[, c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
#' colheadings <- c("Species of \\\\ Iris",
#'                  "Sepal \\\\ length \\\\ (cm)", "Sepal \\\\ width \\\\ (cm)",
#'                  "Petal \\\\ length \\\\ (cm)", "Petal \\\\ width \\\\ (cm)")
#' align <- c("l", "r", "r", "r", "r")
#' title <- "Measurments of sepal length and width and petal length and width,
#'           for three species of Iris flower."
#' headnotes <- "\\textbf{Species of Iris}: inlcudes setosa, versicolor, and virginica.
#'               \\textbf{Abbreviations}: cm, centimeters"
#' PrintTable(d, colheadings, align, title = title, headnotes = headnotes,
#'            nrec = c(40L, 45L), rm_dup = 1L)
#'

PrintTable <- function(d, colheadings=NULL, align=NULL, label=NULL,
                       title=NULL, headnotes=NULL, footnotes=NULL,
                       nrec=nrow(d), na="--", rm_dup=NULL) {

  checkmate::assertDataFrame(d, min.rows=1, min.cols=1)
  checkmate::assertCharacter(colheadings, any.missing=FALSE, len=ncol(d), null.ok=TRUE)
  checkmate::assertCharacter(align, any.missing=FALSE, len=ncol(d), null.ok=TRUE)
  checkmate::assertString(label, null.ok=TRUE)
  checkmate::assertString(title, null.ok=TRUE)
  checkmate::assertString(headnotes, null.ok=TRUE)
  checkmate::assertString(footnotes, null.ok=TRUE)
  checkmate::assertIntegerish(nrec, lower=1, any.missing=FALSE, min.len=1, max.len=2)
  checkmate::assertString(na, null.ok=TRUE)
  checkmate::qassert(rm_dup, c("0", "S+", sprintf("X+(0,%d)", ncol(d))))

  if (!is.null(colheadings))
    colnames(d) <- sprintf("{\\normalfont\\bfseries\\sffamily \\makecell{%s}}",
                           colheadings)
  align <- c("r", align)

  cap1 <- strwrap(title, width=.Machine$integer.max)
  cap2 <- strwrap(headnotes, width=.Machine$integer.max)

  idxs <- which(unlist(lapply(d, is.factor)))
  d[, idxs] <- lapply(d[, idxs, drop=FALSE], as.character)

  n <- nrow(d)
  if (n > nrec[1]) {
    if (is.na(nrec[2])) nrec[2] <- nrec[1]
    n <- unique(c(cumsum(c(nrec[1], rep(nrec[2], (n - nrec[1]) %/% nrec[2]))), n))
  }

  for (i in seq_along(n)) {
    if (i == 2) cat("\\captionsetup[table]{list=no}\n")
    if (i == 1) {
      idxs <- 1:n[i]
      caption <- c(sprintf("%s\\par \\medskip [\\footnotesize{%s}]", cap1, cap2), cap1)
    } else {
      idxs <- (n[i - 1L] + 1L):n[i]
      caption <- sprintf("%s---Continued", cap1)
      label <- NULL
      cat("\\addtocounter{table}{-1}\n")
    }
    hline.after <- c(-1, 0, if (i == length(n)) length(idxs) else NULL)

    tbl <- d[idxs, ]
    for (id in rm_dup) tbl[[id]][duplicated(tbl[[id]])] <- ""
    tbl <- xtable::xtable(tbl)
    xtable::caption(tbl) <- caption
    xtable::label(tbl) <- label
    xtable::align(tbl) <- align
    print(tbl,
          include.rownames=FALSE,
          caption.placement="top",
          booktabs=TRUE,
          sanitize.colnames.function=function(x){x},
          size="\\small",
          sanitize.text.function=identity,
          hline.after=hline.after,
          NA.string=na)

    if (i > 1 && i == length(n)) cat("\\captionsetup[table]{list=yes}\n")
  }

  invisible(NULL)
}
