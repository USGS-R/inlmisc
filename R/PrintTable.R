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
#' @param digits 'integer'.
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
#' @param hline 'integer'.
#'   Something.
#' @param na 'character'.
#'   Something.
#' @param rm_dup 'integer'.
#'   Something.
#' @param landscape 'logical'.
#'   Something.
#'   This option requires \code{\\usepackage[pdftex]{lscape}} in the LaTeX preamble.
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

PrintTable <- function(d, colheadings=NULL, align=NULL, digits=NULL, label=NULL,
                       title=NULL, headnotes=NULL, footnotes=NULL,
                       nrec=nrow(d), hline=NULL, na="--",
                       rm_dup=NULL, landscape=FALSE) {

  checkmate::assertDataFrame(d, min.rows=1, min.cols=1)
  checkmate::assertCharacter(colheadings, any.missing=FALSE, len=ncol(d), null.ok=TRUE)
  checkmate::assertCharacter(align, any.missing=FALSE, len=ncol(d), null.ok=TRUE)
  checkmate::assertIntegerish(digits, any.missing=FALSE, len=ncol(d), null.ok=TRUE)
  checkmate::assertString(label, null.ok=TRUE)
  checkmate::assertString(title, null.ok=TRUE)
  checkmate::assertString(headnotes, null.ok=TRUE)
  checkmate::assertString(footnotes, null.ok=TRUE)
  checkmate::assertIntegerish(nrec, lower=1, any.missing=FALSE, min.len=1, max.len=2)
  checkmate::assertIntegerish(hline, lower=1, upper=nrow(d) - 1L, any.missing=FALSE, null.ok=TRUE)
  checkmate::assertString(na, null.ok=TRUE)
  checkmate::assertInt(rm_dup, lower=1, upper=ncol(d), null.ok=TRUE)
  checkmate::assertFlag(landscape)

  if (!is.null(colheadings))
    colnames(d) <- sprintf("{\\normalfont\\bfseries\\sffamily \\makecell{%s}}",
                           colheadings)

  cap1 <- strwrap(title, width=.Machine$integer.max)
  cap2 <- strwrap(headnotes, width=.Machine$integer.max)

  idxs <- which(unlist(lapply(d, is.factor)))
  d[, idxs] <- lapply(d[, idxs, drop=FALSE], as.character)

  n <- nrow(d)
  if (n > nrec[1]) {
    if (length(nrec) == 1) nrec[2] <- nrec[1]
    n <- unique(c(cumsum(c(nrec[1], rep(nrec[2], (n - nrec[1]) %/% nrec[2]))), n))
  }

  if (landscape) {
    cat("\\begin{landscape}\n")
    on.exit(cat("\\end{landscape}\n"))
  }

  for (i in seq_along(n)) {
    if (i == 2) cat("\\captionsetup[table]{list=no}\n")
    if (i == 1){
      idxs <- 1:n[i]
      caption <- c(sprintf("%s\\par \\medskip [\\footnotesize{%s}]", cap1, cap2), cap1)
    } else {
      idxs <- (n[i - 1L] + 1L):n[i]
      caption <- sprintf("%s---Continued", cap1)
      label <- NULL
      cat("\\addtocounter{table}{-1}\n")
    }

    tbl <- d[idxs, ]
    if (!is.null(rm_dup))
      for (j in rev(seq_len(rm_dup)))
        tbl[[j]][duplicated(tbl[, seq_len(j), drop=FALSE])] <- ""

    tbl <- xtable::xtable(tbl)
    xtable::caption(tbl) <- caption
    if (length(caption) > 0) xtable::label(tbl) <- label
    if (!is.null(align)) xtable::align(tbl) <- c("l", align)
    if (!is.null(digits)) xtable::digits(tbl) <- c(0L, digits)
    add.to.row <- NULL

    hline.after <- sort(unique(stats::na.omit(c(-1, 0, match(c(hline, nrow(d)), idxs)))))

    if (!is.null(footnotes) && i == length(n)) {
      fmt <- "\\midrule\n\\multicolumn{%s}{l}{\\footnotesize{%s}}\\\\"
      cmd <- sprintf(fmt, ncol(tbl), footnotes)
      add.to.row <- list(pos=list(nrow(tbl)), command=cmd)
      hline.after <- head(hline.after, -1)
    }

    print(tbl,
          include.rownames=FALSE,
          caption.placement="top",
          booktabs=TRUE,
          sanitize.colnames.function=function(x){x},
          size="\\small",
          sanitize.text.function=identity,
          add.to.row=add.to.row,
          hline.after=hline.after,
          NA.string=na)

    if (i > 1 && i == length(n)) cat("\\captionsetup[table]{list=yes}\n")
  }

  invisible(NULL)
}
