#' Print as LaTeX Table
#'
#' This function prints the LaTeX code associated with the supplied data table.
#' The applied output format attempts to adhere to the design recommendations
#' for tables in United States Geological Survey (USGS) publications.
#'
#' @param d 'data.frame'.
#'   Data table to print (row names are excluded).
#' @param colheadings 'character'.
#'   Vector of length equal to the number of columns in the table, indicating column headings.
#' @param align 'character'.
#'   Vector of length equal to the number of columns in the table,
#'   indicating the alignment of the corresponding columns.
#'   Use \code{"l"}, \code{"r"}, and \code{"c"} to denote left, right,
#'   and center alignment, respectively.
#' @param digits 'integer'.
#'   Vector of length equal to the number of columns in the table,
#'   indicating the number of digits to display in the corresponding columns.
#' @param label 'character'.
#'   String containing the LaTeX label anchor.
#'   Specifying this argument allows you to easily reference the table within the LaTeX document.
#'   For example, when \code{label = "id"}, use \code{\\ref\{id\}}
#'   to reference the table within a sentence.
#' @param title 'character'.
#'   String containing the table caption.
#' @param headnotes 'character'.
#'   String placed below the table caption to provide information pertaining to the caption,
#'   to the table as a whole, or to the column headings.
#' @param footnotes 'character'.
#'   String placed at the end of the table to provide explanations of individual entries in the table.
#' @param nrec 'integer'.
#'   Vector of length equal to 2, indicating the maximum number of records to show on the first page,
#'   and every subsequent page, respectively.
#'   Value is recycled as necessary.
#' @param hline 'integer'.
#'   Vector of numbers between 1 and \code{nrow(d) - 1}, indicating the table rows after which
#'   a horizontal line should appear.
#' @param na 'character'.
#'   String to be used for missing values in table entries.
#' @param include.rownames 'logical'.
#'   Whether to print row names.
#' @param rm_dup 'integer'.
#'   End value of a sequence of column indexes \code{(1:rm_dup)}.
#'   Duplicate values contained in these columns will be set equal to an empty string.
#'   Where duplicates in a column are determined from the 'character' vector formed by
#'   combining its content with the content from all previous columns in the table.
#' @param landscape 'logical'.
#'   If true, conforming PDF viewers will display the table in landscape orientation.
#'   This option requires \code{\\usepackage[pdftex]{lscape}} in the LaTeX preamble.
#' @param ...
#'   Additional arguments to be passed to the \code{\link[xtable]{print.xtable}} function.
#'   Use with care, many of its arguments are already included.
#'
#' @details
#'   Requires \code{\\usepackage{caption}}, \code{\\usepackage{booktabs}}, and
#'   \code{\\usepackage{makecell}} in the LaTeX preamble.
#'
#' @return Invisible \code{NULL}
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
#' d <- datasets::iris[, c(5, 1:4)]
#' colheadings <- c("Species of Iris",
#'                  "Sepal length \\\\ (cm)", "Sepal width \\\\ (cm)",
#'                  "Petal length \\\\ (cm)", "Petal width \\\\ (cm)")
#' align <- c("l", "c", "c", "c", "c")
#' digits <- c(0, 1, 1, 1, 1)
#' title <- "Measurements of sepal length and width and petal length and width,
#'           for three species of Iris flower."
#' headnotes <- "\\textbf{Species of Iris}: includes setosa, versicolor, and virginica.
#'               \\textbf{Abbreviations}: cm, centimeters"
#' levels(d[[1]]) <- sprintf("%s\\footnotemark[%d]", levels(d[[1]]), 1:3)
#' footnotes <- paste(sprintf("\\footnotemark[%d] Common name is %s iris.", 1:3,
#'                            c("Wild Flag", "Blue Flag", "Virginia")), collapse = "\\\\")
#' hline <- utils::tail(which(!duplicated(d[[1]])), -1) - 1L
#' PrintTable(d, colheadings, align, digits, title = title, headnotes = headnotes,
#'            footnotes = footnotes, hline = hline, nrec = c(41, 42), rm_dup = 1)
#'
#' \dontrun{
#' sink("table-example.tex")
#' cat("\\documentclass{article}",
#'     "\\usepackage[labelsep=period,labelfont=bf]{caption}",
#'     "\\usepackage{booktabs}",
#'     "\\usepackage{makecell}",
#'     "\\usepackage[pdftex]{lscape}",
#'     "\\makeatletter",
#'     "\\setlength{\\@fptop}{0pt}",
#'     "\\makeatother",
#'     "\\begin{document}", sep = "\n")
#' PrintTable(d, colheadings, align, digits, title = title, headnotes = headnotes,
#'            footnotes = footnotes, hline = hline, nrec = c(41, 42), rm_dup = 1)
#' cat("\\clearpage\n")
#' PrintTable(datasets::CO2[, c(2, 3, 1, 4, 5)], digits = c(0, 0, 0, 0, 1),
#'            title = "Carbon dioxide uptake in grass plants.", nrec = 45, rm_dup = 3)
#' cat("\\clearpage\n")
#' PrintTable(datasets::mtcars, title = "Motor trend car road tests.",
#'            include.rownames = TRUE, landscape = TRUE)
#' cat("\\end{document}\n")
#' sink()
#' tools::texi2pdf("table-example.tex", clean = TRUE)  # requires TeX installation
#' system("open table-example.pdf")
#'
#' file.remove("table-example.tex", "table-example.pdf")
#' }
#'

PrintTable <- function(d, colheadings=NULL, align=NULL, digits=NULL, label=NULL,
                       title=NULL, headnotes=NULL, footnotes=NULL, nrec=nrow(d),
                       hline=NULL, na="--", include.rownames=FALSE,
                       rm_dup=NULL, landscape=FALSE, ...) {

  checkmate::assertDataFrame(d, min.rows=1, min.cols=1)
  checkmate::assertCharacter(colheadings, any.missing=FALSE, len=ncol(d), null.ok=TRUE)
  checkmate::assertCharacter(align, any.missing=FALSE, len=ncol(d), null.ok=TRUE)
  checkmate::assertIntegerish(digits, any.missing=FALSE, len=ncol(d), null.ok=TRUE)
  checkmate::assertString(label, null.ok=TRUE)
  checkmate::assertString(title, null.ok=TRUE)
  checkmate::assertString(headnotes, null.ok=TRUE)
  checkmate::assertString(footnotes, null.ok=TRUE)
  checkmate::assertIntegerish(nrec, lower=1, any.missing=FALSE, min.len=1, max.len=2)
  checkmate::assertIntegerish(hline, lower=1, upper=nrow(d) - 1, any.missing=FALSE, null.ok=TRUE)
  checkmate::assertString(na, null.ok=TRUE)
  checkmate::assertFlag(include.rownames)
  checkmate::assertInt(rm_dup, lower=1, upper=ncol(d), null.ok=TRUE)
  checkmate::assertFlag(landscape)

  if (is.null(colheadings)) colheadings <- colnames(d)
  colnames(d) <- sprintf("{\\normalfont\\bfseries\\sffamily \\makecell{%s}}", colheadings)

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

    row_names <- utils::type.convert(rownames(d))
    row_align <- ifelse(is.numeric(row_names), "r", "l")
    row_digits <- ifelse(is.double(row_names), format.info(row_names)[2], 0L)
    if (!is.null(align)) xtable::align(tbl) <- c(row_align, align)
    if (!is.null(digits)) xtable::digits(tbl) <- c(row_digits, digits)

    add.to.row <- NULL
    hline.after <- sort(unique(stats::na.omit(c(-1L, 0L, match(c(hline, nrow(d)), idxs)))))
    if (!is.null(footnotes) && i == length(n)) {
      fmt <- "\\midrule\n\\multicolumn{%s}{l}{\\makecell[l]{%s}}\\\\"
      cmd <- sprintf(fmt, ncol(tbl), footnotes)
      add.to.row <- list(pos=list(nrow(tbl)), command=cmd)
      hline.after <- utils::head(hline.after, -1)
    }

    print(x=tbl,
          type="latex",
          caption.placement="top",
          size="\\small",
          hline.after=hline.after,
          NA.string=na,
          include.rownames=include.rownames,
          add.to.row=add.to.row,
          sanitize.text.function=identity,
          sanitize.colnames.function=function(x){x},
          math.style.exponents=TRUE,
          format.args=list(big.mark=","),
          booktabs=TRUE,
          comment=FALSE,
          ...)

    if (i > 1 && i == length(n)) cat("\\captionsetup[table]{list=yes}\n")
  }

  invisible(NULL)
}
