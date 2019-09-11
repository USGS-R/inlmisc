#' Print as LaTeX Table
#'
#' Print the LaTeX code associated with the supplied data table.
#' The applied output format attempts to adhere to the design recommendations
#' for tables in United States Geological Survey (USGS) publications.
#'
#' @param d 'data.frame' or 'matrix'.
#'   Data table to print.
#' @param colheadings 'character' vector, 'matrix', or 'data.frame'.
#'   Column headings.
#'   For table objects, rows represent layers of headings (stacked headings).
#'   The number of columns (or vector length) must equal the number of columns in argument \code{d}.
#'   A column heading can span multiple columns by repeating adjacent headings.
#'   Use \code{\\\\\\\\} to code a line break.
#' @param align 'character' vector.
#'   Column alignment.
#'   Specify \code{"l"} to left align, \code{"r"} to right align, \code{"c"} to center align,
#'   and \code{"S"} to align on the decimal point.
#' @param digits 'integer' vector.
#'   Number of digits to display in the corresponding columns.
#' @param label 'character' string.
#'   LaTeX label anchor.
#'   Specifying this argument allows you to easily reference the table within the LaTeX document.
#'   For example, when \code{label = "id"}, use \code{\\ref\{id\}}
#'   to reference the table within a sentence.
#' @param title 'character' string.
#'   Table caption
#' @param headnotes 'character' string.
#'   Label placed below the table caption to provide information pertaining to the caption,
#'   to the table as a whole, or to the column headings.
#' @param footnotes 'character' string.
#'   Label placed at the end of the table to provide explanations
#'   of individual entries in the table.
#' @param nrec 'integer' vector of length 1 or 2, value is recycled as necessary.
#'   Maximum number of records to show on the first page, and every subsequent page, respectively.
#' @param hline 'integer' vector.
#'   Numbers between 1 and \code{nrow(d) - 1} indicating the table rows after which
#'   a horizontal line should appear.
#' @param na 'character' string.
#'   Value to be used for missing values in table entries.
#' @param rm_dup 'integer' count.
#'   End value of a sequence of column indexes \code{(1:rm_dup)}.
#'   Duplicate values contained in these columns will be set equal to an empty string.
#'   Where duplicates in a column are determined from the 'character' vector formed by
#'   combining its content with the content from all previous columns in the table.
#' @param landscape 'logical' flag.
#'   If true, conforming PDF viewers will display the table in landscape orientation.
#'   This option requires \code{\\usepackage[pdftex]{lscape}} in the LaTeX preamble.
#' @param ...
#'   Additional arguments to be passed to the \code{\link[xtable]{print.xtable}} function.
#'   The arguments \code{type}, \code{hline.after} and \code{add.to.row} should not be included.
#'
#' @details
#'   Requires \code{\\usepackage{caption}}, \code{\\usepackage{booktabs}},
#'   \code{\\usepackage{makecell}}, \code{\\usepackage{multirow}}, and
#'   \code{\\usepackage{siunitx}} in the LaTeX preamble.
#'
#' @return Invisible \code{NULL}
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords print
#'
#' @export
#'
#' @examples
#' d <- datasets::iris[, c(5, 1:4)]
#' colheadings <- rbind(c("Species \\\\ type", rep("Sepal", 2), rep("Petal", 2)),
#'                      c("", rep(c("Length", "Width"), 2)))
#' align <- c("l", "c", "c", "c", "c")
#' digits <- c(0, 1, 1, 1, 1)
#' title <- "Measurements of sepal length and width and petal
#'           length and width for three species of Iris flower."
#' headnotes <- "\\textbf{Species of Iris}: includes setosa, versicolor, and virginica.
#'               \\textbf{Abbreviations}: cm, centimeters"
#' levels(d[[1]]) <- sprintf("%s\\footnotemark[%d]", levels(d[[1]]), 1:3)
#' footnotes <- paste(sprintf("\\footnotemark[%d] Common name is %s iris.", 1:3,
#'                            c("Wild Flag", "Blue Flag", "Virginia")),
#'                    collapse = "\\\\")
#' hline <- utils::tail(which(!duplicated(d[[1]])), -1) - 1L
#' PrintTable(d, colheadings, align, digits, title = title,
#'            headnotes = headnotes, footnotes = footnotes,
#'            hline = hline, nrec = c(41, 42), rm_dup = 1)
#'
#' \dontrun{
#' sink("table-example.tex")
#' cat("\\documentclass{article}",
#'     "\\usepackage{geometry}",
#'     "\\usepackage[labelsep = period, labelfont = bf]{caption}",
#'     "\\usepackage{siunitx}",
#'     "\\sisetup{input-ignore = {,}, input-decimal-markers = {.},",
#'     "          group-separator = {,}, group-minimum-digits = 4}",
#'     "\\usepackage{booktabs}",
#'     "\\usepackage{makecell}",
#'     "\\usepackage{multirow}",
#'     "\\usepackage[pdftex]{lscape}",
#'     "\\makeatletter",
#'     "\\setlength{\\@fptop}{0pt}",
#'     "\\makeatother",
#'     "\\begin{document}", sep = "\n")
#' PrintTable(d, colheadings, align, digits, title = title,
#'            headnotes = headnotes, footnotes = footnotes,
#'            hline = hline, nrec = c(41, 42), rm_dup = 1)
#' cat("\\clearpage\n")
#' PrintTable(datasets::CO2[, c(2, 3, 1, 4, 5)],
#'            digits = c(0, 0, 0, 0, 1),
#'            title = "Carbon dioxide uptake in grass plants.",
#'            nrec = 45, rm_dup = 3)
#' cat("\\clearpage\n")
#' digits <- c(1, 0, 1, 0, 2, 3, 2, 0, 0, 0, 0)
#' PrintTable(datasets::mtcars, digits = digits,
#'            title = "Motor trend car road tests.",
#'            landscape = TRUE, include.rownames = TRUE)
#' cat("\\clearpage\n")
#' x <- c(1.2, 1.23, 1121.2, 184, NA, pi, 0.4)
#' d <- data.frame(matrix(rep(x, 4), ncol = 4))
#' d[, 1] <- prettyNum(d[, 1])
#' d[, 4] <- formatC(d[, 4], digits = 2, format = "e")
#' colheadings <- paste("Wide heading", 1:ncol(d))
#' align <- c("S", "S",
#'            "S[round-mode = places, round-precision = 2]",
#'            "S[scientific-notation = true, table-format = 1.2e+1]")
#' PrintTable(d, colheadings, align)
#' cat("\\end{document}\n")
#' sink()
#' tinytex::pdflatex("table-example.tex")  # requires TeX
#' system("open table-example.pdf")
#'
#' file.remove("table-example.tex", "table-example.pdf")
#' }
#'

PrintTable <- function(d, colheadings=NULL, align=NULL, digits=NULL, label=NULL,
                       title=NULL, headnotes=NULL, footnotes=NULL, nrec=nrow(d),
                       hline=NULL, na="\\textemdash", rm_dup=NULL, landscape=FALSE,
                       ...) {

  stopifnot(inherits(d, c("matrix", "data.frame")))
  d <- as.data.frame(d, stringsAsFactors=FALSE)
  checkmate::assertDataFrame(d, min.rows=1, min.cols=1)

  if (inherits(colheadings, c("matrix", "data.frame"))) {
    colheadings <- apply(colheadings, 2, as.character)
  } else {
    checkmate::assertCharacter(colheadings, len=ncol(d), null.ok=TRUE)
    if (is.null(colheadings)) colheadings <- colnames(d)
    colheadings[is.na(colheadings)] <- ""
    colheadings <- t(matrix(colheadings))
  }
  checkmate::assertMatrix(colheadings, ncols=ncol(d), min.rows=1)

  checkmate::assertCharacter(align, any.missing=FALSE, min.len=1, max.len=ncol(d), null.ok=TRUE)
  if (!is.null(align)) align <- rep(align, length.out=ncol(d))

  checkmate::assertIntegerish(digits, any.missing=FALSE, min.len=1, max.len=ncol(d), null.ok=TRUE)
  if (!is.null(digits)) digits <- rep(digits, length.out=ncol(d))

  checkmate::assertString(label, null.ok=TRUE)
  checkmate::assertString(title, null.ok=TRUE)
  checkmate::assertString(headnotes, null.ok=TRUE)
  checkmate::assertString(footnotes, null.ok=TRUE)
  checkmate::assertIntegerish(nrec, lower=1, any.missing=FALSE, min.len=1, max.len=2)
  checkmate::assertIntegerish(hline, lower=1, upper=nrow(d) - 1,
                              any.missing=FALSE, null.ok=TRUE)
  checkmate::assertString(na, null.ok=TRUE)
  checkmate::assertInt(rm_dup, lower=1, upper=ncol(d), null.ok=TRUE)
  checkmate::assertFlag(landscape)

  font <- "\\normalfont\\bfseries\\sffamily"

  colheadings[is.na(colheadings)] <- ""
  if (nrow(colheadings) > 1) {
    colheadings <- apply(colheadings, 2, function(x) {
      x[duplicated(x)] <- ""
      is <- x == ""
      c(x[!is], x[is])
    })
  }

  cmd <- NULL
  for (i in seq_len(nrow(colheadings))) {

    x <- colheadings[i, 1]
    cols <- 1L
    for (j in seq_len(ncol(colheadings) - 1)) {
      n <- length(cols)
      if (x[n] == colheadings[i, j + 1]) {
        cols[n] <- cols[n] + 1L
      } else {
        x[n + 1] <- colheadings[i, j + 1]
        cols[n + 1] <- 1L
      }
    }

    line <- NULL
    rows <- rep(1L, length(x))
    if (i < nrow(colheadings)) {
      cnt <- 0L
      for (k in seq_along(x)) {
        idx <- cnt + 1L
        cnt <- cnt + cols[k]
        if (x[k] != "" & all(colheadings[i + 1, idx:cnt] == ""))
          rows[k] <- nrow(colheadings) - i + 1L
        if (rows[k] == 1 & cols[k] > 1)
          line <- paste0(line, sprintf("\\cmidrule(lr){%d-%d}", idx, cnt))
      }
    }

    is <- x != ""
    x[is] <- sprintf("{%s \\makecell{%s}}", font, x[is])

    is <- rows > 1
    fmt <- "\\multirow{%d}{*}[-0.5\\dimexpr \\aboverulesep + \\belowrulesep + \\cmidrulewidth]{%s}"
    x[is] <- sprintf(fmt, rows[is], x[is])

    x <- sprintf("\\multicolumn{%d}{c}{%s}", cols, x)

    cmd[i] <- paste0(paste(x, collapse=" & "), " \\\\ ", line, "\n")
  }

  if (methods::hasArg("include.rownames") && list(...)$include.rownames)
    cmd <- paste("&", cmd)

  add.to.row <- list()
  add.to.row$pos <- list(0)
  add.to.row$command <- paste(cmd, collapse="")

  cap1 <- strwrap(title, width=.Machine$integer.max)
  cap2 <- strwrap(headnotes, width=.Machine$integer.max)

  idxs <- which(unlist(lapply(d, is.factor)))
  d[, idxs] <- lapply(d[, idxs, drop=FALSE], as.character)

  n <- nrow(d)
  if (n > nrec[1]) {
    if (length(nrec) == 1) nrec[2] <- nrec[1]
    n <- unique(c(cumsum(c(nrec[1], rep(nrec[2], (n - nrec[1]) %/% nrec[2]))), n))
  }

  Print <- xtable::print.xtable
  formals(Print)$type <- "latex"
  formals(Print)$caption.placement <- "top"
  formals(Print)$size <- "\\small"
  formals(Print)$NA.string <- na
  formals(Print)$include.colnames <- FALSE
  formals(Print)$sanitize.text.function <- identity
  formals(Print)$sanitize.colnames.function <- function(x) {x}
  formals(Print)$include.rownames <- FALSE
  formals(Print)$math.style.exponents <- TRUE
  formals(Print)$format.args <- list("big.mark"=",")
  formals(Print)$booktabs <- TRUE
  formals(Print)$comment <- FALSE

  for (i in seq_along(n)) {
    if (i > 1) cat("\n\\clearpage\n")
    if (i == 2) cat("\\captionsetup[table]{list=no}\n")
    if (landscape) cat("\\begin{landscape}\n")

    if (i == 1) {
      idxs <- 1:n[i]
      caption <- c(sprintf("%s\\par \\medskip [\\footnotesize{%s}]", cap1, cap2), cap1)
    } else {
      idxs <- (n[i - 1] + 1):n[i]
      caption <- sprintf("%s---Continued", cap1)
      label <- NULL
      cat("\\addtocounter{table}{-1}\n")
    }

    tbl <- d[idxs, ]
    if (!is.null(rm_dup))
      for (j in rev(seq_len(rm_dup)))
        tbl[[j]][duplicated(tbl[, seq_len(j), drop=FALSE])] <- ""

    tbl <- xtable::xtable(tbl)
    if (length(caption) > 0) {
      xtable::caption(tbl) <- caption
      xtable::label(tbl) <- label
    }

    row_names <- utils::type.convert(rownames(d))
    row_align <- ifelse(is.numeric(row_names), "r", "l")
    row_digits <- ifelse(is.double(row_names), format.info(row_names)[2], 0)
    if (!is.null(align)) xtable::align(tbl) <- c(row_align, align)

    x <- switch(1 + is.null(digits), digits, rep(3, ncol(d)))
    xtable::digits(tbl) <- c(row_digits, x)

    hline.after <- sort(unique(stats::na.omit(c(-1, 0, match(c(hline, nrow(d)), idxs)))))

    if (!is.null(footnotes) && i == length(n)) {
      fmt <- "\\midrule\n\\multicolumn{%s}{l}{\\makecell[l]{%s}} \\\\\n"
      add.to.row$command[2] <- sprintf(fmt, ncol(tbl), footnotes)
      add.to.row$pos[[2]] <- nrow(tbl)
      hline.after <- utils::head(hline.after, -1)
    }

    Print(x=tbl, hline.after=hline.after, add.to.row=add.to.row, ...)

    if (landscape) cat("\\end{landscape}\n")
    if (i > 1 && i == length(n)) cat("\\captionsetup[table]{list=yes}\n")
  }

  invisible()
}
