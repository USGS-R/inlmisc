#' Print as LaTeX Figure
#'
#' This function prints the Rnw (\R and LaTeX) code associated with the supplied figure.
#' A figure can be composed of several subfigures
#' and passed to the function as text string(s) of \R plotting commands.
#' The applied output format attempts to adhere to the design recommendations
#' for figures in United States Geological Survey (USGS) publications.
#'
#' @param fig 'text'.
#'   Vector of figure plotting commands written in \R.
#'   The length of the vector is either equal to the number of subfigures, or 1 when a single plot is desired.
#'   An element in the vector contains the commands for creating a single plot.
#' @param nr,nc 'integer'.
#'   Maximum number of rows and columns in the subfigure layout on a page.
#' @param label 'character'.
#'   String containing the LaTeX label anchor.
#'   Subfigures are labeled using a concatenation of the \code{label} argument and a letter.
#'   For example, specifying \code{label = "id"} for a figure composed of 3 subfigures results in:
#'   \code{"id_a"}, \code{"id_b"}, and \code{"id_c"} labels.
#' @param title 'character'.
#'   String containing the figure caption.
#' @param headings 'character'.
#'   Vector of subfigure captions, values are recycled as necessary
#'   to match the vector length of the \code{fig} argument.
#' @param width,height 'numeric'.
#'   Figure (or subfigure) width and height in inches, to be used in the graphics device.
#'
#' @details
#'   Requires \code{\\usepackage{caption}} and \code{\\usepackage{subcaption}}
#'   in the LaTeX preamble.
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
#' fig <- sprintf("par(mar = c(2.1, 2.1, 0.1, 1.1)); plot(runif(%s))", 10)
#' title <- "Example of a figure caption."
#' PrintFigure(fig, label = "id", title = title)
#'
#' fig <- sprintf("par(mar = c(2.1, 2.1, 0.1, 1.1)); plot(runif(%s))", 1:10)
#' headings <- sprintf("Example subfigure caption, n=%s", 1:10)
#' PrintFigure(fig, 3, 2, "id", title, headings, width = 3, height = 2)
#'
#' \dontrun{
#' sink("figure-example.Rnw")
#' cat("\\documentclass{article}",
#'     "\\usepackage[labelsep=period, labelfont=bf]{caption}",
#'     "\\usepackage{subcaption}",
#'     "\\begin{document}", sep = "\n")
#' PrintFigure(fig, 3, 2, "id", title, headings, width = 3, height = 2)
#' cat("\\end{document}\n")
#' sink()
#' knitr::knit2pdf("figure-example.Rnw", clean = TRUE)  # requires TeX installation
#' system("open figure-example.pdf")
#'
#' file.remove("figure-example.Rnw", "figure-example.tex", "figure-example.pdf")
#' }
#'

PrintFigure <- function(fig, nr=1, nc=1, label="", title="", headings="", width=7, height=7) {

  # check arguments
  checkmate::assertCharacter(fig, any.missing=FALSE, min.len=1)
  checkmate::assertInt(nr, lower=1)
  checkmate::assertInt(nc, lower=1)
  checkmate::assertString(label)
  checkmate::assertString(title)
  checkmate::assertCharacter(headings, any.missing=FALSE, min.len=1, max.len=length(fig))
  checkmate::assertNumber(width, finite=TRUE)
  checkmate::assertNumber(height, finite=TRUE)

  # maximum number of plots on a page
  n <- nr * nc

  # total number of plots on all pages
  np <- length(fig)

  # recycle subfigure captions
  headings <- rep(headings, length.out=np)

  cat("\n")
  for (i in seq_len(np)) {
    sublabel <- if (np > 1) sprintf("%s_%s", label, letters[i]) else label

    if (i == n) {
      caption <- title
    } else if (i > n && ((i %% n) == 0 || i == np)) {
      caption <- "---Continued"
    } else {
      caption <- NA
    }

    if (i == n + 1L) cat("\\captionsetup[figure]{list=no}\n\n")

    if ((i - 1L) %% n == 0) {
      cat("\\begin{figure}\n")
      if (i > 1) cat("  \\ContinuedFloat\n")
    } else if ((i - 1L) %% nc == 0) {
      cat("  \\par\\bigskip\n")
    } else {
      cat("  \\qquad\n")
    }

    if (np > 1) {
      cat(sprintf("  \\begin{subfigure}{%.2f\\textwidth}\n", 1 / nc))
      cat(sprintf("    \\caption{{%s \\label{fig:%s}}}\n", headings[i], sublabel))
    }
    cat(sprintf("    <<%s, echo=FALSE, results='asis', fig.width=%s, fig.height=%s>>=\n",
                sublabel, width, height))
    cat(sprintf("    %s\n", fig[i]))
    cat("    @\n")
    if (np > 1) cat("  \\end{subfigure}\n")

    if (!is.na(caption)) cat(sprintf("  \\caption{{%s}}\n", caption))
    if (i %% n == 0 || i == np) cat("\\end{figure}\n\n")
    if (i > n && i == np) cat("\\captionsetup[figure]{list=yes}\n\n")
  }

  invisible(NULL)
}
