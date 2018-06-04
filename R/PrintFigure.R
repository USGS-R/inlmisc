#' Print as LaTeX Figure
#'
#' This function prints the LaTeX code associated with the supplied figure.
#' A figure can be composed of several subfigures
#' and passed to the function as \R plotting commands.
#' The applied output format attempts to adhere to the design recommendations
#' for figures in United States Geological Survey (USGS) publications.
#'
#' @param fig 'text'.
#'   Vector of figure plotting commands written in \R.
#'   The length of the vector is either equal to the number of subfigures,
#'   or 1 when a single plot is desired.
#'   An element in the vector contains the commands for creating a single plot.
#' @param nr,nc 'integer'.
#'   Maximum number of rows and columns in the subfigure layout on a page in the output document.
#' @param label 'character'.
#'   String containing the LaTeX label anchor.
#'   Subfigures are labeled using a concatenation of the \code{label} argument and an index number.
#'   For example, specifying \code{label = "id"} for a figure composed of 3 subfigures results in:
#'   labels \code{"id-1"}, \code{"id-2"}, and \code{"id-3"}.
#' @param title 'character'.
#'   String containing the figure caption.
#' @param title_lof 'character'.
#'   String containing the figure caption to be listed at the beginning
#'   of the paper in a \dQuote{List of Figures}.
#' @param headings 'character'.
#'   Vector of subfigure captions, values are recycled as necessary
#'   to match the vector length of the \code{fig} argument.
#'
#' @details
#'   Requires \code{\\usepackage{caption}} and \code{\\usepackage{subcaption}} in the LaTeX preamble.
#'   The width and height, in inches, to be used in the graphics device (that is, a single plot)
#'   are specified in the code-chunk options \code{fig.width} and \code{fig.height}, respectively.
#'   And always write raw results from \R into the output document
#'   by also specifying \code{results = "asis"} in the code-chunk options.
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
#' \dontrun{
#' cat("\\documentclass{article}",
#'     "\\usepackage[labelsep=period, labelfont=bf]{caption}",
#'     "\\usepackage{subcaption}",
#'     "\\captionsetup[figure]{skip=5pt}",
#'     "\\captionsetup[subfigure]{skip=-5pt, labelfont={bf, it}}",
#'     "\\renewcommand{\\thesubfigure}{\\Alph{subfigure}}",
#'     "\\begin{document}\n",
#'     "<<id, echo=FALSE, fig.width=3, fig.height=2, results='asis'>>=",
#'     "par(mar=c(2.1, 2.1, 1.1, 1.1))",
#'     "fig <- sprintf('plot(runif(%s))', 1:9)",
#'     "headings <- sprintf('Subfigure caption, n=%s', 1:9)",
#'     "PrintFigure(fig, 3, 2, 'id', title='Figure caption', headings=headings)",
#'     "@\n",
#'     "\\end{document}",
#'     file = "figure-example.Rnw", sep = "\n")
#' knitr::knit2pdf("figure-example.Rnw", clean = TRUE)  # requires TeX installation
#' system("open figure-example.pdf")
#'
#' unlink("figure-example.*")
#' unlink("figure", recursive = TRUE)
#' }
#'

PrintFigure <- function(fig, nr=1, nc=1, label="", title="", title_lof=title, headings="") {

  # check arguments
  checkmate::assertCharacter(fig, any.missing=FALSE, min.len=1)
  checkmate::assertInt(nr, lower=1)
  checkmate::assertInt(nc, lower=1)
  checkmate::assertString(label)
  checkmate::assertString(title)
  checkmate::assertString(title_lof)
  checkmate::assertCharacter(headings, any.missing=FALSE, min.len=1, max.len=length(fig))

  # total number of plots on all pages
  np <- length(fig)

  # maximum number of plots on a page
  n <- nr * nc
  if (np < n) n <- np

  # recycle subfigure captions
  headings <- rep(headings, length.out=np)

  # make subfigure labels unique
  if (np > 1) label <- sprintf("%s-%s", label, 1:np)

  cat("\n")
  for (i in seq_len(np)) {
    if (i == n) {
      caption <- strwrap(title, width=.Machine$integer.max)
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
      cat(sprintf("    \\caption{{%s \\label{fig:%s}}}\n", headings[i], label[i]))
    }

    # evaluate plotting commands
    eval(parse(text=fig[i]))

    cat("\n")
    if (np > 1) cat("  \\end{subfigure}\n")

    if (!is.na(caption)) {
      caption_lof <- strwrap(title_lof, width=.Machine$integer.max)
      if (caption == caption_lof || caption == "---Continued")
        cat(sprintf("  \\caption{{%s}}\n", caption))
      else
        cat(sprintf("  \\caption[{%s}]{{%s}}\n", caption_lof, caption))
    }

    if (i %% n == 0 || i == np) cat("\\end{figure}\n\n")
    if (i > n && i == np) cat("\\captionsetup[figure]{list=yes}\n\n")
  }

  invisible(NULL)
}
