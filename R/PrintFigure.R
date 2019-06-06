#' Print as LaTeX Figure
#'
#' Print the LaTeX code associated with the supplied figure.
#' A figure can be composed of several subfigures
#' and passed to the function as \R plotting commands.
#' The applied output format attempts to adhere to the design recommendations
#' for figures in United States Geological Survey (USGS) publications.
#'
#' @param fig 'character' vector.
#'   Figure plotting commands written in \R.
#'   The length of the vector is either equal to the number of subfigures,
#'   or 1 when a single plot is desired.
#'   An element in the vector contains the commands for creating a single plot.
#' @param nr,nc 'integer' count.
#'   Maximum number of rows and columns in the subfigure layout on a page in the output document.
#' @param label 'character' string.
#'   LaTeX label anchor.
#'   Subfigures are labeled using a concatenation of the \code{label} argument and an index number.
#'   For example, specifying \code{label = "id"} for a figure composed of 3 subfigures results in:
#'   labels \code{"id-1"}, \code{"id-2"}, and \code{"id-3"}.
#' @param title 'character' string.
#'   Figure caption
#' @param title_lof 'character' string.
#'   Figure caption to be listed at the beginning of the paper in a \dQuote{List of Figures}.
#' @param headings 'character' vector.
#'   Subfigure captions, values are recycled as necessary
#'   to match the vector length of the \code{fig} argument.
#'   To exclude a subfigure caption specify its vector element as \code{NA}.
#' @param pos 'character' string.
#'   Placement specifiers to be used in \code{\\begin{figure}[pos]}.
#'   The specifiers can consist of the following characters in any order:
#'   \itemize{
#'     \item \code{"h"} place the float about at the same point it occurs in the source text;
#'     \item \code{"t"} position at the top of the page;
#'     \item \code{"b"} position at the bottom of the page;
#'     \item \code{"p"} put on a special page for floats only;
#'     \item \code{"!"} override internal parameters LaTeX uses for determining float positions; and
#'     \item \code{"H"} places the float at precisely the location in the source text,
#'       requires \code{\\usepackage{float}} in the LaTeX preamble.
#'   }
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
#'     "\\captionsetup[figure]{skip=10pt}",
#'     "\\captionsetup[subfigure]{skip=-10pt, labelfont={bf, it}}",
#'     "\\renewcommand{\\thesubfigure}{\\Alph{subfigure}}",
#'     "\\begin{document}",
#'     "<<id, echo=FALSE, fig.width=3, fig.height=2, results='asis'>>=",
#'     "par(mar=c(2.1, 2.1, 1.1, 1.1))",
#'     "n <- 9",
#'     "fig <- sprintf('plot(runif(%s))', seq_len(n))",
#'     "headings <- sprintf('Subfigure caption, n=%s', seq_len(n))",
#'     "PrintFigure(fig, 3, 2, 'id', title='Figure caption', headings=headings)",
#'     "@",
#'     "\\end{document}",
#'     file = "figure-example.Rnw", sep = "\n")
#' knitr::knit2pdf("figure-example.Rnw", clean = TRUE)  # requires TeX installation
#' system("open figure-example.pdf")
#'
#' unlink("figure-example.*")
#' unlink("figure", recursive = TRUE)
#' }
#'

PrintFigure <- function(fig, nr=1, nc=1, label="", title="", title_lof=title,
                        headings="", pos="") {

  # check arguments
  checkmate::assertCharacter(fig, any.missing=FALSE, min.len=1)
  checkmate::assertCount(nr, positive=TRUE)
  checkmate::assertCount(nc, positive=TRUE)
  checkmate::assertString(label)
  checkmate::assertString(title)
  checkmate::assertString(title_lof)
  checkmate::assertCharacter(headings, min.len=1, max.len=length(fig))
  checkmate::assertString(pos)

  # total number of plots on all pages
  np <- length(fig)

  # maximum number of plots on a page
  n <- nr * nc
  if (np < n) n <- np

  # recycle subfigure caption
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

    if (i == n + 1) cat("\\captionsetup[figure]{list=no}\n\n")

    if ((i - 1) %% n == 0) {
      if (nchar(pos) > 0)
        cat(sprintf("\\begin{figure}[%s]\n", pos))
      else
        cat("\\begin{figure}\n")
      if (i > 1) cat("  \\ContinuedFloat\n")
    } else if ((i - 1) %% nc == 0) {
      cat("  \\par\\bigskip\n")
    } else {
      cat("  \\qquad\n")
    }

    cat(sprintf("  \\begin{subfigure}{%.2f\\textwidth}\n", 1 / nc))
    cat("    \\centering\n")

    if (!is.na(headings[i])) {
      if (np > 1) {
        cat(sprintf("    \\caption{{%s \\label{fig:%s}}}\n", headings[i], label[i]))
      } else if (nchar(headings[i]) > 0) {
        cat(sprintf("    \\caption*{{%s}}\n", headings[i]))
      }
    }

    eval(parse(text=fig[i]))
    cat("\n")

    cat("  \\end{subfigure}\n")

    if (!is.na(caption)) {
      caption_lof <- strwrap(title_lof, width=.Machine$integer.max)
      if (caption == caption_lof || caption == "---Continued")
        cat(sprintf("  \\caption{{%s}}\n", caption))
      else
        cat(sprintf("  \\caption[{%s}]{{%s}}\n", caption_lof, caption))
    }

    if (i %% n == 0 || i == np) cat("\\end{figure}\n\n")

    if (i > n && i == np) cat("\\captionsetup[figure]{list=yes}\n")
  }

  invisible()
}
