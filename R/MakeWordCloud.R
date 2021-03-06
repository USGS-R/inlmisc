#' Create a Word Cloud from a Frequency Table of Words
#'
#' Create a word cloud from a frequency table of words, and save to a PNG file.
#' Visualizations are created using the
#' \sQuote{\href{https://wordcloud2-js.timdream.org/}{wordcloud2.js}} JavaScript library.
#'
#' @param x 'data.frame'.
#'   A frequency table of words that includes \code{"word"} and \code{"freq"} in each column.
#' @param max_words 'integer' number.
#'   Maximum number of words to include in the word cloud.
#' @param size 'numeric' number.
#'   Font size, where the larger size indicates a bigger word.
#' @param shape 'character' string.
#'   Shape of the \dQuote{cloud} to draw.
#'   Possible shapes include a \code{"circle"}, \code{"cardioid"}, \code{"diamond"},
#'   \code{"triangle-forward"}, \code{"triangle"}, \code{"pentagon"}, and \code{"star"}.
#' @param ellipticity 'numeric' number.
#'   Degree of \dQuote{flatness} of the shape to draw, a value between 0 and 1.
#' @param ...
#'   Additional arguments to be passed to the
#'   \code{\link[wordcloud2]{wordcloud2}} function.
#' @param width 'integer' number.
#'   Desired image width in pixels.
#' @param output 'character' string.
#'   Path to the output file, by default the word cloud is copied to a temporary file.
#' @param display 'logical' flag.
#'   Whether to display the saved PNG file in a graphics window.
#'   Requires access to the \pkg{png} package.
#'
#' @details The \pkg{webshot} package requires the external program \href{https://phantomjs.org/}{PhantomJS},
#'   which may be installed using the \code{webshot::\link[webshot]{install_phantomjs}()} command.
#'   To recompress the PNG file to a smaller size requires that the external program
#'   \href{http://optipng.sourceforge.net/}{OptiPNG} is accessible through a command window.
#'
#' @return The word cloud plots in PNG format, and the path of the output file is returned.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords hplot
#'
#' @export
#'
#' @examples
#' \dontrun{
#' MakeWordCloud(wordcloud2::demoFreq, size = 1.5, display = TRUE)
#' }
#'

MakeWordCloud <- function(x, max_words=200L, size=1, shape="circle", ellipticity=0.65, ...,
                          width=910L, output=NULL, display=FALSE) {

  # check arguments
  checkmate::assert_data_frame(x, types=c("factor", "character", "integerish"),
                               any.missing=FALSE, min.rows=3, min.cols=2)
  checkmate::assert_names(colnames(x), must.include=c("word", "freq"))
  checkmate::assert_count(max_words, positive=TRUE)
  checkmate::assert_number(size, lower=0, finite=TRUE)
  shape <- match.arg(shape, c("circle", "cardioid", "diamond", "triangle-forward",
                              "triangle", "pentagon", "star"))
  checkmate::assert_number(ellipticity, lower=0, upper=1, finite=TRUE)
  checkmate::assert_count(width, positive=TRUE)
  checkmate::assertFlag(display)

  if (display && !requireNamespace("png", quietly=TRUE)) {
    txt <- sprintf("Displaying word cloud's require access to the %s package.",
                   sQuote("png"))
    stop(txt, call.=FALSE)
  }

  if (is.null(output)) output <- tempfile(fileext=".png")
  output <- normalizePath(output, winslash="/", mustWork=FALSE)
  checkmate::assert_path_for_output(output, overwrite=TRUE, extension="png")

  # sort data in decreasing frequency
  d <- x[order(x$freq, decreasing=TRUE), ]

  # exclude words that are infrequently used
  d <- utils::head(x, max_words)

  # create word cloud html widget
  wc <- wordcloud2::wordcloud2(d, size=size, shape=shape, ellipticity=ellipticity, ...)

  # configure to not display hover labels
  sty <- htmltools::HTML(".wcLabel {display: none;}")
  tag <- htmltools::tags$head(htmltools::tags$style(sty))
  wc <- htmlwidgets::prependContent(wc, tag)

  # save word-cloud widget to a temporary html file
  html <- tempfile(fileext=".html")
  htmlwidgets::saveWidget(wc, html, selfcontained=FALSE)

  # take screenshot of html file and save to a png file
  height <- as.integer(width * ellipticity)
  webshot::webshot(url=html, file=output, vwidth=width, vheight=height,
                   cliprect="viewport", delay=10)

  # remove html files
  unlink(list.files(dirname(html), sub(".html", "", basename(html)), full.names=TRUE),
         recursive=TRUE)

  # recompress png file
  suppressWarnings(system2("optipng", c("-quiet", "-strip all", "-o7", shQuote(output))))

  # display saved png in a graphics window
  if (display && requireNamespace("grid", quietly=TRUE)) {
    grid::grid.raster(png::readPNG(output))
  }

  output
}
