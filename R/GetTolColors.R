#' Color Palette for Qualitative Data
#'
#' This function creates a vector of \code{n} contiguous colors from color schemes by Paul Tol (2012).
#'
#' @param n 'integer'.
#'   Number of colors to be in the palette, the maximum is 21.
#' @param alpha 'numeric'.
#'   Alpha transparency, parameter values range from 0 (fully transparent) to 1 (fully opaque).
#'   Specify as \code{NULL} to exclude the alpha channel color component.
#' @param plot 'logical'.
#'   Whether to display the color palette.
#'
#' @return Returns a 'character' vector of length \code{n} with elements of 7 or 9 characters,
#'   "#" followed by the red, blue, green, and optionally alpha values in hexadecimal.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @references
#'   Tol, Paul, 2012, Colour Schemes:
#'   SRON Technical Note, doc. no. SRON/EPS/TN/09-002, issue 2.2, 16 p.,
#'   accesed January 26, 2018 at \url{https://personal.sron.nl/~pault/colourschemes.pdf}.
#'
#' @keywords color
#'
#' @export
#'
#' @examples
#' op <- par(mfrow = c(2, 1), oma = c(1, 1, 1, 1))
#' GetTolColors(7, plot = TRUE)
#' GetTolColors(21, alpha = 0.85, plot = TRUE)
#' par(op)
#'

GetTolColors <- function(n, alpha=1, plot=FALSE) {

  checkmate::assertInt(n, lower=1, upper=21)
  checkmate::assertNumber(alpha, lower=0, upper=1, finite=TRUE, null.ok=TRUE)
  checkmate::assertFlag(plot)

  # color schemes copied from Peter Carl's blog post, accessed January 26, 2018 at
  # https://tradeblotter.wordpress.com/2013/02/28/the-paul-tol-21-color-salute/
  pal <- list(c("#4477AA"),
              c("#4477AA", "#CC6677"),
              c("#4477AA", "#DDCC77", "#CC6677"),
              c("#4477AA", "#117733", "#DDCC77", "#CC6677"),
              c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677"),
              c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677", "#AA4499"),
              c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677", "#AA4499"),
              c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#AA4499"),
              c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499"),
              c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
              c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
              c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499"),
              c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C"),
              c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C"),
              c("#114477", "#4477AA", "#77AADD", "#117755", "#44AA88", "#99CCBB", "#777711", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA"),
              c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122"),
              c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455"),
              c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788"),
              c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122"),
              c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455"),
              c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788"))
  col <- pal[[n]]

  if (is.finite(alpha)) col <- grDevices::adjustcolor(col, alpha.f=alpha)

  if (plot) {
    graphics::plot.default(0, 0, type="n", xlim=c(0, 1), ylim=c(0, 1), axes=FALSE, xlab="", ylab="")
    graphics::rect(0:(n - 1) / n, 0, 1:n / n, 1, col=col, border="#D3D3D3")
  }

  return(col)
}
