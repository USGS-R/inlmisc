#' Color Palette for Qualitative Data
#'
#' This function creates a vector of \code{n} contiguous colors from
#' qualitative color schemes by Paul Tol (2018).
#'
#' @param n 'integer'.
#'   Number of colors to be in the palette, the maximum is based on the specified color scheme.
#' @param scheme 'character'.
#'   Qualitative color scheme:
#'   \code{"bright"}, \code{"vibrant"}, \code{"muted"}, or \code{"light"}.
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
#'   Tol, Paul, 2018, Colour Schemes:
#'   SRON Technical Note, doc. no. SRON/EPS/TN/09-002, issue 3.0, 17 p.,
#'   accesed July 18, 2018 at \url{https://personal.sron.nl/~pault/data/colourschemes.pdf}.
#'
#' @keywords color
#'
#' @export
#'
#' @examples
#' op <- par(mfrow = c(4, 1), oma = c(1, 1, 1, 1))
#' GetTolColors(7, scheme = "bright",  plot = TRUE)
#' GetTolColors(7, scheme = "vibrant", plot = TRUE)
#' GetTolColors(9, scheme = "muted",   plot = TRUE)
#' GetTolColors(9, scheme = "light",   plot = TRUE)
#' par(op)
#'

GetTolColors <- function(n, scheme=c("bright", "vibrant", "muted", "light"),
                         alpha=NULL, plot=FALSE) {

  checkmate::assertInt(n, lower=1)
  scheme <- match.arg(scheme)
  checkmate::assertNumber(alpha, lower=0, upper=1, finite=TRUE, null.ok=TRUE)
  checkmate::assertFlag(plot)

  if (scheme == "bright") {
    pal <- c("blue"         = "#4477AA",
             "red"          = "#EE6677",
             "green"        = "#228833",
             "yellow"       = "#CCBB44",
             "cyan"         = "#66CCEE",
             "purple"       = "#AA3377",
             "grey"         = "#BBBBBB")
  } else if (scheme == "vibrant") {
    pal <- c("orange"       = "#EE7733",
             "blue"         = "#0077BB",
             "cyan"         = "#33BBEE",
             "magenta"      = "#EE3377",
             "red"          = "#CC3311",
             "teal"         = "#009988",
             "grey"         = "#BBBBBB")
  } else if (scheme == "muted") {
    pal <- c("rose"         = "#CC6677",
             "indigo"       = "#332288",
             "sand"         = "#DDCC77",
             "green"        = "#117733",
             "cyan"         = "#88CCEE",
             "wine"         = "#882255",
             "teal"         = "#44AA99",
             "olive"        = "#999933",
             "purple"       = "#AA4499")
  } else if (scheme == "light") {
    pal <- c("libht blue"   = "#77AADD",
             "orange"       = "#EE8866",
             "light yellow" = "#EEDD88",
             "pink"         = "#FFAABB",
             "light cyan"   = "#99DDFF",
             "mint"         = "#44BB99",
             "pear"         = "#BBCC33",
             "olive"        = "#AAAA00",
             "pale grey"    = "#DDDDDD")
  }
  checkmate::assertInt(n, upper=length(pal))
  col <- pal[1:n]

  if (!is.null(alpha)) {
    col <- grDevices::adjustcolor(col, alpha.f=alpha)
    names(col) <- names(pal)[1:n]
  }

  if (plot) {
    graphics::plot.default(0, 0, type="n", xlim=c(0, 1), ylim=c(0, 1),
                           axes=FALSE, xlab="", ylab="", main=scheme)
    graphics::rect(0:(n - 1) / n, 0, 1:n / n, 1, col=col, border="#D3D3D3")
    at <- 0:(n - 1) / n + (1 / (2 * n))
    graphics::axis(1, at=at, labels=names(col), tick=FALSE)
  }

  return(col)
}
