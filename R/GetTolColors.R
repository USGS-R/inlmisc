#' Tol Color Palettes
#'
#' This function creates a vector of \code{n} contiguous colors from
#' color schemes by Paul Tol (2018).
#'
#' @param n 'integer'.
#'   Number of colors to be in the palette, the maximum value is based on the specified color scheme.
#' @param scheme 'character'.
#'   Color scheme: select
#'   \code{"bright"}, \code{"vibrant"}, \code{"muted"}, or \code{"light"} for sequential colors; and
#'   \code{"rainbow"} for discrete colors.
#'   Where \code{n < 8} for \code{"bright"} and \code{"vibrant"},
#'   \code{n < 10} for \code{"muted"} and \code{"light"}, and
#'   \code{n < 24} for \code{"rainbow"}.
#' @param alpha 'numeric'.
#'   Alpha transparency, values range from 0 (fully transparent) to 1 (fully opaque).
#'   Specify as \code{NULL} to exclude the alpha channel color component.
#' @param plot 'logical'.
#'   Whether to display the color palette.
#'
#' @return Returns a 'character' vector of length \code{n} with elements of 7 or 9 characters,
#'   \code{"#"} followed by the red, blue, green, and optionally alpha values in hexadecimal.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @references
#'   Tol, Paul, 2018, Colour Schemes:
#'   SRON Technical Note, doc. no. SRON/EPS/TN/09-002, issue 3.0, 17 p.,
#'   accessed July 18, 2018 at \url{https://personal.sron.nl/~pault/data/colourschemes.pdf}.
#'
#' @keywords color
#'
#' @export
#'
#' @examples
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0), mai = c(0.4, 0, 0.4, 0))
#' GetTolColors( 7, scheme = "bright",  plot = TRUE)
#' GetTolColors( 7, scheme = "vibrant", plot = TRUE)
#' GetTolColors( 9, scheme = "muted",   plot = TRUE)
#' GetTolColors( 9, scheme = "light",   plot = TRUE)
#' GetTolColors(23, scheme = "rainbow", plot = TRUE)
#' par(op)
#'

GetTolColors <- function(n, scheme=c("bright", "vibrant", "muted", "light", "rainbow"),
                         alpha=NULL, plot=FALSE) {

  scheme <- match.arg(scheme)
  nmax <- c("bright"=7, "vibrant"=7, "muted"=9, "light"=9, "rainbow"=23)
  checkmate::assertInt(n, lower=1, upper=nmax[scheme])
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
  } else if (scheme == "rainbow") {
    pal <- c("1"            = "#E8ECFB",
             "2"            = "#D9CCE3",
             "3"            = "#D1BBD7",
             "4"            = "#CAACCB",
             "5"            = "#BA8DB4",
             "6"            = "#AE76A3",
             "7"            = "#AA6F9E",
             "8"            = "#994F88",
             "9"            = "#882E72",
             "10"           = "#1965B0",
             "11"           = "#437DBF",
             "12"           = "#5289C7",
             "13"           = "#6195CF",
             "14"           = "#7BAFDE",
             "15"           = "#4EB265",
             "16"           = "#90C987",
             "17"           = "#CAE0AB",
             "18"           = "#F7F056",
             "19"           = "#F7CB45",
             "20"           = "#F6C141",
             "21"           = "#F4A736",
             "22"           = "#F1932D",
             "23"           = "#EE8026",
             "24"           = "#E8601C",
             "25"           = "#E65518",
             "26"           = "#DC050C",
             "27"           = "#A5170E",
             "28"           = "#72190E",
             "29"           = "#42150A")
    idx <- list(c(10),
                c(10, 26),
                c(10, 18, 26),
                c(10, 15, 18, 26),
                c(10, 14, 15, 18, 26),
                c(10, 14, 15, 17, 18, 26),
                c( 9, 10, 14, 15, 17, 18, 26),
                c( 9, 10, 14, 15, 17, 18, 23, 26),
                c( 9, 10, 14, 15, 17, 18, 23, 26, 28),
                c( 9, 10, 14, 15, 17, 18, 21, 24, 26, 28),
                c( 9, 10, 12, 14, 15, 17, 18, 21, 24, 26, 28),
                c( 3,  6,  9, 10, 12, 14, 15, 17, 18, 21, 24, 26),
                c( 3,  6,  9, 10, 12, 14, 15, 16, 17, 18, 21, 24, 26),
                c( 3,  6,  9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26),
                c( 3,  6,  9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 28),
                c( 3,  5,  7,  9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 28),
                c( 3,  5,  7,  8,  9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 28),
                c( 3,  5,  7,  8,  9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 27, 28),
                c( 2,  4,  5,  7,  8,  9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 27, 28),
                c( 2,  4,  5,  7,  8,  9, 10, 11, 13, 14, 15, 16, 17, 18, 20, 22, 24, 26, 27, 28),
                c( 2,  4,  5,  7,  8,  9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 21, 23, 25, 26, 27, 28),
                c( 2,  4,  5,  7,  8,  9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 21, 23, 25, 26, 27, 28, 29),
                c( 1,  2,  4,  5,  7,  8,  9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 21, 23, 25, 26, 27, 28, 29))
  }

  col <- if (scheme == "rainbow") pal[idx[[n]]] else pal[1:n]

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
