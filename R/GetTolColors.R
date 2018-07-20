#' Tol Color Palettes
#'
#' This function creates a vector of \code{n} colors from
#' qualitative, diverging, and sequential color schemes by Paul Tol (2018).
#'
#' @param n 'integer'.
#'   Number of colors to be in the palette, the maximum value is
#'   dependent on the specified color scheme, see \sQuote{Details} section.
#' @param scheme 'character'.
#'   Color scheme: select
#'     \code{"bright"}, \code{"vibrant"}, \code{"muted"}, \code{"pale"},
#'       \code{"dark"}, or \code{"light"} for qualitative colors;
#'     \code{"sunset"}, \code{"BuRd"}, or \code{"PRGn"} for diverging colors;
#'     \code{"YlOrBr"}, \code{"discrete rainbow"} or \code{"smooth rainbow"} for sequential colors; and
#'     \code{"ground cover"} for global land cover classification.
#' @param alpha 'numeric'.
#'   Alpha transparency, values range from 0 (fully transparent) to 1 (fully opaque).
#'   Specify as \code{NULL} to exclude the alpha channel color component.
#' @param start,end 'numeric'.
#'   Starting and ending color level in the palette, respectively.
#'   Specified as a number in the interval from 0 to 1.
#'   Applies only to interpolated color schemes:
#'   \code{"sunset"}, \code{"BuRd"}, \code{"PRGn"}, \code{"YlOrBr"}, and \code{"smooth rainbow"}.
#' @param ...
#'   Additional arguments to be passed to the \code{\link[grDevices]{colorRamp}} function
#'   and used to interpolate a color scheme.
#' @param plot 'logical'.
#'   Whether to display the color palette.
#'
#' @details Limits on the maximum number of discrete colors for a scheme are:
#'   \code{n < 8} for \code{"bright"} and \code{"vibrant"};
#'   \code{n < 10} for \code{"muted"}, \code{"light"}, \code{"YlOrBr"}, \code{"BuRd"}, and \code{"PRGn"};
#'   \code{n < 24} for \code{"discrete rainbow"}.
#'   Schemes \code{"ground cover"} (\code{n = 14}), \code{"pale"} (\code{n = 6}),
#'   and \code{"dark"} (\code{n = 6}) are intended to be
#'   accessed in their entirety and subset using element names.
#'
#' @return Returns a 'character' vector of length \code{n} with elements of 7 or 9 characters,
#'   \code{"#"} followed by the red, blue, green, and optionally alpha values in hexadecimal.
#'   For some schemes the returned object includes a \code{"bad"} attribute that
#'   indicates the color assigned to bad data.
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
#'
#' col <- GetTolColors(3); print(col)
#' attr(col, "bad")
#'
#' # Qualitative schemes
#' op <- par(mfrow = c(6, 1), oma = c(0, 0, 0, 0), mai = c(0.4, 0, 0.4, 0))
#' GetTolColors(7, scheme = "bright",  plot = TRUE)
#' GetTolColors(7, scheme = "vibrant", plot = TRUE)
#' GetTolColors(9, scheme = "muted",   plot = TRUE)
#' GetTolColors(6, scheme = "pale",    plot = TRUE)
#' GetTolColors(6, scheme = "dark",    plot = TRUE)
#' GetTolColors(9, scheme = "light",   plot = TRUE)
#' par(op)
#'
#' # Diverging schemes
#' op <- par(mfrow = c(6, 1), oma = c(0, 0, 0, 0), mai = c(0.4, 0, 0.4, 0))
#' GetTolColors( 11, scheme = "sunset", plot = TRUE)
#' GetTolColors(255, scheme = "sunset", plot = TRUE)
#' GetTolColors(  9, scheme = "BuRd",   plot = TRUE)
#' GetTolColors(255, scheme = "BuRd",   plot = TRUE)
#' GetTolColors(  9, scheme = "PRGn",   plot = TRUE)
#' GetTolColors(255, scheme = "PRGn",   plot = TRUE)
#' par(op)
#'
#' # Sequential schemes
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0), mai = c(0.4, 0, 0.4, 0))
#' GetTolColors(  9, scheme = "YlOrBr",           plot = TRUE)
#' GetTolColors(255, scheme = "YlOrBr",           plot = TRUE)
#' GetTolColors( 23, scheme = "discrete rainbow", plot = TRUE)
#' GetTolColors( 34, scheme = "smooth rainbow",   plot = TRUE)
#' GetTolColors(255, scheme = "smooth rainbow",   plot = TRUE)
#' par(op)
#'
#' # Cover scheme
#' GetTolColors(14, scheme = "ground cover", plot = TRUE)
#'
#' # Alpha transparency
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0), mai = c(0.4, 0, 0.4, 0))
#' GetTolColors(23, scheme = "discrete rainbow", alpha = 1.0, plot = TRUE)
#' GetTolColors(23, scheme = "discrete rainbow", alpha = 0.8, plot = TRUE)
#' GetTolColors(23, scheme = "discrete rainbow", alpha = 0.6, plot = TRUE)
#' GetTolColors(23, scheme = "discrete rainbow", alpha = 0.4, plot = TRUE)
#' GetTolColors(23, scheme = "discrete rainbow", alpha = 0.2, plot = TRUE)
#' par(op)
#'
#' # Color levels
#' op <- par(mfrow = c(4, 1), oma = c(0, 0, 0, 0), mai = c(0.4, 0, 0.4, 0))
#' GetTolColors(255, start = 0.0, end = 1.0, plot = TRUE)
#' GetTolColors(255, start = 0.0, end = 0.5, plot = TRUE)
#' GetTolColors(255, start = 0.5, end = 1.0, plot = TRUE)
#' GetTolColors(255, start = 0.2, end = 0.8, plot = TRUE)
#' par(op)
#'

GetTolColors <- function(n, scheme="smooth rainbow", alpha=NULL,
                         start=0, end=1, ..., plot=FALSE) {

  nmax <- c("bright"           = 7,    # qualitative
            "vibrant"          = 7,
            "muted"            = 9,
            "pale"             = 6,
            "dark"             = 6,
            "light"            = 9,
            "sunset"           = Inf,  # diverging
            "BuRd"             = Inf,
            "PRGn"             = Inf,
            "YlOrBr"           = Inf,  # sequential
            "discrete rainbow" = 23,
            "smooth rainbow"   = Inf,
            "ground cover"     = 14)   # cover
  schemes <- names(nmax)

  scheme <- match.arg(scheme, schemes)
  checkmate::assertInt(n, lower=1, upper=nmax[scheme])
  checkmate::assertNumber(alpha, lower=0, upper=1, finite=TRUE, null.ok=TRUE)
  checkmate::assertNumber(start, lower=0, upper=1, finite=TRUE)
  checkmate::assertNumber(end, lower=start, upper=1, finite=TRUE)
  checkmate::assertFlag(plot)

  bad <- NULL

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
    bad <- "#DDDDDD"
  } else if (scheme == "pale") {
    pal <- c("pale blue"    = "#BBCCEE",
             "pale cyan"    = "#CCEEFF",
             "pale green"   = "#CCDDAA",
             "pale yellow"  = "#EEEEBB",
             "pale red"     = "#FFCCCC",
             "pale grey"    = "#DDDDDD")
  } else if (scheme == "dark") {
    pal <- c("dark blue"    = "#222255",
             "dark cyan"    = "#225555",
             "dark green"   = "#225522",
             "dark yellow"  = "#666633",
             "dark red"     = "#663333",
             "dark grey"    = "#555555")
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
  } else if (scheme == "sunset") {
    pal <- c("1"            = "#364B9A",
             "2"            = "#4A7BB7",
             "3"            = "#6EA6CD",
             "4"            = "#98CAE1",
             "5"            = "#C2E4EF",
             "6"            = "#EAECCC",
             "7"            = "#FEDA8B",
             "8"            = "#FDB366",
             "9"            = "#F67E4B",
             "10"           = "#DD3D2D",
             "11"           = "#A50026")
    bad <- "#FFFFFF"
  } else if (scheme == "BuRd") {
    pal <- c("1"            = "#2166AC",
             "2"            = "#4393C3",
             "3"            = "#92C5DE",
             "4"            = "#D1E5F0",
             "5"            = "#F7F7F7",
             "6"            = "#FDDBC7",
             "7"            = "#F4A582",
             "8"            = "#D6604D",
             "9"            = "#B2182B")
    bad <- "#FFEE99"
  } else if (scheme == "PRGn") {
    pal <- c("1"            = "#762A83",
             "2"            = "#9970AB",
             "3"            = "#C2A5CF",
             "4"            = "#E7D4E8",
             "5"            = "#F7F7F7",
             "6"            = "#D9F0D3",
             "7"            = "#ACD39E",
             "8"            = "#5AAE61",
             "9"            = "#1B7837")
    bad <- "#FFEE99"
  } else if (scheme == "YlOrBr") {
    pal <- c("1"            = "#FFFFE5",
             "2"            = "#FFF7BC",
             "3"            = "#FEE391",
             "4"            = "#FEC44F",
             "5"            = "#FB9A29",
             "6"            = "#EC7014",
             "7"            = "#CC4C02",
             "8"            = "#993404",
             "9"            = "#662506")
    bad <- "#888888"
  } else if (scheme == "discrete rainbow") {
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
    bad <- "#777777"
  } else if (scheme == "smooth rainbow") {
    pal <- c("1"            = "#E8ECFB",
             "2"            = "#DDD8EF",
             "3"            = "#D1C1E1",
             "4"            = "#C3A8D1",
             "5"            = "#B58FC2",
             "6"            = "#A778B4",
             "7"            = "#9B62A7",
             "8"            = "#8C4E99",
             "9"            = "#6F4C9B",
             "10"           = "#6059A9",
             "11"           = "#5568B8",
             "12"           = "#4E79C5",
             "13"           = "#4D8AC6",
             "14"           = "#4E96BC",
             "15"           = "#549EB3",
             "16"           = "#59A5A9",
             "17"           = "#60AB9E",
             "18"           = "#69B190",
             "19"           = "#77B77D",
             "20"           = "#8CBC68",
             "21"           = "#A6BE54",
             "22"           = "#BEBC48",
             "23"           = "#D1B541",
             "24"           = "#DDAA3C",
             "25"           = "#E49C39",
             "26"           = "#E78C35",
             "27"           = "#E67932",
             "28"           = "#E4632D",
             "29"           = "#DF4828",
             "30"           = "#DA2222",
             "31"           = "#B8221E",
             "32"           = "#95211B",
             "33"           = "#721E17",
             "34"           = "#521A13")
    bad <- "#666666"
  } else if (scheme == "ground cover") {
    pal <- c("water"                       = "#5566AA",
             "evergreen needleleaf forest" = "#117733",
             "deciduous needleleaf forest" = "#44AA66",
             "mixed forest"                = "#55AA22",
             "evergreen broadleaf forest"  = "#668822",
             "deciduous broadleaf forest"  = "#99BB55",
             "woodland"                    = "#558877",
             "wooded grassland"            = "#88BBAA",
             "grassland"                   = "#AADDCC",
             "cropland"                    = "#44AA88",
             "closed shrubland"            = "#DDCC66",
             "open shrubland"              = "#FFDD44",
             "bare ground"                 = "#FFEE88",
             "urban and built"             = "#BB0011")
  }

  if (scheme %in% c("bright", "vibrant", "muted", "pale", "dark", "light", "ground cover")) {
    col <- pal[1:n]
  } else if (scheme == "discrete rainbow") {
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
    col <- pal[idx[[n]]]
  } else {
    norm <- (seq_along(pal) - 1) / (length(pal) - 1)
    idxs <- seq.int(which.min(abs(norm - start)), which.min(abs(norm - end)), 1)
    if (length(idxs) < 2) stop("problem with 'start' and (or) 'end' argument(s)")
    col <- grDevices::colorRampPalette(pal[idxs], ...)(n)
    names(col) <- seq_along(col)
  }

  labels <- names(col)
  if (!is.null(alpha)) {
    col <- grDevices::adjustcolor(col, alpha.f=alpha); names(col) <- labels
    if (!is.null(bad)) bad <- grDevices::adjustcolor(bad, alpha.f=alpha)
  }

  attr(col, "bad") <- bad

  if (plot) {
    if (any(is <- c(!is.null(alpha), start > 0 | end < 1))) {
      txt <- c(paste0("alpha = ", alpha), paste0("start = ", start, ", end = ", end))
      main <- sprintf("%s (%s)", scheme, paste(txt[is], collapse=", "))
    } else {
      main <- scheme
    }
    graphics::plot.default(0, 0, type="n", xlim=c(0, 1), ylim=c(0, 1),
                           axes=FALSE, xlab="", ylab="", main=main)
    if (n > 50) {
      border <- NA
      labels <- FALSE
    } else {
      border <- "#D3D3D3"
    }
    graphics::rect(0:(n - 1) / n, 0, 1:n / n, 1, col=col, border=border)
    at <- 0:(n - 1) / n + 1 / (2 * n)
    graphics::axis(1, at=at, labels=labels, tick=FALSE, mgp=c(3, 0.2, 0))
  }

  return(col)
}
