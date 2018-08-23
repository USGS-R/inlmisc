#' Tol Color Schemes
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
#'     \code{"dark"}, or \code{"light"} for qualitative colors;
#'     \code{"sunset"}, \code{"BuRd"}, or \code{"PRGn"} for diverging colors;
#'     \code{"YlOrBr"}, \code{"discrete rainbow"} or \code{"smooth rainbow"} for sequential colors; and
#'     \code{"ground cover"} for the \href{http://glcf.umd.edu/data/landcover/data.shtml}{AVHRR}
#'     global land cover classification (Hansen and others, 1998).
#' @param alpha 'numeric'.
#'   Alpha transparency, values range from 0 (fully transparent) to 1 (fully opaque).
#'   Specify as \code{NULL} to exclude the alpha channel value from the color name.
#' @param start,end 'numeric'.
#'   Starting and ending color level in the palette, respectively.
#'   Specified as a number in the interval from 0 to 1.
#'   Applies only to interpolated color schemes:
#'   \code{"sunset"}, \code{"BuRd"}, \code{"PRGn"}, \code{"YlOrBr"}, and \code{"smooth rainbow"}.
#' @param bias 'numeric'.
#'   Interpolation bias where larger values result in more widely spaced colors at the high end.
#'   See \code{\link[grDevices]{colorRamp}} function for details.
#' @param reverse 'logical'.
#'   Whether to reverse the direction of colors in the palette.
#' @param plot 'logical'.
#'   Whether to display the color palette.
#'
#' @details Limits on the maximum number of discrete colors for a scheme are:
#'   \code{n < 8} for \code{"bright"} and \code{"vibrant"};
#'   \code{n < 10} for \code{"muted"}, \code{"light"}, \code{"YlOrBr"}, \code{"BuRd"}, and \code{"PRGn"};
#'   \code{n < 24} for \code{"discrete rainbow"}.
#'   Schemes \code{"pale"} (\code{n = 6}),  \code{"dark"} (\code{n = 6}),
#'   and \code{"ground cover"} (\code{n = 14}) are intended to be
#'   accessed in their entirety and subset using color names.
#'
#' @return Returns a 'character' vector of \code{n} color names---each
#'   name is 7 or 9 characters and formatted as
#'   \code{"#"} followed by the red, blue, green, and optionally alpha values in hexadecimal.
#'   For some color schemes the returned object includes a \code{"bad"} attribute giving
#'   the color assigned to bad data.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @references
#'   Hansen, M., DeFries, R., Townshend, J.R.G., and Sohlberg, R., 1998,
#'   UMD Global Land Cover Classification, 1 Kilometer, 1.0:
#'   Department of Geography, University of Maryland, College Park, Maryland, 1981-1994.
#'
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
#' # Number of colors (n)
#' op <- par(mfrow = c(7, 1), oma = c(0, 0, 0, 0))
#' GetTolColors( 2, scheme = "discrete rainbow", plot = TRUE)
#' GetTolColors( 4, scheme = "discrete rainbow", plot = TRUE)
#' GetTolColors( 6, scheme = "discrete rainbow", plot = TRUE)
#' GetTolColors( 8, scheme = "discrete rainbow", plot = TRUE)
#' GetTolColors(10, scheme = "discrete rainbow", plot = TRUE)
#' GetTolColors(15, scheme = "discrete rainbow", plot = TRUE)
#' GetTolColors(23, scheme = "discrete rainbow", plot = TRUE)
#' par(op)
#'
#' # Qualitative color schemes (scheme)
#' op <- par(mfrow = c(6, 1), oma = c(0, 0, 0, 0))
#' GetTolColors(7, scheme = "bright",  plot = TRUE)
#' GetTolColors(7, scheme = "vibrant", plot = TRUE)
#' GetTolColors(9, scheme = "muted",   plot = TRUE)
#' GetTolColors(6, scheme = "pale",    plot = TRUE)
#' GetTolColors(6, scheme = "dark",    plot = TRUE)
#' GetTolColors(9, scheme = "light",   plot = TRUE)
#' par(op)
#'
#' # Diverging color schemes (scheme)
#' op <- par(mfrow = c(6, 1), oma = c(0, 0, 0, 0))
#' GetTolColors( 11, scheme = "sunset", plot = TRUE)
#' GetTolColors(255, scheme = "sunset", plot = TRUE)
#' GetTolColors(  9, scheme = "BuRd",   plot = TRUE)
#' GetTolColors(255, scheme = "BuRd",   plot = TRUE)
#' GetTolColors(  9, scheme = "PRGn",   plot = TRUE)
#' GetTolColors(255, scheme = "PRGn",   plot = TRUE)
#' par(op)
#'
#' # Sequential color schemes (scheme)
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' GetTolColors(  9, scheme = "YlOrBr",           plot = TRUE)
#' GetTolColors(255, scheme = "YlOrBr",           plot = TRUE)
#' GetTolColors( 23, scheme = "discrete rainbow", plot = TRUE)
#' GetTolColors( 34, scheme = "smooth rainbow",   plot = TRUE)
#' GetTolColors(255, scheme = "smooth rainbow",   plot = TRUE)
#' par(op)
#'
#' # Cover color scheme (scheme)
#' op <- par(oma = c(1, 0, 0, 0), cex = 0.7)
#' GetTolColors(14, scheme = "ground cover", plot = TRUE)
#' par(op)
#'
#' # Alpha transparency (alpha)
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' GetTolColors(34, alpha = 1.0, plot = TRUE)
#' GetTolColors(34, alpha = 0.8, plot = TRUE)
#' GetTolColors(34, alpha = 0.6, plot = TRUE)
#' GetTolColors(34, alpha = 0.4, plot = TRUE)
#' GetTolColors(34, alpha = 0.2, plot = TRUE)
#' par(op)
#'
#' # Color levels (start, end)
#' op <- par(mfrow = c(4, 1), oma = c(0, 0, 0, 0))
#' GetTolColors(255, start = 0.0, end = 1.0, plot = TRUE)
#' GetTolColors(255, start = 0.0, end = 0.5, plot = TRUE)
#' GetTolColors(255, start = 0.5, end = 1.0, plot = TRUE)
#' GetTolColors(255, start = 0.2, end = 0.8, plot = TRUE)
#' par(op)
#'
#' # Interpolation bias (bias)
#' op <- par(mfrow = c(7, 1), oma = c(0, 0, 0, 0))
#' GetTolColors(255, bias = 0.4, plot = TRUE)
#' GetTolColors(255, bias = 0.6, plot = TRUE)
#' GetTolColors(255, bias = 0.8, plot = TRUE)
#' GetTolColors(255, bias = 1.0, plot = TRUE)
#' GetTolColors(255, bias = 1.2, plot = TRUE)
#' GetTolColors(255, bias = 1.4, plot = TRUE)
#' GetTolColors(255, bias = 1.6, plot = TRUE)
#' par(op)
#'
#' # Reverse colors (reverse)
#' op <- par(mfrow = c(2, 1), oma = c(0, 0, 0, 0), cex = 0.7)
#' GetTolColors(10, reverse = FALSE, plot = TRUE)
#' GetTolColors(10, reverse = TRUE,  plot = TRUE)
#' par(op)
#'

GetTolColors <- function(n, scheme="smooth rainbow",
                         alpha=NULL, start=0, end=1, bias=1, reverse=FALSE,
                         plot=FALSE) {

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
  checkmate::qassert(bias, "N1(0,)")
  checkmate::assertFlag(reverse)
  checkmate::assertFlag(plot)

  if (nmax[scheme] < Inf && (start > 0 | end < 1))
    warning("'start' and 'end' apply only to interpolated color schemes")

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
    col <- grDevices::colorRampPalette(pal[idxs], bias=bias)(n)
    names(col) <- seq_along(col)
  }

  if (reverse) col <- rev(col)

  labels <- names(col)
  if (!is.null(alpha)) {
    col <- grDevices::adjustcolor(col, alpha.f=alpha); names(col) <- labels
    if (!is.null(bad)) bad <- grDevices::adjustcolor(bad, alpha.f=alpha)
  }

  attr(col, "bad") <- bad

  if (plot) {
    txt <- c(paste0("n = ", n),
             paste0("alpha = ", alpha),
             paste0("start = ", start, ", end = ", end),
             paste0("bias = ", bias),
             paste0("reverse = ", reverse))
    is <- c(TRUE, !is.null(alpha), start > 0 | end < 1, bias != 1, reverse)
    main <- sprintf("%s (%s)", scheme, paste(txt[is], collapse=", "))
    op <- graphics::par(mar = c(3, 2, 2, 2)); on.exit(graphics::par(op))
    graphics::plot.default(NA, type="n", xlim=c(0, 1), ylim=c(0, 1), main=main,
                           xaxs="i", yaxs="i", bty="n", xaxt="n", yaxt="n",
                           xlab="", ylab="", col.main="#333333")
    if (n > 50) {  # criterion for showing tick labels
      border <- NA
      labels <- FALSE
    } else {
      border <- "#D3D3D3"
      labels <- gsub(" ", "\n", labels)
    }
    graphics::rect(0:(n - 1) / n, 0, 1:n / n, 1, col=col, border=border, lwd=0.5)
    graphics::axis(1, at=0:(n - 1) / n + 1 / (2 * n), labels=labels,
                   tick=FALSE, line=-0.5, padj=1, mgp=c(3, 0, 0), col.lab="#333333")
    graphics::box(lwd=0.5, col="#D3D3D3")
  }

  return(col)
}
