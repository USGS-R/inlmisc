#' Paul Tol's Color Schemes
#'
#' This function creates a vector of \code{n} colors from
#' qualitative, diverging, and sequential color schemes by Paul Tol (2018).
#'
#' @param n 'integer'.
#'   Number of colors to be in the palette.
#'   The maximum number of colors is dependent on the specified color scheme,
#'   see \sQuote{Details} section for upper limits.
#' @param scheme 'character'.
#'   Color scheme name: specify
#'   \code{"bright"}, \code{"vibrant"}, \code{"muted"}, \code{"pale"},
#'   \code{"dark"}, \code{"light"}, or \code{"ground cover"} for a qualitative color scheme;
#'   \code{"sunset"}, \code{"BuRd"}, or \code{"PRGn"} for a diverging color scheme; and
#'   \code{"YlOrBr"}, \code{"discrete rainbow"}, or \code{"smooth rainbow"} (the default)
#'   for a sequential color scheme.
#'   Supports partial string matching so argument may be abbreviated.
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
#' @param blind 'character'.
#'   Type of color blindness to simulate: specify as \code{"deutan"} for green-blind vision,
#'   \code{"protan"} for red-blind vision, \code{"tritan"} for green-blue-blind vision, or
#'   \code{"monochromacy"} for total color blindness.
#'   The partial color blindness options require that the \pkg{dichromat} package is available,
#'   see \code{\link[dichromat]{dichromat}} function for additional information.
#'   Supports partial string matching so argument may be abbreviated.
#' @param gray 'logical'.
#'   Whether to subset the \code{"bright"}, \code{"vibrant"}, and \code{"muted"}
#'   color schemes to work after conversion to gray scale.
#'   Note that the sequential color scheme \code{"YlOrBr"} works well without subsetting.
#' @param fmt 'character'.
#'   Format for returned color names.
#'   Specify as \code{"hex"} (the default) to express color components in hexadecimal format,
#'   or \code{"rgb"} to express in decimal format, see \sQuote{Value} section for details.
#'   Supports partial string matching so argument may be abbreviated.
#' @param plot 'logical'.
#'   Whether to display the palette colors in the active graphics window.
#'
#' @details Upper limits on the number of discrete colors for a scheme are:
#'   \code{n = 6} for \code{"pale"} and \code{"dark"};
#'   \code{n = 8} for \code{"bright"} and \code{"vibrant"};
#'   \code{n = 10} for \code{"muted"}, \code{"light"}, \code{"YlOrBr"}, \code{"BuRd"}, and \code{"PRGn"};
#'   \code{n = 14} for \code{"ground cover"}; and
#'   \code{n = 24} for \code{"discrete rainbow"}.
#'   The exception to these upper limits occurs when the \code{gray} argument is true: in that case
#'   \code{n = 3} for \code{"bright"}, \code{n = 4} for \code{"vibrant"}, and \code{n = 5} for \code{"muted"}.
#'   Color schemes \code{"pale"},  \code{"dark"}, and \code{"ground cover"} are
#'   intended to be accessed in their entirety and subset using element vector names.
#'   The very specific \code{"ground cover"} color scheme is a color-blind safe version of the
#'   \href{http://glcf.umd.edu/data/landcover/data.shtml}{AVHRR}
#'   global land cover classification color scheme (Hansen and others, 1998).
#'
#' @return For \code{fmt = "hex"}, returns a 'character' vector of \code{n} color names in hexadecimal format.
#'   A hexadecimal color is specified with a string of the form \code{"#RRGGBB"} or \code{"#RRGGBBAA"}
#'   where \code{RR}, \code{GG}, \code{BB}, and \code{AA} are the
#'   red, green, blue, and alpha hexadecimal values (00 to FF), respectively.
#'   And for \code{fmt = "rgb"}, an integer 'matrix' of decimal values (0 to 255) is returned
#'   with \code{n} rows and three or four (when \code{alpha} is specified) columns:
#'   \code{red}, \code{green}, \code{blue}, and \code{alpha}.
#'   The returned object also includes a \code{"bad"} attribute giving
#'   the color name assigned to bad data---equal to \code{NA} if unspecified.
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
#'   accessed August 29, 2018 at \url{https://personal.sron.nl/~pault/data/colourschemes.pdf}.
#'
#' @keywords color
#'
#' @export
#'
#' @examples
#'
#' col <- GetTolColors(5); print(col)
#' GetTolColors(5, fmt = "rgb")
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
#' op <- par(oma = c(1, 0, 0, 0), cex = 0.7)
#' GetTolColors(14, scheme = "ground cover", plot = TRUE)
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
#' GetTolColors(255, start = 0.3, end = 0.9, plot = TRUE)
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
#' # Color blindness (blind)
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' GetTolColors(34, blind = NULL,           plot = TRUE)
#' GetTolColors(34, blind = "deutan",       plot = TRUE)
#' GetTolColors(34, blind = "protan",       plot = TRUE)
#' GetTolColors(34, blind = "tritan",       plot = TRUE)
#' GetTolColors(34, blind = "monochromacy", plot = TRUE)
#' par(op)
#'
#' # Gray-scale preparation (gray)
#' op <- par(mfrow = c(6, 1), oma = c(0, 0, 0, 0))
#' GetTolColors(3, "bright",  gray = TRUE, plot = TRUE)
#' GetTolColors(3, "bright",  gray = TRUE, plot = TRUE, blind = "m")
#' GetTolColors(4, "vibrant", gray = TRUE, plot = TRUE)
#' GetTolColors(4, "vibrant", gray = TRUE, plot = TRUE, blind = "m")
#' GetTolColors(5, "muted",   gray = TRUE, plot = TRUE)
#' GetTolColors(5, "muted",   gray = TRUE, plot = TRUE, blind = "m")
#' par(op)
#'

GetTolColors <- function(n, scheme="smooth rainbow", alpha=NULL, start=0, end=1,
                         bias=1, reverse=FALSE, blind=NULL, gray=FALSE,
                         fmt=c("hex", "rgb"), plot=FALSE) {

  checkmate::assertFlag(gray)
  nmax <- c("bright"           = ifelse(gray, 3, 7),  # qualitative
            "vibrant"          = ifelse(gray, 4, 7),
            "muted"            = ifelse(gray, 5, 9),
            "pale"             = 6,
            "dark"             = 6,
            "light"            = 9,
            "ground cover"     = 14,
            "sunset"           = Inf,                 # diverging
            "BuRd"             = Inf,
            "PRGn"             = Inf,
            "YlOrBr"           = Inf,                 # sequential
            "discrete rainbow" = 23,
            "smooth rainbow"   = Inf)
  schemes <- names(nmax)
  scheme <- match.arg(scheme, schemes)
  checkmate::assertInt(n, lower=1, upper=nmax[scheme])
  checkmate::assertNumber(alpha, lower=0, upper=1, finite=TRUE, null.ok=TRUE)
  checkmate::assertNumber(start, lower=0, upper=1, finite=TRUE)
  checkmate::assertNumber(end, lower=start, upper=1, finite=TRUE)
  checkmate::qassert(bias, "N1(0,)")
  checkmate::assertFlag(reverse)
  if (!is.null(blind)) {
    blind <- match.arg(blind, c("deutan", "protan", "tritan", "monochromacy"))
    if (blind != "monochromacy" && !requireNamespace("dichromat", quietly=TRUE))
      stop("simulating partial color blindness requires the dichromat package")
  }
  fmt <- match.arg(fmt)
  checkmate::assertFlag(plot)

  if (nmax[scheme] < Inf && (start > 0 | end < 1))
    warning("'start' and 'end' apply only to interpolated color schemes")

  bad <- as.character(NA)

  if (scheme == "bright") {
    pal <- c("blue"                        = "#4477AA",
             "red"                         = "#EE6677",
             "green"                       = "#228833",
             "yellow"                      = "#CCBB44",
             "cyan"                        = "#66CCEE",
             "purple"                      = "#AA3377",
             "grey"                        = "#BBBBBB")
    if (gray) pal <- pal[c("yellow", "red", "green")]
  } else if (scheme == "vibrant") {
    pal <- c("orange"                      = "#EE7733",
             "blue"                        = "#0077BB",
             "cyan"                        = "#33BBEE",
             "magenta"                     = "#EE3377",
             "red"                         = "#CC3311",
             "teal"                        = "#009988",
             "grey"                        = "#BBBBBB")
    if (gray) pal <- pal[c("grey", "orange", "magenta", "blue")]
  } else if (scheme == "muted") {
    pal <- c("rose"                        = "#CC6677",
             "indigo"                      = "#332288",
             "sand"                        = "#DDCC77",
             "green"                       = "#117733",
             "cyan"                        = "#88CCEE",
             "wine"                        = "#882255",
             "teal"                        = "#44AA99",
             "olive"                       = "#999933",
             "purple"                      = "#AA4499")
    bad <- c("pale grey"                   = "#DDDDDD")
    if (gray) pal <- pal[c("sand", "teal", "purple", "green", "indigo")]
  } else if (scheme == "pale") {
    pal <- c("pale blue"                   = "#BBCCEE",
             "pale cyan"                   = "#CCEEFF",
             "pale green"                  = "#CCDDAA",
             "pale yellow"                 = "#EEEEBB",
             "pale red"                    = "#FFCCCC",
             "pale grey"                   = "#DDDDDD")
  } else if (scheme == "dark") {
    pal <- c("dark blue"                   = "#222255",
             "dark cyan"                   = "#225555",
             "dark green"                  = "#225522",
             "dark yellow"                 = "#666633",
             "dark red"                    = "#663333",
             "dark grey"                   = "#555555")
  } else if (scheme == "light") {
    pal <- c("libht blue"                  = "#77AADD",
             "orange"                      = "#EE8866",
             "light yellow"                = "#EEDD88",
             "pink"                        = "#FFAABB",
             "light cyan"                  = "#99DDFF",
             "mint"                        = "#44BB99",
             "pear"                        = "#BBCC33",
             "olive"                       = "#AAAA00",
             "pale grey"                   = "#DDDDDD")
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
  } else if (scheme == "sunset") {
    pal <- c("1"                           = "#364B9A",
             "2"                           = "#4A7BB7",
             "3"                           = "#6EA6CD",
             "4"                           = "#98CAE1",
             "5"                           = "#C2E4EF",
             "6"                           = "#EAECCC",
             "7"                           = "#FEDA8B",
             "8"                           = "#FDB366",
             "9"                           = "#F67E4B",
             "10"                          = "#DD3D2D",
             "11"                          = "#A50026")
    bad <- c("1"                           = "#FFFFFF")
  } else if (scheme == "BuRd") {
    pal <- c("1"                           = "#2166AC",
             "2"                           = "#4393C3",
             "3"                           = "#92C5DE",
             "4"                           = "#D1E5F0",
             "5"                           = "#F7F7F7",
             "6"                           = "#FDDBC7",
             "7"                           = "#F4A582",
             "8"                           = "#D6604D",
             "9"                           = "#B2182B")
    bad <- c("1"                           = "#FFEE99")
  } else if (scheme == "PRGn") {
    pal <- c("1"                           = "#762A83",
             "2"                           = "#9970AB",
             "3"                           = "#C2A5CF",
             "4"                           = "#E7D4E8",
             "5"                           = "#F7F7F7",
             "6"                           = "#D9F0D3",
             "7"                           = "#ACD39E",
             "8"                           = "#5AAE61",
             "9"                           = "#1B7837")
    bad <- c("1"                           = "#FFEE99")
  } else if (scheme == "YlOrBr") {
    pal <- c("1"                           = "#FFFFE5",
             "2"                           = "#FFF7BC",
             "3"                           = "#FEE391",
             "4"                           = "#FEC44F",
             "5"                           = "#FB9A29",
             "6"                           = "#EC7014",
             "7"                           = "#CC4C02",
             "8"                           = "#993404",
             "9"                           = "#662506")
    bad <- c("1"                           = "#888888")
  } else if (scheme == "discrete rainbow") {
    pal <- c("1"                           = "#E8ECFB",
             "2"                           = "#D9CCE3",
             "3"                           = "#D1BBD7",
             "4"                           = "#CAACCB",
             "5"                           = "#BA8DB4",
             "6"                           = "#AE76A3",
             "7"                           = "#AA6F9E",
             "8"                           = "#994F88",
             "9"                           = "#882E72",
             "10"                          = "#1965B0",
             "11"                          = "#437DBF",
             "12"                          = "#5289C7",
             "13"                          = "#6195CF",
             "14"                          = "#7BAFDE",
             "15"                          = "#4EB265",
             "16"                          = "#90C987",
             "17"                          = "#CAE0AB",
             "18"                          = "#F7F056",
             "19"                          = "#F7CB45",
             "20"                          = "#F6C141",
             "21"                          = "#F4A736",
             "22"                          = "#F1932D",
             "23"                          = "#EE8026",
             "24"                          = "#E8601C",
             "25"                          = "#E65518",
             "26"                          = "#DC050C",
             "27"                          = "#A5170E",
             "28"                          = "#72190E",
             "29"                          = "#42150A")
    bad <- c("1"                           = "#777777")
  } else if (scheme == "smooth rainbow") {
    pal <- c("1"                           = "#E8ECFB",
             "2"                           = "#DDD8EF",
             "3"                           = "#D1C1E1",
             "4"                           = "#C3A8D1",
             "5"                           = "#B58FC2",
             "6"                           = "#A778B4",
             "7"                           = "#9B62A7",
             "8"                           = "#8C4E99",
             "9"                           = "#6F4C9B",
             "10"                          = "#6059A9",
             "11"                          = "#5568B8",
             "12"                          = "#4E79C5",
             "13"                          = "#4D8AC6",
             "14"                          = "#4E96BC",
             "15"                          = "#549EB3",
             "16"                          = "#59A5A9",
             "17"                          = "#60AB9E",
             "18"                          = "#69B190",
             "19"                          = "#77B77D",
             "20"                          = "#8CBC68",
             "21"                          = "#A6BE54",
             "22"                          = "#BEBC48",
             "23"                          = "#D1B541",
             "24"                          = "#DDAA3C",
             "25"                          = "#E49C39",
             "26"                          = "#E78C35",
             "27"                          = "#E67932",
             "28"                          = "#E4632D",
             "29"                          = "#DF4828",
             "30"                          = "#DA2222",
             "31"                          = "#B8221E",
             "32"                          = "#95211B",
             "33"                          = "#721E17",
             "34"                          = "#521A13")
    bad <- c("1"                           = "#666666")
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
    if (length(idxs) < 2) stop("problem with 'start' and (or) 'end' argument")
    col <- grDevices::colorRampPalette(pal[idxs], bias=bias)(n)
    names(col) <- seq_along(col)
  }

  if (reverse) col <- rev(col)

  if (!is.null(blind) | !is.null(alpha)) {
    col_names <- names(col)
    bad_names <- names(bad)
    if (!is.null(blind)) {
      if (blind == "monochromacy") {
        col <- .ColToGray(col)
        if (!is.na(bad)) bad <- .ColToGray(bad)
      } else {
        col <- dichromat::dichromat(col, type=blind)
        if (!is.na(bad)) bad <- dichromat::dichromat(bad, type=blind)
      }
    }
    if (!is.null(alpha)) {
      col <- grDevices::adjustcolor(col, alpha.f=alpha)
      if (!is.na(bad)) bad <- grDevices::adjustcolor(bad, alpha.f=alpha)
    }
    names(col) <- col_names
    names(bad) <- bad_names
  }

  # plot colors,
  # code adapted from example in colorspace::rainbow_hcl function documentation,
  # authored by Achim Zeileis and accessed August 8, 2018
  # at https://CRAN.R-project.org/package=colorspace
  if (plot) {
    txt <- c(paste0("n = ", n),
             paste0("scheme = '", scheme, "'"),
             paste0("alpha = ", alpha),
             paste0("start = ", start, ", end = ", end),
             paste0("bias = ", bias),
             paste0("reverse = ", reverse),
             paste0("blind = '", blind, "'"),
             paste0("gray = ", gray))
    is <- c(TRUE, TRUE, !is.null(alpha), start > 0 | end < 1,
            bias != 1, reverse, !is.null(blind), gray)
    main <- paste(txt[is], collapse=", ")
    op <- graphics::par(mar = c(3, 2, 2, 2)); on.exit(graphics::par(op))
    graphics::plot.default(NA, type="n", xlim=c(0, 1), ylim=c(0, 1), main=main,
                           xaxs="i", yaxs="i", bty="n", xaxt="n", yaxt="n",
                           xlab="", ylab="", col.main="#333333")
    if (n > 50) {  # arbitrary cutoff criterion for drawing tick labels
      border <- NA
      labels <- FALSE
    } else {
      border <- "#D3D3D3"
      labels <- gsub(" ", "\n", names(col))
    }
    graphics::rect(0:(n - 1) / n, 0, 1:n / n, 1, col=col, border=border, lwd=0.5)
    graphics::axis(1, at=0:(n - 1) / n + 1 / (2 * n), labels=labels, tick=FALSE,
                   line=-0.5, padj=1, mgp=c(3, 0, 0), col.lab="#333333")
    graphics::box(lwd=0.5, col="#D3D3D3")
    return(invisible(col))
  }

  if (fmt == "rgb") {
    col <- t(grDevices::col2rgb(col, alpha=!is.null(alpha)))
    if (!is.na(bad)) bad <- t(grDevices::col2rgb(bad, alpha=!is.null(alpha)))
  }

  attr(col, "bad") <- bad

  return(col)
}


# Convert colors to gray/grayscale,
# code from TeachingDemos::col2grey function,
# authored by Greg Snow and accessed August 29, 2018
# at https://CRAN.R-project.org/package=TeachingDemos
# and licensed under Artistic-2.0
# https://cran.r-project.org/web/licenses/Artistic-2.0
# Function integrated here without logical changes.

.ColToGray <- function(cols) {
  rgb <- grDevices::col2rgb(cols)
  gry <- rbind(c(0.3, 0.59, 0.11)) %*% rgb
  grDevices::rgb(gry, gry, gry, maxColorValue=255)
}
