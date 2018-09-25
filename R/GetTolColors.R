#' Paul Tol's Color Schemes
#'
#' This function creates a vector of \code{n} colors from
#' qualitative, diverging, and sequential color schemes by Paul Tol (2018).
#' All colors are defined in sRGB color space.
#'
#' @param n 'integer'.
#'   Number of colors to be in the palette.
#'   The maximum number of colors in a generated palette is dependent on the specified color scheme,
#'   see \sQuote{Details} section for maximum values.
#' @param scheme 'character'.
#'   Color scheme name: specify
#'   \code{"bright"}, \code{"high-contrast"}, \code{"vibrant"}, \code{"muted"}, \code{"pale"},
#'   \code{"dark"}, \code{"light"}, or \code{"ground cover"} for a qualitative color scheme;
#'   \code{"sunset"}, \code{"BuRd"}, or \code{"PRGn"} for a diverging color scheme; and
#'   \code{"YlOrBr"}, \code{"iridescent"}, \code{"discrete rainbow"},
#'   or \code{"smooth rainbow"} (the default) for a sequential color scheme.
#'   Partial string matching is supported so argument may be abbreviated.
#' @param alpha 'numeric'.
#'   Alpha transparency, values range from 0 (fully transparent) to 1 (fully opaque).
#'   Specify as \code{NULL} to exclude the alpha channel value from colors.
#' @param start,end 'numeric'.
#'   Starting and ending color level in the palette, respectively.
#'   Specified as a number in the interval from 0 to 1.
#'   Applies only to interpolated color schemes: \code{"sunset"}, \code{"BuRd"},
#'   \code{"PRGn"}, \code{"YlOrBr"}, \code{"iridescent"}, and \code{"smooth rainbow"}.
#' @param bias 'numeric'.
#'   Interpolation bias where larger values result in more widely spaced colors at the high end.
#'   See \code{\link[grDevices]{colorRamp}} function for details.
#' @param reverse 'logical'.
#'   Whether to reverse the color order in the palette.
#' @param blind 'character'.
#'   Type of color blindness to simulate: specify \code{"deutan"} for green-blind vision,
#'   \code{"protan"} for red-blind vision, \code{"tritan"} for green-blue-blind vision, or
#'   \code{"monochrome"} for total-color blindness.
#'   A partial-color blindness simulation requires that the \pkg{dichromat} package is available,
#'   see \code{\link[dichromat]{dichromat}} function for additional information.
#'   Partial string matching is supported so argument may be abbreviated.
#' @param gray 'logical'.
#'   Whether to subset/reorder the \code{"bright"}, \code{"high-contrast"}, \code{"vibrant"},
#'   and \code{"muted"} schemes to work well after conversion to gray scale.
#'
#' @details The maximum number of colors in a palette is:
#'   \code{n = 5} for \code{"high-contrast"};
#'   \code{n = 6} for \code{"pale"} and \code{"dark"};
#'   \code{n = 8} for \code{"bright"} and \code{"vibrant"};
#'   \code{n = 10} for \code{"muted"}, \code{"light"}, \code{"YlOrBr"}, \code{"BuRd"}, and \code{"PRGn"};
#'   \code{n = 14} for \code{"ground cover"}; and
#'   \code{n = 24} for \code{"discrete rainbow"}.
#'   For \code{"sunset"}, \code{"BuRd"}, \code{"PRGn"}, \code{"YlOrBr"}, \code{"iridescent"},
#'   and \code{"smooth rainbow"}, a continuous version of the scheme is available that
#'   has no limit placed on the number of colors in a palette.
#'   The exception to these limits occurs when the \code{gray} argument is true: in that case
#'   \code{n = 3} for \code{"bright"}, \code{n = 4} for \code{"vibrant"},
#'   and \code{n = 5} for \code{"muted"}.
#'   Color schemes \code{"pale"},  \code{"dark"}, and \code{"ground cover"} are
#'   intended to be accessed in their entirety and subset using vector element names.
#'   The very specific \code{"ground cover"} scheme is a color-blind safe version of the
#'   \href{http://glcf.umd.edu/data/landcover/data.shtml}{AVHRR}
#'   global land cover classification (Hansen and others, 1998).
#'
#' @return Returns an object of class 'Tol' that inherits behavior from the 'character' class.
#'   The object is comprised of a 'character' vector of \code{n} colors in the RGB color system.
#'   Colors are specified with a string of the form \code{"#RRGGBB"} or \code{"#RRGGBBAA"}
#'   where \code{RR}, \code{GG}, \code{BB}, and \code{AA} are the
#'   red, green, blue, and alpha hexadecimal values (00 to FF), respectively.
#'   Attributes of the returned object include:
#'   \code{"names"}, the informal names assigned to colors in the palette,
#'   where \code{NULL} indicates no color names are specified;
#'   \code{"bad"}, a 'character' string giving the color meant for bad data, in hexadecimal format,
#'   where \code{NA} indicates no bad color is specified; and
#'   \code{"call"}, an object of class '\link{call}' giving the unevaluated function call (expression)
#'   that can be used to reproduce the color palette.
#'   Use the \code{\link{eval}} function to evaluate the \code{"call"} argument.
#'   A simple \code{plot} method is provided for the 'Tol' class that
#'   shows a palette of colors using a sequence of shaded rectangles,
#'   see \sQuote{Examples} section for usage.
#'
#' @note The sequential color schemes \code{"YlOrBr"} and \code{"iridescent"}
#'   work well for conversion to gray scale.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link[grDevices]{col2rgb}}
#'
#' @references
#'   Hansen, M., DeFries, R., Townshend, J.R.G., and Sohlberg, R., 1998,
#'   UMD Global Land Cover Classification, 1 Kilometer, 1.0:
#'   Department of Geography, University of Maryland, College Park, Maryland, 1981-1994.
#'
#'   Tol, Paul, 2018, Colour Schemes:
#'   SRON Technical Note, doc. no. SRON/EPS/TN/09-002, issue 3.1, 20 p.,
#'   accessed September 24, 2018 at \url{https://personal.sron.nl/~pault/data/colourschemes.pdf}.
#'
#' @keywords color
#'
#' @export
#'
#' @examples
#'
#' cols <- GetTolColors(n = 10)
#' print(cols)
#' plot(cols)
#'
#' # Qualitative color schemes (scheme)
#' op <- par(mfrow = c(7, 1), oma = c(0, 0, 0, 0))
#' plot(GetTolColors(7, scheme = "bright"))
#' plot(GetTolColors(5, scheme = "high-contrast"))
#' plot(GetTolColors(7, scheme = "vibrant"))
#' plot(GetTolColors(9, scheme = "muted"))
#' plot(GetTolColors(6, scheme = "pale"))
#' plot(GetTolColors(6, scheme = "dark"))
#' plot(GetTolColors(9, scheme = "light"))
#' par(op)
#'
#' op <- par(oma = c(1, 0, 0, 0), cex = 0.7)
#' plot(GetTolColors(14, scheme = "ground cover"))
#' par(op)
#'
#' # Diverging color schemes (scheme)
#' op <- par(mfrow = c(6, 1), oma = c(0, 0, 0, 0))
#' plot(GetTolColors( 11, scheme = "sunset"))
#' plot(GetTolColors(255, scheme = "sunset"))
#' plot(GetTolColors(  9, scheme = "BuRd"))
#' plot(GetTolColors(255, scheme = "BuRd"))
#' plot(GetTolColors(  9, scheme = "PRGn"))
#' plot(GetTolColors(255, scheme = "PRGn"))
#' par(op)
#'
#' # Sequential color schemes (scheme)
#' op <- par(mfrow = c(7, 1), oma = c(0, 0, 0, 0))
#' plot(GetTolColors(  9, scheme = "YlOrBr"))
#' plot(GetTolColors(255, scheme = "YlOrBr"))
#' plot(GetTolColors( 23, scheme = "iridescent"))
#' plot(GetTolColors(255, scheme = "iridescent"))
#' plot(GetTolColors( 23, scheme = "discrete rainbow"))
#' plot(GetTolColors( 34, scheme = "smooth rainbow"))
#' plot(GetTolColors(255, scheme = "smooth rainbow"))
#' par(op)
#'
#' # Alpha transparency (alpha)
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' plot(GetTolColors(34, alpha = 1.0))
#' plot(GetTolColors(34, alpha = 0.8))
#' plot(GetTolColors(34, alpha = 0.6))
#' plot(GetTolColors(34, alpha = 0.4))
#' plot(GetTolColors(34, alpha = 0.2))
#' par(op)
#'
#' # Color levels (start, end)
#' op <- par(mfrow = c(4, 1), oma = c(0, 0, 0, 0))
#' plot(GetTolColors(255, start = 0.0, end = 1.0))
#' plot(GetTolColors(255, start = 0.0, end = 0.5))
#' plot(GetTolColors(255, start = 0.5, end = 1.0))
#' plot(GetTolColors(255, start = 0.3, end = 0.9))
#' par(op)
#'
#' # Interpolation bias (bias)
#' op <- par(mfrow = c(7, 1), oma = c(0, 0, 0, 0))
#' plot(GetTolColors(255, bias = 0.4))
#' plot(GetTolColors(255, bias = 0.6))
#' plot(GetTolColors(255, bias = 0.8))
#' plot(GetTolColors(255, bias = 1.0))
#' plot(GetTolColors(255, bias = 1.2))
#' plot(GetTolColors(255, bias = 1.4))
#' plot(GetTolColors(255, bias = 1.6))
#' par(op)
#'
#' # Reverse colors (reverse)
#' op <- par(mfrow = c(2, 1), oma = c(0, 0, 0, 0), cex = 0.7)
#' plot(GetTolColors(10, reverse = FALSE))
#' plot(GetTolColors(10, reverse = TRUE))
#' par(op)
#'
#' # Color blindness (blind)
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' plot(GetTolColors(34, blind = NULL))
#' plot(GetTolColors(34, blind = "deutan"))
#' plot(GetTolColors(34, blind = "protan"))
#' plot(GetTolColors(34, blind = "tritan"))
#' plot(GetTolColors(34, blind = "monochrome"))
#' par(op)
#'
#' # Gray-scale preparation (gray)
#' op <- par(mfrow = c(8, 1), oma = c(0, 0, 0, 0))
#' plot(GetTolColors(3, "bright",        gray = TRUE))
#' plot(GetTolColors(3, "bright",        gray = TRUE, blind = "monochrome"))
#' plot(GetTolColors(5, "high-contrast", gray = TRUE))
#' plot(GetTolColors(5, "high-contrast", gray = TRUE, blind = "monochrome"))
#' plot(GetTolColors(4, "vibrant",       gray = TRUE))
#' plot(GetTolColors(4, "vibrant",       gray = TRUE, blind = "monochrome"))
#' plot(GetTolColors(5, "muted",         gray = TRUE))
#' plot(GetTolColors(5, "muted",         gray = TRUE, blind = "monochrome"))
#' par(op)
#'

GetTolColors <- function(n, scheme="smooth rainbow", alpha=NULL, start=0, end=1,
                         bias=1, reverse=FALSE, blind=NULL, gray=FALSE) {

  checkmate::assertFlag(gray)
  nmax <- c("bright"           = ifelse(gray, 3, 7),  # qualitative
            "vibrant"          = ifelse(gray, 4, 7),
            "high-contrast"    = 5,
            "muted"            = ifelse(gray, 5, 9),
            "pale"             = 6,
            "dark"             = 6,
            "light"            = 9,
            "ground cover"     = 14,
            "sunset"           = Inf,                 # diverging
            "BuRd"             = Inf,
            "PRGn"             = Inf,
            "YlOrBr"           = Inf,                 # sequential
            "iridescent"       = Inf,
            "discrete rainbow" = 23,
            "smooth rainbow"   = Inf)
  schemes <- names(nmax)
  scheme <- match.arg(scheme, schemes)
  checkmate::assertInt(n, lower=1, upper=nmax[scheme])
  checkmate::assertNumber(alpha, lower=0, upper=1, finite=TRUE, null.ok=TRUE)
  checkmate::assertNumber(start, lower=0, upper=1, finite=TRUE)
  checkmate::assertNumber(end, lower=start, upper=1, finite=TRUE)
  checkmate::qassert(bias, "N1(0,)")
  checkmate::assertString(blind, min.chars=1, null.ok=TRUE)
  checkmate::assertFlag(reverse)

  if (is.character(blind)) {
    if (blind == "monochromacy") blind <- "monochrome"  # backward compatibility
    blind <- match.arg(blind, c("deutan", "protan", "tritan", "monochrome"))
    if (blind != "monochrome" && !requireNamespace("dichromat", quietly=TRUE))
      stop("simulating partial color blindness requires the dichromat package")
  }

  if (nmax[scheme] < Inf && (start > 0 | end < 1))
    warning("'start' and 'end' apply only to interpolated color schemes")

  bad <- as.character(NA)

  if (scheme == "bright") {
    pal <- c("blue"   = "#4477AA",
             "red"    = "#EE6677",
             "green"  = "#228833",
             "yellow" = "#CCBB44",
             "cyan"   = "#66CCEE",
             "purple" = "#AA3377",
             "grey"   = "#BBBBBB")
    if (gray) pal <- pal[c("yellow", "red", "green")]
  } else if (scheme == "high-contrast") {
    pal <- c("blue"   = "#004488",
             "yellow" = "#DDAA33",
             "red"    = "#BB5566",
             "white"  = "#FFFFFF",
             "black"  = "#000000")
    if (gray) pal <- pal[c("white", "yellow", "red", "blue", "black")]
  } else if (scheme == "vibrant") {
    pal <- c("orange"  = "#EE7733",
             "blue"    = "#0077BB",
             "cyan"    = "#33BBEE",
             "magenta" = "#EE3377",
             "red"     = "#CC3311",
             "teal"    = "#009988",
             "grey"    = "#BBBBBB")
    if (gray) pal <- pal[c("grey", "orange", "magenta", "blue")]
  } else if (scheme == "muted") {
    pal <- c("rose"      = "#CC6677",
             "indigo"    = "#332288",
             "sand"      = "#DDCC77",
             "green"     = "#117733",
             "cyan"      = "#88CCEE",
             "wine"      = "#882255",
             "teal"      = "#44AA99",
             "olive"     = "#999933",
             "purple"    = "#AA4499")
    bad <- c("pale grey" = "#DDDDDD")
    if (gray) pal <- pal[c("sand", "teal", "purple", "green", "indigo")]
  } else if (scheme == "pale") {
    pal <- c("pale blue"   = "#BBCCEE",
             "pale cyan"   = "#CCEEFF",
             "pale green"  = "#CCDDAA",
             "pale yellow" = "#EEEEBB",
             "pale red"    = "#FFCCCC",
             "pale grey"   = "#DDDDDD")
  } else if (scheme == "dark") {
    pal <- c("dark blue"   = "#222255",
             "dark cyan"   = "#225555",
             "dark green"  = "#225522",
             "dark yellow" = "#666633",
             "dark red"    = "#663333",
             "dark grey"   = "#555555")
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
    pal <- c("#364B9A",
             "#4A7BB7",
             "#6EA6CD",
             "#98CAE1",
             "#C2E4EF",
             "#EAECCC",
             "#FEDA8B",
             "#FDB366",
             "#F67E4B",
             "#DD3D2D",
             "#A50026")
    bad <- "#FFFFFF"
  } else if (scheme == "BuRd") {
    pal <- c("#2166AC",
             "#4393C3",
             "#92C5DE",
             "#D1E5F0",
             "#F7F7F7",
             "#FDDBC7",
             "#F4A582",
             "#D6604D",
             "#B2182B")
    bad <- "#FFEE99"
  } else if (scheme == "PRGn") {
    pal <- c("#762A83",
             "#9970AB",
             "#C2A5CF",
             "#E7D4E8",
             "#F7F7F7",
             "#D9F0D3",
             "#ACD39E",
             "#5AAE61",
             "#1B7837")
    bad <- "#FFEE99"
  } else if (scheme == "YlOrBr") {
    pal <- c("#FFFFE5",
             "#FFF7BC",
             "#FEE391",
             "#FEC44F",
             "#FB9A29",
             "#EC7014",
             "#CC4C02",
             "#993404",
             "#662506")
    bad <- "#888888"
  } else if (scheme == "iridescent") {
    pal <- c("#FEFBE9",
             "#FCF7D5",
             "#F5F3C1",
             "#EAF0B5",
             "#DDECBF",
             "#D0E7CA",
             "#C2E3D2",
             "#B5DDD8",
             "#A8D8DC",
             "#9BD2E1",
             "#8DCBE4",
             "#81C4E7",
             "#7BBCE7",
             "#7EB2E4",
             "#88A5DD",
             "#9398D2",
             "#9B8AC4",
             "#9D7DB2",
             "#9A709E",
             "#906388",
             "#805770",
             "#684957",
             "#46353A")
    bad <- "#999999"
  } else if (scheme == "discrete rainbow") {
    pal <- c("1"  = "#E8ECFB",
             "2"  = "#D9CCE3",
             "3"  = "#D1BBD7",
             "4"  = "#CAACCB",
             "5"  = "#BA8DB4",
             "6"  = "#AE76A3",
             "7"  = "#AA6F9E",
             "8"  = "#994F88",
             "9"  = "#882E72",
             "10" = "#1965B0",
             "11" = "#437DBF",
             "12" = "#5289C7",
             "13" = "#6195CF",
             "14" = "#7BAFDE",
             "15" = "#4EB265",
             "16" = "#90C987",
             "17" = "#CAE0AB",
             "18" = "#F7F056",
             "19" = "#F7CB45",
             "20" = "#F6C141",
             "21" = "#F4A736",
             "22" = "#F1932D",
             "23" = "#EE8026",
             "24" = "#E8601C",
             "25" = "#E65518",
             "26" = "#DC050C",
             "27" = "#A5170E",
             "28" = "#72190E",
             "29" = "#42150A")
    bad <- "#777777"
  } else if (scheme == "smooth rainbow") {
    pal <- c("#E8ECFB",
             "#DDD8EF",
             "#D1C1E1",
             "#C3A8D1",
             "#B58FC2",
             "#A778B4",
             "#9B62A7",
             "#8C4E99",
             "#6F4C9B",
             "#6059A9",
             "#5568B8",
             "#4E79C5",
             "#4D8AC6",
             "#4E96BC",
             "#549EB3",
             "#59A5A9",
             "#60AB9E",
             "#69B190",
             "#77B77D",
             "#8CBC68",
             "#A6BE54",
             "#BEBC48",
             "#D1B541",
             "#DDAA3C",
             "#E49C39",
             "#E78C35",
             "#E67932",
             "#E4632D",
             "#DF4828",
             "#DA2222",
             "#B8221E",
             "#95211B",
             "#721E17",
             "#521A13")
    bad <- "#666666"
  }

  if (scheme %in% c("bright", "high-contrast", "vibrant", "muted",
                    "pale", "dark", "light", "ground cover")) {
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
  }

  if (reverse) col <- rev(col)

  if (!is.null(blind) | !is.null(alpha)) {
    col_names <- names(col)
    bad_names <- names(bad)
    if (!is.null(blind)) {
      if (blind == "monochrome") {
        col <- .Col2Gray(col)
        if (!is.na(bad)) bad <- .Col2Gray(bad)
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

  cl <- as.call(list(quote(GetTolColors), "n"=n, "scheme"=scheme, "alpha"=alpha,
                     "start"=start, "end"=end, "bias"=bias, "reverse"=reverse,
                     "blind"=blind, "gray"=gray))

  return(.MakeTolClass(col, bad, cl))
}

#' @export

# Plot function for 'Tol' color palette

plot.Tol <- function(x, ...) {
  checkmate::assertClass(x, c("Tol", "character"), ordered=TRUE)

  n <- length(x)
  arg <- as.list(attr(x, "call"))

  txt <- c(paste0("n = ", n),
           paste0("scheme = '", arg$scheme, "'"),
           paste0("alpha = ", arg$alpha),
           paste0("start = ", arg$start, ", end = ", arg$end),
           paste0("bias = ", arg$bias),
           paste0("reverse = ", arg$reverse),
           paste0("blind = '", arg$blind, "'"),
           paste0("gray = ", arg$gray))
  is <- c(TRUE, TRUE, !is.null(arg$alpha), arg$start > 0 | arg$end < 1,
          arg$bias != 1, arg$reverse, !is.null(arg$blind), arg$gray)
  main <- paste(txt[is], collapse=", ")

  if (n > 34) {  # cutoff criterion for drawing tick labels
    border <- NA
    labels <- FALSE
  } else {
    border <- "#D3D3D3"
    labels <- gsub(" ", "\n", names(x))
    if (length(labels) == 0) {
      labels <- seq_along(x)
      if (arg$reverse) labels <- rev(labels)
    }
  }

  # code adapted from example in
  # colorspace::rainbow_hcl} function documentation,
  # authored by Achim Zeileis and accessed August 8, 2018
  # at https://CRAN.R-project.org/package=colorspace
  op <- graphics::par(mar=c(3, 2, 2, 2)); on.exit(graphics::par(op))
  graphics::plot.default(NA, type="n", xlim=c(0, 1), ylim=c(0, 1), main=main,
                         xaxs="i", yaxs="i", bty="n", xaxt="n", yaxt="n",
                         xlab="", ylab="", col.main="#333333", ...)
  graphics::rect(0:(n - 1) / n, 0, 1:n / n, 1, col=x, border=border, lwd=0.5)
  graphics::axis(1, at=0:(n - 1) / n + 1 / (2 * n), labels=labels, tick=FALSE,
                 line=-0.5, padj=1, mgp=c(3, 0, 0), col.lab="#333333")
  graphics::box(lwd=0.5, col="#D3D3D3")

  invisible()
}


# Convert colors to gray/grayscale,
# code from TeachingDemos::col2grey function,
# authored by Greg Snow and accessed August 29, 2018
# at https://CRAN.R-project.org/package=TeachingDemos
# and licensed under Artistic-2.0
# https://cran.r-project.org/web/licenses/Artistic-2.0
# Function integrated here without logical changes.

.Col2Gray <- function(cols) {
  rgb <- grDevices::col2rgb(cols)
  gry <- rbind(c(0.3, 0.59, 0.11)) %*% rgb
  grDevices::rgb(gry, gry, gry, maxColorValue=255)
}


# Constructor function for 'Tol' class

.MakeTolClass <- function(x, bad, call) {
  pattern <- "^#(\\d|[a-f]){6,8}$"
  checkmate::assertCharacter(x, pattern=pattern, ignore.case=TRUE,
                             any.missing=FALSE, min.len=1)
  checkmate::assertString(bad, na.ok=TRUE, pattern=pattern, ignore.case=TRUE)
  stopifnot(is.call(call))
  stopifnot(all(names(formals(GetTolColors)) %in% names(as.list(call))))
  structure(x, bad=bad, call=call, class=append("Tol", class(x)))
}

