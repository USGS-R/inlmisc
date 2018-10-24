#' Get Palette Colors
#'
#' Create a vector of \code{n} colors from qualitative, diverging, and sequential color schemes.
#'
#' @param n 'integer' count.
#'   Number of colors to be in the palette.
#'   The maximum number of colors in a generated palette is dependent on the specified color scheme,
#'   see \sQuote{Details} section for maximum values.
#' @param scheme 'character' string.
#'   Name of color scheme, see \sQuote{Details} section for scheme descriptions.
#'   Partial string matching is supported so argument may be abbreviated.
#' @param alpha 'numeric' number.
#'   Alpha transparency, values range from 0 (fully transparent) to 1 (fully opaque).
#'   Specify as \code{NULL} to exclude the alpha channel value from colors.
#' @param start,end 'numeric' number.
#'   Starting and ending color level in the palette, respectively.
#'   Specified as a number in the interval from 0 to 1.
#'   Applies only to interpolated color schemes.
#' @param bias 'numeric' number.
#'   Interpolation bias where larger values result in more widely spaced colors at the high end.
#'   See \code{\link[grDevices]{colorRamp}} function for details.
#' @param reverse 'logical' flag.
#'   Whether to reverse the color order in the palette.
#' @param blind 'character' string.
#'   Type of color blindness to simulate: specify \code{"deutan"} for green-blind vision,
#'   \code{"protan"} for red-blind vision, \code{"tritan"} for green-blue-blind vision, or
#'   \code{"monochrome"} for total-color blindness.
#'   A partial-color blindness simulation requires that the \pkg{dichromat} package is available,
#'   see \code{\link[dichromat]{dichromat}} function for additional information.
#'   Partial string matching is supported so argument may be abbreviated.
#' @param gray 'logical' flag.
#'   Whether to subset/reorder the \code{"bright"}, \code{"high-contrast"}, \code{"vibrant"},
#'   and \code{"muted"} schemes to work well after conversion to gray scale.
#'
#' @details The suggested data type for color schemes and the characteristics of generated palettes are given in the table below.
#'   [\bold{Type}: is the type of data being represented, either qualitative, diverging, or sequential.
#'   \bold{Max n}: is the maximum number of colors in a generated palette.
#'   And the maximum \code{n} value when palette colors are designed for gray-scale conversion is enclosed in parentheses.
#'   \bold{N}: not-a-number color.
#'   \bold{B}: background color.
#'   \bold{F}: foreground color.
#'   \bold{Abbreviations}: --, not available]
#'
#'   \if{html}{\figure{table.svg}{options: width=460 alt="Table: schemes"}}
#'   \if{latex}{\figure{table.pdf}{options: width=12.17cm}}
#'
#'   Schemes \code{"pale"}, \code{"dark"}, and \code{"ground cover"} are
#'   intended to be accessed in their entirety and subset using vector element names.
#'
#' @return When argument \code{n} is specified the function
#'   returns an object of class 'inlcol' that inherits behavior from the 'character' class.
#'   And when \code{n} is unspecified a variant of the \code{GetColors} function is
#'   returned that has default (formal) argument values set equal to the values specified by the user.
#'
#'   The inlcol-class object is comprised of a 'character' vector of \code{n} colors in the RGB color system.
#'   Colors are specified with a string of the form \code{"#RRGGBB"} or \code{"#RRGGBBAA"}
#'   where \code{RR}, \code{GG}, \code{BB}, and \code{AA} are the
#'   red, green, blue, and alpha hexadecimal values (00 to FF), respectively.
#'   Attributes of the returned object include:
#'   \code{"names"}, the informal names assigned to colors in the palette,
#'   where \code{NULL} indicates no color names are specified;
#'   \code{"NaN"}, a 'character' string giving the color meant for missing data, in hexadecimal format,
#'   where \code{NA} indicates no color is specified; and
#'   \code{"call"}, an object of class '\link{call}' giving the unevaluated function call (expression)
#'   that can be used to reproduce the color palette.
#'   Use the \code{\link{eval}} function to evaluate the \code{"call"} argument.
#'   A simple \code{plot} method is provided for the 'inlcol' class that
#'   shows a palette of colors using a sequence of shaded rectangles,
#'   see \sQuote{Examples} section for usage.
#'
#' @note The sequential color schemes \code{"YlOrBr"} and \code{"iridescent"}
#'   work well for conversion to gray scale.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @references
#'   Dewez, Thomas, 2004, Variations on a DEM palette, accessed October 15, 2018 at
#'   \url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/td/index.html}
#'
#'   Tol, Paul, 2018, Colour Schemes:
#'   SRON Technical Note, doc. no. SRON/EPS/TN/09-002, issue 3.1, 20 p.,
#'   accessed September 24, 2018 at \url{https://personal.sron.nl/~pault/data/colourschemes.pdf}.
#'
#'   Wessel, P., Smith, W.H.F., Scharroo, R., Luis, J.F., and Wobbe, R., 2013,
#'   Generic Mapping Tools: Improved version released, AGU, v. 94, no. 45, p. 409--410
#'   doi:\href{https://doi.org/10.1002/2013EO450001}{10.1002/2013EO450001}
#'
#' @seealso \code{\link[grDevices]{col2rgb}}
#'
#' @keywords color
#'
#' @export
#'
#' @examples
#' pal <- GetColors(n = 10)
#' print(pal)
#' plot(pal)
#'
#' Fun <- GetColors(scheme = "DEM screen", alpha = 0.9)
#' filled.contour(datasets::volcano, color.palette = Fun)
#'
#' # Diverging color schemes (scheme)
#' op <- par(mfrow = c(6, 1), oma = c(0, 0, 0, 0))
#' plot(GetColors(  9, scheme = "BuRd"))
#' plot(GetColors(255, scheme = "BuRd"))
#' plot(GetColors(  9, scheme = "PRGn"))
#' plot(GetColors(255, scheme = "PRGn"))
#' plot(GetColors( 11, scheme = "sunset"))
#' plot(GetColors(255, scheme = "sunset"))
#' par(op)
#'
#' # Qualitative color schemes (scheme)
#' op <- par(mfrow = c(7, 1), oma = c(0, 0, 0, 0))
#' plot(GetColors(7, scheme = "bright"))
#' plot(GetColors(6, scheme = "dark"))
#' plot(GetColors(5, scheme = "high-contrast"))
#' plot(GetColors(9, scheme = "light"))
#' plot(GetColors(9, scheme = "muted"))
#' plot(GetColors(6, scheme = "pale"))
#' plot(GetColors(7, scheme = "vibrant"))
#' par(op)
#'
#' # Sequential color schemes (scheme)
#' op <- par(mfrow = c(7, 1), oma = c(0, 0, 0, 0))
#' plot(GetColors( 23, scheme = "discrete rainbow"))
#' plot(GetColors( 34, scheme = "smooth rainbow"))
#' plot(GetColors(255, scheme = "smooth rainbow"))
#' plot(GetColors(  9, scheme = "YlOrBr"))
#' plot(GetColors(255, scheme = "YlOrBr"))
#' plot(GetColors( 23, scheme = "iridescent"))
#' plot(GetColors(255, scheme = "iridescent"))
#' par(op)
#'
#' # Alpha transparency (alpha)
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' plot(GetColors(34, alpha = 1.0))
#' plot(GetColors(34, alpha = 0.8))
#' plot(GetColors(34, alpha = 0.6))
#' plot(GetColors(34, alpha = 0.4))
#' plot(GetColors(34, alpha = 0.2))
#' par(op)
#'
#' # Color levels (start, end)
#' op <- par(mfrow = c(4, 1), oma = c(0, 0, 0, 0))
#' plot(GetColors(255, start = 0.0, end = 1.0))
#' plot(GetColors(255, start = 0.0, end = 0.5))
#' plot(GetColors(255, start = 0.5, end = 1.0))
#' plot(GetColors(255, start = 0.3, end = 0.9))
#' par(op)
#'
#' # Interpolation bias (bias)
#' op <- par(mfrow = c(7, 1), oma = c(0, 0, 0, 0))
#' plot(GetColors(255, bias = 0.4))
#' plot(GetColors(255, bias = 0.6))
#' plot(GetColors(255, bias = 0.8))
#' plot(GetColors(255, bias = 1.0))
#' plot(GetColors(255, bias = 1.2))
#' plot(GetColors(255, bias = 1.4))
#' plot(GetColors(255, bias = 1.6))
#' par(op)
#'
#' # Reverse colors (reverse)
#' op <- par(mfrow = c(4, 1), oma = c(0, 0, 0, 0), cex = 0.7)
#' plot(GetColors(10, reverse = FALSE))
#' plot(GetColors(10, reverse = TRUE))
#' plot(GetColors(10, reverse = FALSE, start = 0.5))
#' plot(GetColors(10, reverse = TRUE,  start = 0.5))
#' par(op)
#'
#' # Color blindness (blind)
#' op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
#' plot(GetColors(34, blind = NULL))
#' plot(GetColors(34, blind = "deutan"))
#' plot(GetColors(34, blind = "protan"))
#' plot(GetColors(34, blind = "tritan"))
#' plot(GetColors(34, blind = "monochrome"))
#' par(op)
#'
#' # Gray-scale preparation (gray)
#' op <- par(mfrow = c(8, 1), oma = c(0, 0, 0, 0))
#' plot(GetColors(3, "bright",        gray = TRUE))
#' plot(GetColors(3, "bright",        gray = TRUE, blind = "monochrome"))
#' plot(GetColors(5, "high-contrast", gray = TRUE))
#' plot(GetColors(5, "high-contrast", gray = TRUE, blind = "monochrome"))
#' plot(GetColors(4, "vibrant",       gray = TRUE))
#' plot(GetColors(4, "vibrant",       gray = TRUE, blind = "monochrome"))
#' plot(GetColors(5, "muted",         gray = TRUE))
#' plot(GetColors(5, "muted",         gray = TRUE, blind = "monochrome"))
#' par(op)
#'

GetColors <- function(n, scheme="smooth rainbow", alpha=NULL, start=0, end=1,
                      bias=1, reverse=FALSE, blind=NULL, gray=FALSE) {

  if (!missing(n)) {
    checkmate::assertCount(n)
    if (n == 0) return(NULL)
  }
  checkmate::assertFlag(gray)

  scheme <- match.arg(scheme, names(schemes))
  s <- schemes[[scheme]]
  nmax <- if(gray) length(s$gray) else s$nmax

  if (!missing(n) && n > nmax)
    stop("n = ", n, " exceeds the maximum number of colors in palette: ",
         nmax, " for '", scheme, "' scheme.")
  if (gray && nmax == 0)
    stop("gray component not available for '", scheme, "' scheme.")

  checkmate::assertNumber(alpha, lower=0, upper=1, finite=TRUE, null.ok=TRUE)
  checkmate::assertNumber(start, lower=0, upper=1, finite=TRUE)
  checkmate::assertNumber(end,   lower=0, upper=1, finite=TRUE)
  stopifnot(start < end)
  checkmate::qassert(bias, "N1(0,)")
  checkmate::assertFlag(reverse)
  checkmate::assertString(blind, min.chars=1, null.ok=TRUE)

  if (is.character(blind)) {
    if (blind == "monochromacy") blind <- "monochrome"  # backward compatibility
    blind <- match.arg(blind, c("deutan", "protan", "tritan", "monochrome"))
    if (blind != "monochrome" && !requireNamespace("dichromat", quietly=TRUE))
      stop("simulating partial color blindness requires the dichromat package")
  }

  if (nmax < Inf && (start > 0 | end < 1))
    warning("'start' and 'end' apply only to interpolated color schemes")

  if (missing(n)) {
    Pal <- GetColors
    formals(Pal) <- eval(substitute(
      alist("n"=, "scheme"=a1, "alpha"=a2, "start"=a3, "end"=a4,
            "bias"=a5, "reverse"=a6, "blind"=a7, "gray"=a8),
      list(a1=scheme, a2=alpha, a3=start, a4=end,
           a5=bias, a6=reverse, a7=blind, a8=gray)
    ))
    return(Pal)
  }

  color <- s$data$color; names(color) <- s$data$name
  if (gray) color <- color[s$gray]

  if (scheme == "discrete rainbow") {
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
    pal <- color[idx[[n]]]
    if (reverse) pal <- rev(pal)
  } else if (nmax < Inf) {
    if (reverse) color <- rev(color)
    pal <- color[1:n]
  } else {
    value <- s$data$value
    if (is.null(value)) value <- seq_along(s$data$color)
    value <- scales::rescale(value)
    if (reverse) {
      color <- rev(color)
      value <- rev(1 - value)
    }
    color <- scales::gradient_n_pal(color, values=value)(seq(start, end, length.out=255))
    pal <- grDevices::colorRampPalette(color, bias=bias, space="Lab")(n)
  }

  nan <- ifelse(is.null(s$nan), as.character(NA), s$nan)

  if (!is.null(blind) | !is.null(alpha)) {
    pal_names <- names(pal)
    if (!is.null(blind)) {
      if (blind == "monochrome") {
        pal <- .Col2Gray(pal)
        if (!is.null(nan)) nan <- .Col2Gray(nan)
      } else {
        pal <- dichromat::dichromat(pal, type=blind)
        if (!is.null(nan)) nan <- dichromat::dichromat(nan, type=blind)
      }
    }
    if (!is.null(alpha)) {
      pal <- grDevices::adjustcolor(pal, alpha.f=alpha)
      if (!is.null(nan)) nan <- grDevices::adjustcolor(nan, alpha.f=alpha)
    }
    names(pal) <- pal_names
  }

  cl <- as.call(list(quote(GetColors), "n"=n, "scheme"=scheme, "alpha"=alpha,
                     "start"=start, "end"=end, "bias"=bias, "reverse"=reverse,
                     "blind"=blind, "gray"=gray))

  .MakeInlcolClass(pal, nan, cl)
}

#' @export

# Plot function for 'inlcol' color palette

plot.inlcol <- function(x, ..., label=TRUE) {
  checkmate::assertCharacter(x, any.missing=FALSE, min.len=1)
  stopifnot(all(.IsColor(x)))
  checkmate::assertFlag(label)

  n <- length(x)

  if (label && inherits(x, "inlcol")) {
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
    reverse <- arg$reverse
  } else {
    main <- NULL
    reverse <- FALSE
  }

  if (label && n < 35) {  # cutoff criterion for drawing tick labels
    border <- "#D3D3D3"
    labels <- gsub(" ", "\n", names(x))
    if (length(labels) == 0) {
      labels <- seq_along(x)
      if (reverse) labels <- rev(labels)
    }
  } else {
    border <- NA
    labels <- FALSE
  }

  # code adapted from example in
  # colorspace::rainbow_hcl function documentation,
  # authored by Achim Zeileis and accessed August 8, 2018
  # at https://CRAN.R-project.org/package=colorspace
  mar <- if (label) c(3, 2, 2, 2) else c(0, 0, 0, 0)
  op <- graphics::par(mar=mar); on.exit(graphics::par(op))
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


# Constructor function for 'inlcol' class

.MakeInlcolClass <- function(x, nan, call) {
  pattern <- "^#(\\d|[a-f]){6,8}$"
  checkmate::assertCharacter(x, pattern=pattern, ignore.case=TRUE,
                             any.missing=FALSE, min.len=1)
  checkmate::assertString(nan, na.ok=TRUE, pattern=pattern, ignore.case=TRUE)
  stopifnot(is.call(call))
  stopifnot(all(names(formals(GetColors)) %in% names(as.list(call))))
  structure(x, nan=nan, call=call, class=append("inlcol", class(x)))
}


# Check for valid color names

.IsColor <- function(x, null.ok=FALSE) {
  if (is.null(x) && null.ok) return(TRUE)
  vapply(x, function(i) tryCatch({
    is.matrix(grDevices::col2rgb(i))
  }, error=function(e) FALSE), TRUE)
}
