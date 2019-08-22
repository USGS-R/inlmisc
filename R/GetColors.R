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
#'   Argument choices may be abbreviated as long as there is no ambiguity.
#' @param alpha 'numeric' number.
#'   Alpha transparency, values range from 0 (fully transparent) to 1 (fully opaque).
#'   Specify as \code{NULL} to exclude the alpha channel value from colors.
#' @param stops 'numeric' vector of length 2.
#'   Color stops defined by interval endpoints (between 0 and 1)
#'   and used to select a subset of the color palette.
#'   Only suitable for schemes that allow for color interpolations.
#' @param bias 'numeric' number.
#'   Interpolation bias where larger values result in more widely spaced colors at the high end.
#'   See \code{\link[grDevices]{colorRamp}} function for details.
#' @param reverse 'logical' flag.
#'   Whether to reverse the order of colors in the scheme.
#' @param blind 'character' string.
#'   Type of color blindness to simulate: specify \code{"deutan"} for green-blind vision,
#'   \code{"protan"} for red-blind vision, \code{"tritan"} for green-blue-blind vision, or
#'   \code{"monochrome"} for total-color blindness.
#'   A partial-color blindness simulation requires that the \pkg{dichromat} package is available,
#'   see \code{\link[dichromat]{dichromat}} function for additional information.
#'   Argument choices may be abbreviated as long as there is no ambiguity.
#' @param gray 'logical' flag.
#'   Whether to subset/reorder the \code{"bright"}, \code{"high-contrast"}, \code{"vibrant"},
#'   and \code{"muted"} schemes to work well after conversion to gray scale.
#' @param ...
#'   Not used
#'
#' @details The suggested data type for color schemes and the
#'   characteristics of generated palettes are given in the tables below.
#'   [\bold{Type}: is the type of data being represented,
#'   either qualitative, diverging, or sequential.
#'   \bold{Max n}: is the maximum number of colors in a generated palette.
#'   And the maximum \code{n} value when scheme colors are designed for
#'   gray-scale conversion is enclosed in parentheses.
#'   A value of infinity indicates that the scheme allows for color interpolations.
#'   \bold{N}: is the not-a-number color.
#'   \bold{B}: is the background color.
#'   \bold{F}: is the foreground color.
#'   \bold{Abbreviations}: --, not available]
#'
#'   \if{html}{\figure{table01.svg}}
#'   \if{latex}{\figure{table01.pdf}{options: width=5.36in}}
#'
#'   \if{html}{\figure{table02.svg}}
#'   \if{latex}{\figure{table02.pdf}{options: width=5.36in}}
#'
#'   \if{html}{\figure{table03.svg}}
#'   \if{latex}{\figure{table03.pdf}{options: width=5.36in}}
#'
#'   \if{html}{\figure{table04.svg}}
#'   \if{latex}{\figure{table04.pdf}{options: width=5.36in}}
#'
#'   \if{html}{\figure{table05.svg}}
#'   \if{latex}{\figure{table05.pdf}{options: width=5.36in}}
#'
#'   Schemes \code{"pale"}, \code{"dark"}, and \code{"ground cover"} are
#'   intended to be accessed in their entirety and subset using vector element names.
#'
#' @return When argument \code{n} is specified the function
#'   returns an object of class 'inlpal' that inherits behavior from the 'character' class.
#'   And when \code{n} is unspecified a variant of the \code{GetColors} function is
#'   returned that has default argument values set equal to the values specified by the user.
#'
#'   The inlpal-class object is comprised of a 'character'
#'   vector of \code{n} colors in the RGB color system.
#'   Colors are specified with a string of the form \code{"#RRGGBB"} or \code{"#RRGGBBAA"}
#'   where \code{RR}, \code{GG}, \code{BB}, and \code{AA} are the
#'   red, green, blue, and alpha hexadecimal values (00 to FF), respectively.
#'   Attributes of the returned object include:
#'   \code{"names"}, the informal names assigned to colors in the palette,
#'   where \code{NULL} indicates no color names are specified;
#'   \code{"NaN"}, a 'character' string giving the color meant for missing data,
#'   in hexadecimal format, where \code{NA} indicates no color is specified; and
#'   \code{"call"}, an object of class '\link{call}' giving the unevaluated function
#'   call (expression) that can be used to reproduce the color palette.
#'   Use the \code{\link{eval}} function to evaluate the \code{"call"} argument.
#'   A simple \code{plot} method is provided for the 'inlpal' class that
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
#'   Mikhailov, Anton, 2019, Turbo, an improved rainbow colormap for visualization:
#'   Google AI Blog, accessed August 21, 2019 at
#'   \url{https://ai.googleblog.com/2019/08/turbo-improved-rainbow-colormap-for.html}.
#'
#'   Tol, Paul, 2018, Colour Schemes:
#'   SRON Technical Note, doc. no. SRON/EPS/TN/09-002, issue 3.1, 20 p.,
#'   accessed September 24, 2018 at \url{https://personal.sron.nl/~pault/data/colourschemes.pdf}.
#'
#'   Wessel, P., Smith, W.H.F., Scharroo, R., Luis, J.F., and Wobbe, R., 2013,
#'   Generic Mapping Tools: Improved version released, AGU, v. 94, no. 45, p. 409--410
#'   doi:\href{https://doi.org/10.1002/2013EO450001}{10.1002/2013EO450001}
#'
#' @seealso
#'   \code{\link{SetHinge}} function to set the hinge location in
#'   a color palette derived from one or two color schemes.
#'
#'   \code{\link[grDevices]{col2rgb}} function to express palette
#'   colors represented in the hexadecimal format as RGB triplets (R, G, B).
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
#' Pal <- GetColors(scheme = "turbo")
#' filled.contour(datasets::volcano, color.palette = Pal,
#'                plot.axes = FALSE)
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
#' # Color stops (stops)
#' op <- par(mfrow = c(4, 1), oma = c(0, 0, 0, 0))
#' plot(GetColors(255, stops = c(0.0, 1.0)))
#' plot(GetColors(255, stops = c(0.0, 0.5)))
#' plot(GetColors(255, stops = c(0.5, 1.0)))
#' plot(GetColors(255, stops = c(0.3, 0.9)))
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
#' op <- par(mfrow = c(2, 1), oma = c(0, 0, 0, 0),
#'           cex = 0.7)
#' plot(GetColors(10, reverse = FALSE))
#' plot(GetColors(10, reverse = TRUE))
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
#' plot(GetColors(3, "bright", gray = TRUE))
#' plot(GetColors(3, "bright", gray = TRUE,
#'                blind = "monochrome"))
#' plot(GetColors(5, "high-contrast", gray = TRUE))
#' plot(GetColors(5, "high-contrast", gray = TRUE,
#'                blind = "monochrome"))
#' plot(GetColors(4, "vibrant", gray = TRUE))
#' plot(GetColors(4, "vibrant", gray = TRUE,
#'                blind = "monochrome"))
#' plot(GetColors(5, "muted", gray = TRUE))
#' plot(GetColors(5, "muted", gray = TRUE,
#'                blind = "monochrome"))
#' par(op)
#'

GetColors <- function(n, scheme="smooth rainbow", alpha=NULL, stops=c(0, 1),
                      bias=1, reverse=FALSE, blind=NULL, gray=FALSE, ...) {

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

  # backward compatibility
  if (methods::hasArg("start")) stops[1] <- list(...)$start
  if (methods::hasArg("end"))   stops[2] <- list(...)$end

  checkmate::assertNumeric(stops, lower=0, upper=1, finite=TRUE, any.missing=FALSE,
                           len=2, unique=TRUE, sorted=TRUE)
  checkmate::qassert(bias, "N1(0,)")
  checkmate::assertFlag(reverse)
  checkmate::assertString(blind, min.chars=1, null.ok=TRUE)

  if (is.character(blind)) {
    if (blind == "monochromacy") blind <- "monochrome"  # backward compatibility
    blind <- match.arg(blind, c("deutan", "protan", "tritan", "monochrome"))
    if (blind != "monochrome" && !requireNamespace("dichromat", quietly=TRUE))
      stop("simulating partial color blindness requires the dichromat package")
  }

  if (nmax < Inf && !identical(stops, c(0, 1)))
    warning("'stops' only applies to interpolated color schemes")

  if (missing(n)) {
    Pal <- GetColors
    formals(Pal) <- eval(substitute(
      alist("n" =,
            "scheme"  = a1,
            "alpha"   = a2,
            "stops"   = a3,
            "bias"    = a4,
            "reverse" = a5,
            "blind"   = a6,
            "gray"    = a7),
      list("a1" = scheme,
           "a2" = alpha,
           "a3" = stops,
           "a4" = bias,
           "a5" = reverse,
           "a6" = blind,
           "a7" = gray)
    ))
    return(Pal)
  }

  color <- s$data$color; names(color) <- s$data$name
  if (gray) color <- color[s$gray]

  if (scheme == "turbo") {

    # code adapted from turbo colormap look-up table,
    # copyright 2019 Google LLC,
    # SPDX-license-identifier: Apache-2.0,
    # authored by Anton Mikhailov and accessed August 21, 2019
    # at https://gist.github.com/mikhailov-work/ee72ba4191942acecc03fe6da94fc73f
    xs <- seq.int(stops[1], stops[2], length.out=n)^bias
    Interpolate <- function(x) {
      x <- max(0, min(1, x))
      a <- floor(x * 255)
      b <- min(255, a + 1)
      f <- x * 255 - a
      a <- a + 1
      b <- b + 1
      c(turbo_rgb[a, 1] + (turbo_rgb[b, 1] - turbo_rgb[a, 1]) * f,
        turbo_rgb[a, 2] + (turbo_rgb[b, 2] - turbo_rgb[a, 2]) * f,
        turbo_rgb[a, 3] + (turbo_rgb[b, 3] - turbo_rgb[a, 3]) * f)
    }
    pal <- vapply(xs, function(x) {
      do.call(grDevices::rgb, as.list(Interpolate(x)))
    }, "")

    if (reverse) pal <- rev(pal)

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
    pal <- color[idx[[n]]]
    if (reverse) pal <- rev(pal)

  } else if (scheme == "bpy") {

    # code adapted from sp::bpy.colors function,
    # authored by Edzer Pebesma and accessed June 4, 2019
    # at https://CRAN.R-project.org/package=sp
    x <- seq.int(stops[1], stops[2], length.out=n)^bias
    r <- ifelse(x < 0.25, 0, ifelse(x < 0.57, x / 0.32 - 0.78125, 1))
    g <- ifelse(x < 0.42, 0, ifelse(x < 0.92, 2 * x - 0.84, 1))
    b <- ifelse(x < 0.25, 4 * x,
                ifelse(x < 0.42, 1, ifelse(x < 0.92, -2 * x + 1.84, x / 0.08 - 11.5)))
    pal <- grDevices::rgb(r, g, b)

    if (reverse) pal <- rev(pal)

  } else if (nmax < Inf) {
    if (reverse) color <- rev(color)
    pal <- color[1:n]

  } else {
    value <- if (is.null(s$data$value)) seq_along(s$data$color) else s$data$value
    value <- scales::rescale(value)
    if (reverse) {
      color <- rev(color)
      value <- rev(1 - value)
    }
    x <- seq.int(stops[1], stops[2], length.out=255)
    color <- scales::gradient_n_pal(color, values=value)(x)
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

  cl <- as.call(list(quote(GetColors),
                     "n"       = n,
                     "scheme"  = scheme,
                     "alpha"   = alpha,
                     "stops"   = stops,
                     "bias"    = bias,
                     "reverse" = reverse,
                     "blind"   = blind,
                     "gray"    = gray))

  .MakeInlpalClass(pal, nan, cl)
}

#' @export

# Plot function for 'inlpal' color palette

plot.inlpal <- function(x, ..., label=TRUE) {
  checkmate::assertCharacter(x, any.missing=FALSE, min.len=1)
  stopifnot(all(IsColor(x)))
  checkmate::assertFlag(label)

  n <- length(x)

  if (label && inherits(x, "inlpal")) {
    arg <- as.list(attr(x, "call"))
    txt <- c(paste0("n = ", n),
             paste0("scheme = '", arg$scheme, "'"),
             paste0("alpha = ", arg$alpha),
             paste0("stops = c(", arg$stops[1], ", ", arg$stops[2], ")"),
             paste0("bias = ", arg$bias),
             paste0("reverse = ", arg$reverse),
             paste0("blind = '", arg$blind, "'"),
             paste0("gray = ", arg$gray))
    is <- c(TRUE, TRUE, !is.null(arg$alpha), !identical(arg$stops, c(0, 1)),
            arg$bias != 1, arg$reverse, !is.null(arg$blind), arg$gray)
    main <- paste(txt[is], collapse=", ")
    reverse <- arg$reverse
  } else {
    main <- NULL
    reverse <- FALSE
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
  xl <- 0:(n - 1) / n
  xr <- 1:n / n
  if (any(grepl("^#(\\d|[a-f]){8}$", x, ignore.case=TRUE))) {
    graphics::rect(0, 0, 1, 1, col="#FFFFFF", border=NA)
  } else if (n > 1) {
    xr <- c(utils::head(xr, -1) + 1 / (2 * n), utils::tail(xr, 1))
  }
  graphics::rect(xl, 0, xr, 1, col=x, border=NA)
  if (label && n < 35) {
    at <- 0:(n - 1) / n + 1 / (2 * n)
    lab <- gsub(" ", "\n", names(x))
    if (length(lab) == 0) {
      lab <- seq_along(x)
      if (reverse) lab <- rev(lab)
    }
    graphics::axis(1, at=at, labels=lab, tick=FALSE, line=-0.5, padj=1,
                   mgp=c(3, 0, 0), col.lab="#333333")
    v <- (0:(n - 1) / n)[-1]
    graphics::abline(v=v, col="#D3D3D3", lwd=0.25)
  }
  graphics::box(lwd=0.25, col="#D3D3D3")

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


# Constructor function for 'inlpal' class

.MakeInlpalClass <- function(x, nan, call) {
  pattern <- "^#(\\d|[a-f]){6,8}$"
  checkmate::assertCharacter(x, pattern=pattern, ignore.case=TRUE,
                             any.missing=FALSE, min.len=1)
  checkmate::assertString(nan, na.ok=TRUE, pattern=pattern, ignore.case=TRUE)
  stopifnot(is.call(call))
  stopifnot(all(names(formals(GetColors)) %in% c(names(as.list(call)), "...")))
  structure(x, nan=nan, call=call, class=append("inlpal", class(x)))
}


# Check for valid color names
#'
#' Check whether a character string is a valid color specification.
#'
#' @param x 'character' vector.
#'   color specification
#' @param null.ok 'logical' flag.
#'   If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#'
#' @return Returns a 'logical' vector of the same length as argument \code{x}.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#' IsColor(c("red", "zzz", "#FFFFFF", "#7FAF1B111"))
#'

IsColor <- function(x, null.ok=FALSE) {
  if (is.null(x) && null.ok) return(TRUE)
  vapply(x, function(i) tryCatch({
    is.matrix(grDevices::col2rgb(i))
  }, error=function(e) FALSE), TRUE)
}
