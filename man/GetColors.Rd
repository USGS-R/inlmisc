% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetColors.R
\name{GetColors}
\alias{GetColors}
\title{Get Palette Colors}
\usage{
GetColors(
  n,
  scheme = "smooth rainbow",
  alpha = NULL,
  stops = c(0, 1),
  bias = 1,
  reverse = FALSE,
  blind = NULL,
  gray = FALSE,
  ...
)
}
\arguments{
\item{n}{'integer' count.
Number of colors to be in the palette.
The maximum number of colors in a generated palette is dependent on the specified color scheme,
see \sQuote{Details} section for maximum values.}

\item{scheme}{'character' string.
Name of color scheme, see \sQuote{Details} section for scheme descriptions.
Argument choices may be abbreviated as long as there is no ambiguity.}

\item{alpha}{'numeric' number.
Alpha transparency, values range from 0 (fully transparent) to 1 (fully opaque).
Specify as \code{NULL} to exclude the alpha channel value from colors.}

\item{stops}{'numeric' vector of length 2.
Color stops defined by interval endpoints (between 0 and 1)
and used to select a subset of the color palette.
Only suitable for schemes that allow for color interpolations.}

\item{bias}{'numeric' number.
Interpolation bias where larger values result in more widely spaced colors at the high end.}

\item{reverse}{'logical' flag.
Whether to reverse the order of colors in the scheme.}

\item{blind}{'character' string.
Type of color blindness to simulate: specify \code{"deutan"} for green-blind vision,
\code{"protan"} for red-blind vision, \code{"tritan"} for green-blue-blind vision, or
\code{"monochrome"} for total-color blindness.
A partial-color blindness simulation requires that the \pkg{dichromat} package is available,
see \code{\link[dichromat]{dichromat}} function for additional information.
Argument choices may be abbreviated as long as there is no ambiguity.}

\item{gray}{'logical' flag.
Whether to subset/reorder the \code{"bright"}, \code{"high-contrast"}, \code{"vibrant"},
and \code{"muted"} schemes to work well after conversion to gray scale.}

\item{...}{Not used}
}
\value{
When argument \code{n} is specified the function
  returns an object of class 'inlpal' that inherits behavior from the 'character' class.
  And when \code{n} is unspecified a variant of the \code{GetColors} function is
  returned that has default argument values set equal to the values specified by the user.

  The inlpal-class object is comprised of a 'character'
  vector of \code{n} colors in the RGB color system.
  Colors are specified with a string of the form \code{"#RRGGBB"} or \code{"#RRGGBBAA"}
  where \code{RR}, \code{GG}, \code{BB}, and \code{AA} are the
  red, green, blue, and alpha hexadecimal values (00 to FF), respectively.
  Attributes of the returned object include:
  \code{"names"}, the informal names assigned to colors in the palette,
  where \code{NULL} indicates no color names are specified;
  \code{"NaN"}, a 'character' string giving the color meant for missing data,
  in hexadecimal format, where \code{NA} indicates no color is specified; and
  \code{"call"}, an object of class '\link{call}' giving the unevaluated function
  call (expression) that can be used to reproduce the color palette.
  Use the \code{\link{eval}} function to evaluate the \code{"call"} argument.
  A simple \code{plot} method is provided for the 'inlpal' class that
  shows a palette of colors using a sequence of shaded rectangles,
  see \sQuote{Examples} section for usage.
}
\description{
Create a vector of \code{n} colors from qualitative, diverging, and sequential color schemes.
}
\details{
The suggested data type for color schemes and the
  characteristics of generated palettes are given in the tables below.
  [\bold{Type}: is the type of data being represented,
  either qualitative, diverging, or sequential.
  \bold{Max n}: is the maximum number of colors in a generated palette.
  And the maximum \code{n} value when scheme colors are designed for
  gray-scale conversion is enclosed in parentheses.
  A value of infinity indicates that the scheme allows for color interpolations.
  \bold{N}: is the not-a-number color.
  \bold{B}: is the background color.
  \bold{F}: is the foreground color.
  \bold{Abbreviations}: --, not available]

  \if{html}{\figure{table01.svg}}
  \if{latex}{\figure{table01.pdf}{options: width=5.36in}}

  \if{html}{\figure{table02.svg}}
  \if{latex}{\figure{table02.pdf}{options: width=5.36in}}

  \if{html}{\figure{table03.svg}}
  \if{latex}{\figure{table03.pdf}{options: width=5.36in}}

  \if{html}{\figure{table04.svg}}
  \if{latex}{\figure{table04.pdf}{options: width=5.36in}}

  \if{html}{\figure{table05.svg}}
  \if{latex}{\figure{table05.pdf}{options: width=5.36in}}

  Schemes \code{"pale"}, \code{"dark"}, and \code{"ground cover"} are
  intended to be accessed in their entirety and subset using vector element names.
}
\note{
Sequential color schemes \code{"YlOrBr"} and \code{"iridescent"}
  work well for conversion to gray scale.
}
\examples{
pal <- GetColors(n = 10)
print(pal)
plot(pal)

Pal <- GetColors(scheme = "turbo")
formals(Pal)
filled.contour(datasets::volcano, color.palette = Pal,
               plot.axes = FALSE)

# Diverging color schemes (scheme)
op <- par(mfrow = c(6, 1), oma = c(0, 0, 0, 0))
plot(GetColors(  9, scheme = "BuRd"))
plot(GetColors(255, scheme = "BuRd"))
plot(GetColors(  9, scheme = "PRGn"))
plot(GetColors(255, scheme = "PRGn"))
plot(GetColors( 11, scheme = "sunset"))
plot(GetColors(255, scheme = "sunset"))
par(op)

# Qualitative color schemes (scheme)
op <- par(mfrow = c(7, 1), oma = c(0, 0, 0, 0))
plot(GetColors(7, scheme = "bright"))
plot(GetColors(6, scheme = "dark"))
plot(GetColors(5, scheme = "high-contrast"))
plot(GetColors(9, scheme = "light"))
plot(GetColors(9, scheme = "muted"))
plot(GetColors(6, scheme = "pale"))
plot(GetColors(7, scheme = "vibrant"))
par(op)

# Sequential color schemes (scheme)
op <- par(mfrow = c(7, 1), oma = c(0, 0, 0, 0))
plot(GetColors( 23, scheme = "discrete rainbow"))
plot(GetColors( 34, scheme = "smooth rainbow"))
plot(GetColors(255, scheme = "smooth rainbow"))
plot(GetColors(  9, scheme = "YlOrBr"))
plot(GetColors(255, scheme = "YlOrBr"))
plot(GetColors( 23, scheme = "iridescent"))
plot(GetColors(255, scheme = "iridescent"))
par(op)

# Alpha transparency (alpha)
op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
plot(GetColors(34, alpha = 1.0))
plot(GetColors(34, alpha = 0.8))
plot(GetColors(34, alpha = 0.6))
plot(GetColors(34, alpha = 0.4))
plot(GetColors(34, alpha = 0.2))
par(op)

# Color stops (stops)
op <- par(mfrow = c(4, 1), oma = c(0, 0, 0, 0))
plot(GetColors(255, stops = c(0.0, 1.0)))
plot(GetColors(255, stops = c(0.0, 0.5)))
plot(GetColors(255, stops = c(0.5, 1.0)))
plot(GetColors(255, stops = c(0.3, 0.9)))
par(op)

# Interpolation bias (bias)
op <- par(mfrow = c(7, 1), oma = c(0, 0, 0, 0))
plot(GetColors(255, bias = 0.4))
plot(GetColors(255, bias = 0.6))
plot(GetColors(255, bias = 0.8))
plot(GetColors(255, bias = 1.0))
plot(GetColors(255, bias = 1.2))
plot(GetColors(255, bias = 1.4))
plot(GetColors(255, bias = 1.6))
par(op)

# Reverse colors (reverse)
op <- par(mfrow = c(2, 1), oma = c(0, 0, 0, 0),
          cex = 0.7)
plot(GetColors(10, reverse = FALSE))
plot(GetColors(10, reverse = TRUE))
par(op)

# Color blindness (blind)
op <- par(mfrow = c(5, 1), oma = c(0, 0, 0, 0))
plot(GetColors(34, blind = NULL))
plot(GetColors(34, blind = "deutan"))
plot(GetColors(34, blind = "protan"))
plot(GetColors(34, blind = "tritan"))
plot(GetColors(34, blind = "monochrome"))
par(op)

# Gray-scale preparation (gray)
op <- par(mfrow = c(8, 1), oma = c(0, 0, 0, 0))
plot(GetColors(3, "bright", gray = TRUE))
plot(GetColors(3, "bright", gray = TRUE,
               blind = "monochrome"))
plot(GetColors(5, "high-contrast", gray = TRUE))
plot(GetColors(5, "high-contrast", gray = TRUE,
               blind = "monochrome"))
plot(GetColors(4, "vibrant", gray = TRUE))
plot(GetColors(4, "vibrant", gray = TRUE,
               blind = "monochrome"))
plot(GetColors(5, "muted", gray = TRUE))
plot(GetColors(5, "muted", gray = TRUE,
               blind = "monochrome"))
par(op)

}
\references{
Dewez, Thomas, 2004, Variations on a DEM palette, accessed October 15, 2018 at
  \url{http://soliton.vm.bytemark.co.uk/pub/cpt-city/td/index.html}

  Mikhailov, Anton, 2019, Turbo, an improved rainbow colormap for visualization:
  Google AI Blog, accessed August 21, 2019 at
  \url{https://ai.googleblog.com/2019/08/turbo-improved-rainbow-colormap-for.html}.

  Tol, Paul, 2018, Colour Schemes:
  SRON Technical Note, doc. no. SRON/EPS/TN/09-002, issue 3.1, 20 p.,
  accessed September 24, 2018 at \url{https://personal.sron.nl/~pault/data/colourschemes.pdf}.

  Wessel, P., Smith, W.H.F., Scharroo, R., Luis, J.F., and Wobbe, R., 2013,
  Generic Mapping Tools: Improved version released, AGU, v. 94, no. 45, p. 409--410
  \url{https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1002/2013EO450001}
}
\seealso{
\code{\link{SetHinge}} function to set the hinge location in
  a color palette derived from one or two color schemes.

  \code{\link[grDevices]{col2rgb}} function to express palette
  colors represented in the hexadecimal format as RGB triplets (R, G, B).
}
\author{
J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
}
\keyword{color}
