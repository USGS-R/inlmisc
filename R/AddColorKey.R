#' Add Color Key to Plot
#'
#' This function can be used to add a color key to a plot.
#'
#' @param mai 'numeric'.
#'   Vector of the form \code{c(bottom, left, top, right)} which gives the margin size specified in inches (optional).
#' @param is.categorical 'logical'.
#'   If true, color-key values represent categorical data;
#'   otherwise, these data values are assumed continuous.
#' @param breaks 'numeric'.
#'   Set of finite numeric breakpoints for the colors:
#'   must have one more breakpoint than color and be in increasing order.
#' @param col 'character'.
#'   Vector of colors to be used in the plot.
#'   This argument requires \code{breaks} specification for continuous data.
#'   For continuous data there should be one less color than breaks; whereas,
#'   categorical data require a color for each category.
#' @param at 'numeric'.
#'   The points at which tick-marks and labels are to be drawn,
#'   only applicable for continuous data.
#'   The tick-marks will be located at the color breaks if the length of \code{at} is greater than or equal to one minus the length of \code{breaks}.
#' @param labels 'logical', 'character', or 'expression'.
#'   Can either be a logical value specifying whether (numerical) annotations are to be made at the tickmarks,
#'   or a character or expression vector of labels to be placed at the tickpoints.
#' @param scientific 'logical'.
#'   Indicates if axes labels should be formatted for scientific notation,
#'   see \code{\link{ToScientific}} for details.
#' @param explanation 'character'.
#'   Label that describes the data values.
#' @param padx 'numeric'.
#'   Inner padding for the left and right margins specified in inches.
#'
#' @return Used for the side-effect of a color key drawn on the current graphics device.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{PlotCrossSection}}, \code{\link{PlotMap}}
#'
#' @keywords hplot
#'
#' @export
#'
#' @examples
#' dev.new(width = 7, height = 2)
#'
#' AddColorKey(is.categorical = FALSE, breaks = 0:10, scientific = TRUE,
#'             explanation = "Example description for data variables in meters.")
#' AddColorKey(is.categorical = FALSE, breaks = 0:10, at = pretty(0:10))
#' AddColorKey(is.categorical = FALSE, breaks = seq(0.5, 10.5, by = 1), at = 1:10)
#'
#' AddColorKey(is.categorical = TRUE, labels = LETTERS[1:5])
#' AddColorKey(is.categorical = TRUE, col = terrain.colors(5))
#'
#' dev.off()
#'

AddColorKey <- function(mai, is.categorical, breaks, col, at=NULL, labels=TRUE,
                        scientific=FALSE, explanation=NULL, padx=0.2) {

  if (!missing(mai)) {
    mai[2] <- mai[2] + padx
    mai[4] <- mai[4] + padx
    op <- graphics::par(mai=mai)
    on.exit(graphics::par(op))
  }

  if (is.categorical) {
    n <- max(c(if (missing(col)) 0 else length(col), length(labels)))
    at <- seq_len(n)
    if (length(n) == 0) stop("categorical data requires colors and (or) labels")
    breaks <- c(0.5, seq_len(n) + 0.5)
  } else if (missing(breaks)) {
    stop("missing breaks argument for continous data")
  }

  if (missing(col)) col <- grDevices::rainbow(length(breaks) - 1L, start=0.0, end=0.8)

  if (is.null(at)) at <- breaks

  lwd <- 0.5
  xlim <- range(breaks)
  plot(NA, type="n", xlim=xlim, ylim=c(0, 1), xaxs="i", yaxs="i", bty="n",
       xaxt="n", yaxt="n", xlab="", ylab="")

  if (is.categorical) {
    bw <- 2 / 6
    pin <- graphics::par("pin")
    repeat {
      if (bw < pin[1] || bw == 0.1) break
      bw <- bw - 0.1
    }
    dx <- (diff(xlim) / pin[1]) * bw / 2
    x <- seq_along(col)
    graphics::rect(xleft=x - dx, ybottom=0, xright=x + dx, ytop=1, col=col, border=NA)
  } else {
    graphics::rect(xleft=head(breaks, -1), ybottom=0, xright=tail(breaks, -1),
                   ytop=1, col=col, border=col, lwd=lwd)
    if (length(at) >= length(breaks) - 1L) {
      graphics::abline(v=breaks, lwd=lwd)
    } else {
      graphics::axis(1, at=at, labels=FALSE, lwd=-1, lwd.ticks=lwd, tck=0.25)
      graphics::axis(3, at=at, labels=FALSE, lwd=-1, lwd.ticks=lwd, tck=0.25)
    }
    graphics::box(lwd=lwd)
  }

  if (is.logical(labels) && labels) {
    labels <- if (is.null(at)) graphics::axTicks(1) else at
    scipen <- if (scientific) NULL else getOption("scipen", default=0L)
    labels <- ToScientific(labels, type="plotmath", scipen=scipen)
  }

  # TODO(jcf): https://joelgranados.com/2012/05/04/r-create-a-plot-with-non-overlapping-labels/
  # n <- nchar(gsub("%|\\^", "", as.character(labels)))

  graphics::axis(1, at=at, labels=labels, lwd=-1, lwd.ticks=-1,
                 padj=-0.3, mgp=c(3, 0.1, 0), cex.axis=0.7)

  if (!is.null(explanation))
    graphics::mtext(explanation, side=3, line=0.1, padj=0, adj=0, cex=0.7)

  invisible()
}
