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
#'   The tick marks will be located at the color breaks if the length of \code{at} is
#'   greater than or equal to one minus the length of \code{breaks}.
#'   Note that tick-mark labels are omitted where they would abut or overlap previously drawn labels
#'   (labels are drawn left to right).
#' @param labels 'logical', 'character', 'expression', or 'numeric'.
#'   Can either be a logical value specifying whether (numerical) annotations are to be made at the tick marks,
#'   or a character or expression vector of labels to be placed at the tick points.
#' @param scientific 'logical' or 'integer'.
#'   Whether axes tick-mark labels should be formatted using scientific notation.
#'   Or an integer penalty, see \code{\link{option}["scipen"]} for details.
#' @param explanation 'character'.
#'   Label that describes the data values.
#' @param padx 'numeric'.
#'   Inner padding for the left and right margins specified in inches.
#' @param log 'logical'.
#'   Whether the axis is to be logarithmic.
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
#' AddColorKey(is.categorical = FALSE, breaks = 0:10,
#'             explanation = "Example description of data variable.")
#'
#' AddColorKey(is.categorical = FALSE, breaks = 0:1000, at = pretty(0:1000))
#'
#' AddColorKey(is.categorical = FALSE, breaks = c(0, 1, 2, 4, 8, 16))
#'
#' breaks <- c(pi * 10^(-5:5))
#' AddColorKey(is.categorical = FALSE, breaks = breaks, log = TRUE)
#'
#' is <- as.logical(seq_along(breaks) %% 2)
#' AddColorKey(is.categorical = FALSE, breaks = breaks, at = breaks[is],
#'             scientific = TRUE, log = TRUE)
#'
#' AddColorKey(is.categorical = FALSE, breaks = breaks, at = breaks[is],
#'             scientific = FALSE, log = TRUE)
#'
#' AddColorKey(is.categorical = TRUE, labels = LETTERS[1:5])
#'
#' AddColorKey(is.categorical = TRUE, col = grDevices::terrain.colors(5))
#'
#' dev.off()
#'

AddColorKey <- function(mai, is.categorical, breaks, col, at=NULL, labels=TRUE,
                        scientific=getOption("scipen", 0L), explanation=NULL,
                        padx=0.2, log=FALSE) {

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

  if (missing(col))
    col <- grDevices::rainbow(length(breaks) - 1L, start=0.0, end=0.8)

  if (is.null(at)) at <- breaks

  cex <- 0.7
  lwd <- 0.5

  xlim <- range(breaks)
  graphics::plot.default(NA, type="n", xlim=xlim, ylim=c(0, 1),
                         log=ifelse(log, "x", ""), xaxs="i", yaxs="i",
                         bty="n", xaxt="n", yaxt="n", xlab="", ylab="")

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

  if (!is.null(explanation))
    graphics::mtext(explanation, side=3, line=0.1, padj=0, adj=0, cex=cex)

  if (is.logical(labels)) {
    if (labels)
      labels <- if (is.null(at)) graphics::axTicks(1) else at
    else
      return(invisible(NULL))
  }

  if (is.numeric(labels)) {

    digits <- if (is.integer(labels)) 0 else format.info(labels)[2]

    if (is.logical(scientific)) {
      scipen <- NULL
    } else {
      scipen <- as.integer(scientific)
      scientific <- TRUE
    }
    if (scientific) {
      labels <- ToScientific(labels, digits=digits, type="plotmath", scipen=scipen)
    } else {
      fmt <- ifelse(is.integer(labels), "d", "fg")
      labels <- formatC(labels, digits=digits, width=1, format=fmt, big.mark=",")
    }
  }

  # omit labels that abut or overlap
  x <- if (log) log10(at) else at
  is <- rep(TRUE, length(x))
  dx <- (graphics::strwidth(labels, cex=cex) + graphics::strwidth("m", cex=cex)) / 2
  hold <- x[1] + dx[1]
  for (i in seq_along(x)[-1]) {
    is[i] <- x[i] - dx[i] > hold
    if (is[i]) hold <- x[i] + dx[i]
  }

  graphics::axis(1, at=at[is], labels=labels[is], lwd=-1, lwd.ticks=-1, padj=0,
                 cex.axis=cex, mgp=c(3, 0.1, 0))

  invisible()
}
