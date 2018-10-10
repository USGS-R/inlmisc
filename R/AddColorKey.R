#' Add Color Key to Plot
#'
#' This function can be used to add a color key to a plot.
#'
#' @param breaks 'numeric' vector.
#'   Finite breakpoints for the colors:
#'   must have one more breakpoint than color and be in increasing order.
#' @param is.categorical 'logical' flag.
#'   If true, color-key values represent categorical data;
#'   otherwise, these data values are assumed continuous.
#' @param col 'character' vector.
#'   Colors to be used in the plot.
#'   This argument requires \code{breaks} specification for continuous data.
#'   For continuous data there should be one less color than breaks; whereas,
#'   categorical data require a color for each category.
#' @param at 'numeric' vector.
#'   Points at which tick-marks and labels are to be drawn,
#'   only applicable for continuous data.
#'   The tick marks will be located at the color breaks if the length of \code{at} is
#'   greater than or equal to one minus the length of \code{breaks}.
#'   Note that tick-mark labels are omitted where they would abut or overlap previously drawn labels
#'   (labels are drawn left to right).
#' @param labels 'logical' flag, 'character' vector, 'expression' vector, 'numeric' vector, or 'factor' vector.
#'   Can either be a flag specifying whether (numerical) annotations are to be made at the tick marks,
#'   or a vector of labels to be placed at the tick points.
#' @param explanation 'character' string.
#'   Label that describes the data values.
#' @param padx 'numeric' number.
#'   Inner padding for the left and right margins specified in inches.
#' @param log 'logical' flag.
#'   Whether the axis is to be logarithmic.
#' @param mai 'numeric' vector of length 4.
#'   Margin size in inches and of the form \code{c(bottom, left, top, right)}.
#' @inheritParams ToScientific
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
#' op <- par(mfrow = c(7, 1), omi = c(1, 1, 1, 1), mar = c(2, 3, 2, 3))
#' AddColorKey(breaks = 0:10, explanation = "Example description of data variable.")
#' AddColorKey(breaks = 0:1000, at = pretty(0:1000))
#' AddColorKey(breaks = c(0, 1, 2, 4, 8, 16))
#' breaks <- c(pi * 10^(-5:5))
#' AddColorKey(breaks = breaks, log = TRUE)
#' AddColorKey(breaks = breaks, at = breaks[as.logical(seq_along(breaks) %% 2)],
#'             scipen = NULL, log = TRUE)
#' AddColorKey(is.categorical = TRUE, labels = LETTERS[1:5])
#' AddColorKey(is.categorical = TRUE, col = GetTolColors(5, scheme = "bright"))
#' par(op)
#'

AddColorKey <- function(breaks, is.categorical=FALSE, col=NULL, at=NULL,
                        labels=TRUE, scipen=getOption("scipen", 0),
                        explanation=NULL, padx=0.2, log=FALSE, mai=NULL) {

  # check arguments
  if (!missing(breaks))
    checkmate::assertNumeric(breaks, finite=TRUE, any.missing=FALSE, unique=TRUE, sorted=TRUE)
  checkmate::assertFlag(is.categorical)
  checkmate::assertCharacter(col, null.ok=TRUE)
  checkmate::assertNumeric(at, null.ok=TRUE)
  stopifnot(inherits(labels, c("logical", "character", "expression", "numeric", "factor")))
  checkmate::assertInt(scipen, na.ok=TRUE, null.ok=TRUE)
  checkmate::assertString(explanation, null.ok=TRUE)
  checkmate::assertNumber(padx, finite=TRUE)
  checkmate::assertFlag(log)
  checkmate::assertNumeric(mai, lower=0, finite=TRUE, any.missing=FALSE, len=4, null.ok=TRUE)

  if (is.categorical) {
    n <- max(c(if (is.null(col)) 0 else length(col), length(labels)))
    at <- seq_len(n)
    if (length(n) == 0) stop("categorical data requires colors and (or) labels")
    breaks <- c(0.5, seq_len(n) + 0.5)
  } else if (missing(breaks)) {
    stop("missing breaks argument for continous data")
  }

  if (is.null(col))
    col <- GetTolColors(length(breaks) - 1, start=0.3, end=0.9)

  if (is.null(at)) at <- breaks

  if (!is.null(mai)) {
    mai[2] <- mai[2] + padx
    mai[4] <- mai[4] + padx
    op <- graphics::par(mai=mai)
    on.exit(graphics::par(op))
  }

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
    graphics::rect(xleft=utils::head(breaks, -1), ybottom=0, xright=utils::tail(breaks, -1),
                   ytop=1, col=col, border=col, lwd=lwd)
    if (length(at) >= length(breaks) - 1) {
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

  if (is.numeric(labels))
    labels <- ToScientific(labels, type="plotmath", scipen=scipen)

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
                 cex.axis=cex, mgp=c(3, 0, 0))

  invisible()
}
