#' Add Color Gradient Legend to Plot
#'
#' This function can be used to add a continuous color gradient legend strip to a plot.
#'
#' @param breaks 'numeric'.
#'   Set of finite numeric breakpoints for the colors.
#' @param pal 'function'.
#'   Color palette
#' @param at 'numeric'.
#'   Vector of points at which tick-marks and labels are to be drawn.
#' @param n 'integer'.
#'   Desired number of tick-marks to be drawn.
#'   Unused if \code{at} argument is specified.
#' @param labels 'logical' or 'character'.
#'   Can either be a logical value specifying whether (numerical) annotations are to be made at the tickmarks,
#'   or a character or expression vector of labels to be placed at the tickpoints.
#' @param scientific 'logical'.
#'   Indicates if labels should be formatted for scientific notation,
#'   see \code{\link{ToScientific}} for details.
#' @param title 'character'.
#'   Title to be placed at the top of the legend.
#' @param strip.dim 'numeric'.
#'   Dimensions of the color strip, in picas.
#' @param ...
#'   Additional arguments to be passed to the \code{\link{GetInsetLocation}} function---used
#'   to position the legend in the main plot region.
#'
#' @return Used for the side-effect of a legend drawn on the current graphics device.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{PlotMap}}
#'
#' @keywords hplot
#'
#' @export
#'
#' @examples
#' plot(NA, xlim = c(0, 100), ylim = c(-10, 10),
#'      xlab = "x", ylab = "y", xaxs = "i", yaxs = "i")
#' breaks <- 0:200
#' AddGradientLegend(breaks, GetTolColors, title = "Title", loc = "bottomleft")
#' AddGradientLegend(breaks, GetTolColors, title = "Title",
#'                   loc = "bottomleft", inset = c(0.2, 0.1))
#' breaks <- seq(0, 2e+06, length.out = 5)
#' AddGradientLegend(breaks, rainbow, loc = "topright", inset = 0.1)
#' pal <- function(...) rev(rainbow(...))  # reverse colors in palette
#' AddGradientLegend(breaks, pal, scientific = TRUE, strip.dim = c(1, 14),
#'                   inset = c(0.2, 0.1))
#'

AddGradientLegend <- function(breaks, pal, at=NULL, n=5, labels=TRUE,
                              scientific=FALSE, title=NULL,
                              strip.dim=c(2, 8), ...) {

  op <- graphics::par(no.readonly=TRUE)
  on.exit(graphics::par(op))

  usr <- graphics::par("usr")
  inches_in_pica <- 0.16666667
  lwd <- 0.25
  cex <- 0.7

  if (is.null(at)) at <- if (n > 2) pretty(breaks, n=n) else range(breaks)
  if (is.character(labels) && length(labels) != length(at))
    stop("Arguments 'labels' and 'at' should be the same length.")
  if (is.logical(labels) && labels) {
    if (scientific)
      labels <- ToScientific(at, type="plotmath")
    else
      labels <- formatC(at, big.mark=",")
  }

  if (length(strip.dim) == 1) strip.dim <- rep(strip.dim, 2)
  dx <- strip.dim[1] * inches_in_pica * (diff(usr[1:2]) / graphics::par("pin")[1])
  dy <- strip.dim[2] * inches_in_pica * (diff(usr[3:4]) / graphics::par("pin")[2])
  xy <- GetInsetLocation(dx, dy, ...)

  breaks_norm <- (breaks - min(breaks)) / (max(breaks) - min(breaks))
  at_norm <- (at - min(breaks)) / (max(breaks) - min(breaks))
  col <- pal(length(breaks) - 1L)

  graphics::rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, col="#FFFFFFCC", border=NA)
  plt <- c(graphics::grconvertX(c(xy[1], xy[1] + dx), "user", "nfc"),
           graphics::grconvertY(c(xy[2], xy[2] + dy), "user", "nfc"))
  graphics::par(plt=plt, bg="#FFFFFFCC", new=TRUE)
  graphics::plot(NA, type="n", xlim=c(0, 1), ylim=c(0, 1),
                 xaxs="i", yaxs="i", bty="n",
                 xaxt="n", yaxt="n", xlab="", ylab="")
  graphics::rect(xleft=0, ybottom=utils::head(breaks_norm, -1),
                 xright=1, ytop=utils::tail(breaks_norm, -1),
                 col=col, border=NA, lwd=lwd)
  graphics::axis(2, at=at_norm, labels=FALSE, lwd=-1, lwd.ticks=lwd, tck=0.25)
  graphics::axis(4, at=at_norm, labels=FALSE, lwd=-1, lwd.ticks=lwd, tck=0.25)
  graphics::box(lwd=lwd)
  graphics::axis(4, at=at_norm, labels=labels, lwd=-1, lwd.ticks=-1,
                 mgp=c(3, 0.2, 0), cex.axis=cex, las=1)
  if (!is.null(title))
    graphics::title(main=title, line=0.5, cex.main=cex)

  invisible(NULL)
}
