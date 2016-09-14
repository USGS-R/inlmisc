#' Add Bubble Map to Plot
#'
#' This function can be used to add a bubble map to a plot.
#' Proportional circle symbols are used to represent spatial point data,
#' where symbol area varies in proportion to an attribute variable.
#'
#' @param x,y numeric.
#'   The x and y coordinates for the centers of the circle symbols.
#'   They can be specified in any way which is accepted by \code{xy.coords}.
#' @param z numeric.
#'   Attribute variable
#' @param zlim numeric.
#'   Minimum and maximum \code{z} values that circle symbols are plotted;
#'   defaults to the range of the finite values of \code{z}.
#' @param inches numeric.
#'   Vector of length 2 specifying the radii limits for the drawn circle symbol.
#' @param scaling character.
#'   Selects the proportional symbol mapping algorithm to be used;
#'   either "perceptual" or "mathematical" scaling (Tanimura and others, 2006).
#' @param bg.pos character or function.
#'   Fill color(s) for circle symbols corresponding to positive \code{z} values.
#'   A color palette also may be specified.
#' @param bg.neg character or function.
#'   Fill color(s) for circle symbols corresponding to negative \code{z} values.
#'   A color palette also may be specified.
#' @param fg character.
#'   Outer-line color for circle symbols.
#'   Specify a value of \code{NA} to remove the symbols outer line, and
#'   \code{NULL} to match the outer-line color with the symbols fill color.
#' @param lwd numeric.
#'   Line width for drawing circle symbols
#' @param cex character.
#'   Character expansion factor for legend labels
#' @param format character.
#'   Formatting for legend values, see \code{\link{formatC}} for options.
#' @param draw.legend logical.
#'   If true, a legend is drawn.
#' @param loc character.
#'   Position of the legend in the main plot region:
#'   "bottomleft", "topleft", "topright", or "bottomright" to denote scale location.
#' @param inset numeric.
#'   Inset distance of the legend from the margins as a fraction of the main plot region.
#'   Defaults to 2 percent of the axis range.
#' @param breaks numeric.
#'   Set of finite breakpoints for the legend circle symbols.
#' @param break.labels character.
#'   Vector of break labels with length equal to \code{breaks}.
#' @param quantile.breaks logical.
#'   If true, \code{breaks} are set to the sample quantiles of \code{z}.
#' @param make.intervals logical.
#'   If true, represent \code{z} within intervals.
#'   See \code{\link{findInterval}} function for details.
#' @param title character.
#'   Main title to be placed at the top of the legend.
#' @param subtitle character.
#'   Legend subtitle to be placed below the main title.
#' @param add logical.
#'   If true, circle symbols (and an optional legend) are added to an existing plot.
#'
#' @return Used for the side-effect of a bubble map drawn on the current graphics device.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @references Tanimura, S., Kuroiwa, C., and Mizota, T., 2006, Proportional Symbol Mapping in R: Journal of Statistical Software, v. 15, no. 5, 7 p.
#'
#' @seealso \code{\link{symbols}}
#'
#' @keywords hplot
#'
#' @export
#'
#' @examples
#' n <- 50L
#' x <- cbind(runif(n, 1, 10), runif(n, 1, 10))
#' z <- runif(n, -5000, 10000)
#' AddBubbles(x, z = z, fg = "green", lwd = 2, title = "Title", loc = "topright",
#'            breaks=pretty(z, n = 8), add = FALSE)
#'
#' Pal1 <- colorRampPalette(c("#F4A582", "#CA0020"))
#' Pal2 <- colorRampPalette(c("#92C5DE", "#0571B0"))
#' AddBubbles(x, z = z, bg.pos = Pal1, bg.neg = Pal2, add = FALSE)
#'
#' AddBubbles(x, z = z, bg.pos = Pal1, bg.neg = Pal2, add = FALSE,
#'            make.intervals = TRUE)
#'
#' AddBubbles(x, z = runif(n, 10, 10000), title = "Quantiles", bg.pos = topo.colors,
#'            quantile.breaks = TRUE, fg = NULL, add = FALSE)
#'

AddBubbles <- function(x, y=NULL, z, zlim=NULL, inches=c(0, 0.2),
                       scaling=c("perceptual", "mathematical"),
                       bg.pos="red", bg.neg="blue", fg=NA, lwd=0.25,
                       cex=0.7, format=NULL, draw.legend=TRUE,
                       loc=c("bottomleft", "topleft", "topright", "bottomright"),
                       inset=0.02, breaks=NULL, break.labels=NULL,
                       quantile.breaks=FALSE, make.intervals=FALSE,
                       title=NULL, subtitle=NULL,
                       add=TRUE) {

  xy <- grDevices::xy.coords(x, y,
                             xlab=deparse(substitute(x)),
                             ylab=deparse(substitute(y)))
  x <- xy$x
  y <- xy$y

  if (is.numeric(zlim)) {
    if (is.na(zlim[1])) zlim[1] <- min(z, na.rm=TRUE)
    if (is.na(zlim[2])) zlim[2] <- max(z, na.rm=TRUE)
    z[z < zlim[1] | z > zlim[2]] <- NA
  }
  is <- !is.na(z)
  x <- x[is]
  y <- y[is]
  z <- z[is]

  if (is.null(breaks)) breaks <- pretty(z, n=6)

  if (quantile.breaks) {
    breaks <- quantile(z, probs=seq(0, 1, 0.25))
    if (is.null(break.labels)) {
      val <- formatC(breaks, format=format, big.mark=",")
      lab <- c("minimum", "25th quartile", "median",
               "75th quartile", "maximum")
      break.labels <- sprintf("%s (%s)", val, lab)
    }
  } else if (make.intervals) {
    interval <- findInterval(z, breaks, rightmost.closed=TRUE)
    s <- formatC(breaks, format=NULL, big.mark=",")
    ss <- sprintf("[%s, %s)", head(s, -1), tail(s, -1))
    if (any(interval == 0)) ss[1] <- sprintf("(-Inf, %s)", s[1])
    n <- length(breaks)
    if (any(interval == n)) {
      if (any(z == max(breaks)))
        ss[length(ss)] <- sub(")$", "]", ss[length(ss)])
      else
        ss[length(ss)] <- sprintf("[%s, Inf)", s[length(s) - 1L])
    }
    if (is.null(break.labels)) break.labels <- ss
    idxs <- findInterval(z, breaks, all.inside=TRUE)
    breaks <- (head(breaks, -1) + tail(breaks, -1)) / 2
    z <- breaks[idxs]
  }

  if (is.null(break.labels))
    break.labels <- formatC(breaks, format=format, big.mark=",")

  if (!add) plot(NA, type="n", xlim=grDevices::extendrange(x),
                 ylim=grDevices::extendrange(y), xlab="x", ylab="y")

  if (is.na(inches[1])) inches[1] <- 0
  if (is.na(inches[2])) inches[2] <- 0.2
  min.r <- diff(graphics::grconvertX(c(0, inches[1]), from="inches", to="user"))
  max.r <- diff(graphics::grconvertX(c(0, inches[2]), from="inches", to="user")) - min.r

  scaling <- match.arg(scaling)
  if (scaling == "mathematical")
    Scale <- function(v, max.v, max.r) {return(sqrt(v / max.v) * max.r)}
  else
    Scale <- function(v, max.v, max.r) {return(((v / max.v)^0.57) * max.r)}
  r <- Scale(abs(z), max(abs(c(z, breaks))), max.r) + min.r

  cols <- rep("#02080D", length(z))
  if (is.function(bg.neg))
    cols[z < 0] <- .Map2Color(abs(z[z < 0]), bg.neg)
  else
    cols[z < 0] <- bg.neg
  if (is.function(bg.pos))
    cols[z > 0] <- .Map2Color(z[z > 0], bg.pos)
  else
    cols[z > 0] <- bg.pos

  idxs <- order(r, decreasing=TRUE)
  fg.cols <- if (is.null(fg)) cols[idxs] else fg
  graphics::symbols(x[idxs], y[idxs], circles=r[idxs], bg=cols[idxs],
                    fg=fg.cols, inches=FALSE, add=TRUE, lwd=lwd)

  if (draw.legend) {
    ipad <- graphics::strwidth("0", cex=cex)  # arbitrary choice for inner padding

    lab.width <- max(graphics::strwidth(break.labels, cex=cex))

    usr <- graphics::par("usr")
    padx <- inset * diff(usr[1:2])
    pady <- inset * diff(usr[3:4])

    r <- Scale(abs(breaks), max(abs(c(z, breaks))), max.r) + min.r

    gap <- max(c(stats::median(r), graphics::strheight("O", cex=cex) * 1.5))

    s <- vapply(seq_along(r), function(i) sum(r[1:i]), 0)
    s <- ipad + gap + 2 * s - r + (seq_along(r) - 1L) * gap

    dx <- ipad + max(r) * 2 + ipad + lab.width + ipad
    dy <- max(s) + r[length(r)] + ipad * 2

    if (!is.null(title)) {
      title.width <- graphics::strwidth(title, cex=cex, font=2) + ipad * 2
      if (dx < title.width) dx <- title.width
      title.height <- graphics::strheight(title, cex=cex, font=2)
      dy <- dy + title.height + ipad
    }

    if (!is.null(subtitle)) {
      subtitle.width <- graphics::strwidth(subtitle, cex=cex) + ipad * 2
      if (dx < subtitle.width) dx <- subtitle.width
      subtitle.height <- graphics::strheight(subtitle, cex=cex) * 1.5
      dy <- dy + subtitle.height
    }

    loc <- match.arg(loc)
    if (loc == "bottomleft") {
      loc <- c(usr[1] + padx, usr[3] + pady)
    } else if (loc == "topleft") {
      loc <- c(usr[1] + padx, usr[4] - pady - dy)
    } else if (loc == "topright") {
      loc <- c(usr[2] - padx - dx, usr[4] - pady - dy)
    } else if (loc == "bottomright") {
      loc <- c(usr[2] - padx - dx, usr[3] + pady)
    }

    graphics::rect(loc[1], loc[2], loc[1] + dx, loc[2] + dy,
                   col="#FFFFFFE7", border="black", lwd=0.5)

    x <- rep(loc[1] + ipad + max(r), length(r))
    y <- loc[2] + s

    cols <- rep("#02080D", length(breaks))
    if (is.function(bg.neg))
      cols[breaks < 0] <- .Map2Color(abs(breaks[breaks < 0]), bg.neg)
    else
      cols[breaks < 0] <- bg.neg
    if (is.function(bg.pos))
      cols[breaks > 0] <- .Map2Color(breaks[breaks > 0], bg.pos)
    else
      cols[breaks > 0] <- bg.pos
    fg.cols <- if (is.null(fg)) cols else fg
    graphics::symbols(x, y, circles=r, bg=cols, fg=fg.cols, inches=FALSE,
                      add=TRUE, lwd=lwd)

    text(loc[1] + ipad + max(r) * 2 + ipad, y, break.labels,
         adj=c(0, 0.5), cex=cex)

    if (!is.null(title))
      text(loc[1] + dx / 2, loc[2] + dy - ipad, title,
           adj=c(0.5, 1), cex=cex, font=2)

    if (!is.null(subtitle))
      text(loc[1] + dx / 2, loc[2] + dy - subtitle.height - title.height - ipad,
           subtitle, adj=c(0.5, 0), cex=cex)
  }

  invisible(NULL)
}

##

.Map2Color <- function(x, Pal, xlim=NULL, n=100L){
  if (length(x) == 0) return(NULL)
  if (is.null(xlim)) xlim <- range(x)
  Pal(n)[findInterval(x, seq(xlim[1], xlim[2], length.out=n), all.inside=TRUE)]
}
