#' Add Bubble Map to Plot
#'
#' This function can be used to add a bubble map to a plot.
#' Proportional circle symbols are used to represent spatial point data,
#' where symbol area varies in proportion to an attribute variable.
#'
#' @param x,y numeric.
#'   The x and y coordinates for the centers of the circle symbols.
#'   They can be specified in any way which is accepted by \code{\link{xy.coords}}.
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
#' @param bg character or function.
#'   Fill color(s) for circle symbols.
#'   A color palette also may be specified.
#' @param bg.neg character or function.
#'   Fill color(s) for circle symbols corresponding to negative \code{z} values.
#'   A color palette also may be specified.
#'   For circle symbols corresponding to positive \code{z} values,
#'   the \code{bg} argument is used for color(s).
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
#' @param fix.radii numeric.
#'   A fixed radius for all circle symbols, in inches;
#'   overrides proportional circle symbols and function behaves more like \code{\link{points}} function.
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
#' set.seed(2)
#'
#' n <- 50L
#' x <- cbind(runif(n, 1, 10), runif(n, 1, 500))
#' z <- runif(n, 0, 1000)
#' z[sample.int(n, 2)] <- 0
#' AddBubbles(x, z = z, fg = "green", lwd = 2, loc = "topright",
#'            title = "Title", subtitle = "Subtitle", add = FALSE)
#'
#' idxs <- sample.int(n, floor(n / 2))
#' z[idxs] <- -z[idxs]
#' AddBubbles(x, z = z, bg.neg = "blue", breaks = pretty(z, n = 8), add = FALSE)
#'
#' Pal1 <- colorRampPalette(c("#CA0020BF", "#F4A582BF"), alpha = TRUE)
#' Pal2 <- colorRampPalette(c("#0571B0BF", "#92C5DEBF"), alpha = TRUE)
#' AddBubbles(x, z = z, bg = Pal1, bg.neg = Pal2, add = FALSE)
#'
#' AddBubbles(x, z = z, bg = Pal1, bg.neg = Pal2, add = FALSE, make.intervals = TRUE)
#'
#' AddBubbles(x, z = z, bg = Pal1, bg.neg = Pal2, add = FALSE, make.intervals = TRUE,
#'            fix.radii = 0.1)
#'
#' AddBubbles(x, z = abs(z), title = "Quantiles", bg = topo.colors,
#'            quantile.breaks = TRUE, add = FALSE)
#'

AddBubbles <- function(x, y=NULL, z, zlim=NULL, inches=c(0, 0.2),
                       scaling=c("perceptual", "mathematical"),
                       bg="red", bg.neg=NULL, fg=NA, lwd=0.25,
                       cex=0.7, format=NULL, draw.legend=TRUE,
                       loc=c("bottomleft", "topleft", "topright", "bottomright"),
                       inset=0.02, breaks=NULL, break.labels=NULL,
                       quantile.breaks=FALSE, make.intervals=FALSE,
                       title=NULL, subtitle=NULL, fix.radii=NULL, add=TRUE) {

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

  # breaks
  if (is.null(breaks)) breaks <- pretty(z, n=6)
  if (quantile.breaks) {
    breaks <- quantile(z, probs=seq(0, 1, 0.25))
    if (is.null(break.labels)) {
      val <- formatC(breaks, format=format, big.mark=",")
      lab <- c("minimum", "25th quartile", "median", "75th quartile", "maximum")
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
    interval <- findInterval(z, breaks, all.inside=TRUE)
    breaks <- (head(breaks, -1) + tail(breaks, -1)) / 2
    z <- breaks[interval]
  }
  if (is.null(break.labels))
    break.labels <- formatC(breaks, format=format, big.mark=",")

  # plot
  if (!add)
    plot(NA, type="n", xlim=grDevices::extendrange(x), ylim=grDevices::extendrange(y),
         xlab="x", ylab="y")

  # aspect ratio
  usr <- graphics::par("usr")
  pin <- graphics::par("pin")
  w <- pin[1] / diff(usr[1:2])
  h <- pin[2] / diff(usr[3:4])
  asp <- w / h

  # radii
  if (is.na(inches[1])) inches[1] <- 0
  if (is.na(inches[2])) inches[2] <- 0.2
  min.r <- diff(graphics::grconvertX(c(0, inches[1]), from="inches", to="user"))
  max.r <- diff(graphics::grconvertX(c(0, inches[2]), from="inches", to="user")) - min.r
  fix.r <- diff(graphics::grconvertX(c(0, fix.radii), from="inches", to="user"))
  scaling <- match.arg(scaling)
  if (scaling == "mathematical")
    Scale <- function(v, max.v, max.r) {return(sqrt(v / max.v) * max.r)}
  else
    Scale <- function(v, max.v, max.r) {return(((v / max.v)^0.57) * max.r)}
  if (is.numeric(fix.radii)) {
    r  <- rep(fix.r, length(z))
    r0 <- rep(fix.r, length(breaks))
  } else {
    r  <- Scale(abs(z), max(abs(c(z, breaks))), max.r) + min.r
    r0 <- Scale(abs(breaks), max(abs(c(z, breaks))), max.r) + min.r
  }

  # color
  cols  <- rep("#02080D", length(z))
  cols0 <- rep("#02080D", length(breaks))
  if (is.null(bg.neg)) {
    if (is.function(bg)) {
      if (make.intervals) {
        cols  <- .Map2Color(breaks, bg, n=length(breaks))[interval]
        cols0 <- .Map2Color(breaks, bg, n=length(breaks))
      } else {
        cols  <- .Map2Color(z, bg)
        cols0 <- .Map2Color(breaks, bg)
      }
    } else {
      cols  <- rep(bg, length(z))
      cols0 <- rep(bg, length(breaks))
    }
  } else {
    if (is.function(bg)) {
      if (make.intervals) {
        idxs <- interval
        idxs[z < 0] <- NA
        idxs <- idxs - min(idxs, na.rm=TRUE) + 1L
        cols[z > 0] <- .Map2Color(breaks[breaks > 0], bg)[stats::na.omit(idxs)]
      } else {
        cols[z > 0] <- .Map2Color(z[z > 0], bg)
      }
      cols0[breaks > 0] <- .Map2Color(breaks[breaks > 0], bg)
    } else {
      cols[z > 0] <- bg
      cols0[breaks > 0] <- bg
    }
    if (is.function(bg.neg)) {
      if (make.intervals) {
        idxs <- interval
        idxs[z > 0] <- NA
        idxs <- idxs - min(idxs, na.rm=TRUE) + 1L
        cols[z < 0] <- .Map2Color(abs(breaks[breaks < 0]), bg.neg)[stats::na.omit(idxs)]
      } else {
        cols[z < 0] <- .Map2Color(abs(z[z < 0]), bg.neg)
      }
      cols0[breaks < 0] <- .Map2Color(abs(breaks[breaks < 0]), bg.neg)
    } else {
      cols[z < 0] <- bg.neg
      cols0[breaks < 0] <- bg.neg
    }
  }

  # draw circle symbols
  idxs <- order(r, decreasing=TRUE)
  fg.col  <- if (is.null(fg)) cols[idxs] else fg
  fg.col0 <- if (is.null(fg)) cols0 else fg
  graphics::symbols(x[idxs], y[idxs], circles=r[idxs], bg=cols[idxs],
                    fg=fg.col, inches=FALSE, lwd=lwd, add=TRUE)

  # legend
  if (draw.legend) {
    ipadx <- graphics::strwidth("O", cex=cex)
    ipady <- ipadx * asp
    lab.width <- max(graphics::strwidth(break.labels, cex=cex))
    padx <- inset * diff(usr[1:2])
    pady <- inset * diff(usr[3:4])

    r1 <- r0
    r1[r1 < (ipadx / 2)] <- ipadx / 2
    s <- ipady + r1[1] * asp
    for (i in seq_along(r1)[-1]) s[i] <- s[i-1] + r1[i - 1] * asp + ipady + r1[i] * asp
    dx <- ipadx + max(r0) * 2 + ipadx + lab.width + ipadx
    dy <- max(s) + r1[length(r1)] * asp + ipady

    # titles
    if (!is.null(title)) {
      title.width <- graphics::strwidth(title, cex=cex, font=2) + ipadx * 2
      if (dx < title.width) dx <- title.width
      title.height <- graphics::strheight(title, cex=cex, font=2) * 1.5
      dy <- dy + title.height
    }
    if (!is.null(subtitle)) {
      subtitle.width <- graphics::strwidth(subtitle, cex=cex) + ipadx * 2
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
    graphics::rect(loc[1], loc[2], loc[1] + dx, loc[2] + dy, col="#FFFFFFE7",
                   border="black", lwd=0.5)
    x <- rep(loc[1] + ipadx + max(r0), length(r0))
    y <- loc[2] + s
    graphics::symbols(x, y, circles=r0, bg=cols0, fg=fg.col0, inches=FALSE,
                      lwd=lwd, add=TRUE)

    text(loc[1] + ipadx + max(r0) * 2 + ipadx, y, break.labels, adj=c(0, 0.5), cex=cex)
    if (!is.null(title))
      text(loc[1] + dx / 2, loc[2] + dy - ipady, title, adj=c(0.5, 0.5), cex=cex, font=2)
    if (!is.null(subtitle))
      text(loc[1] + dx / 2, loc[2] + dy - title.height - subtitle.height, subtitle,
           adj=c(0.5, 0), cex=cex)
  }
  invisible(NULL)
}

##

.Map2Color <- function(x, Pal, xlim=NULL, n=100L){
  if (length(x) == 0) return(NULL)
  if (is.null(xlim)) xlim <- range(x)
  Pal(n)[findInterval(x, seq(xlim[1], xlim[2], length.out=n), all.inside=TRUE)]
}
