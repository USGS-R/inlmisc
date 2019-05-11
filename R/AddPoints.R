#' Add Points to Plot
#'
#' Add point symbols to a plot.
#' Proportional circle symbols may be used to represent point data,
#' where symbol area varies in proportion to an attribute variable.
#'
#' @param x,y 'numeric' vector or 'SpatialPoints*'.
#'   \emph{x} and \emph{y} coordinates for the centers of the circle symbols.
#'   If numeric, can be specified in any way which is accepted by \code{\link{xy.coords}}.
#' @param z 'numeric' vector, 'integer' vector, or 'factor'.
#'   Attribute variable.
#'   For objects of class factor, a fixed radius is used for circle symbols,
#'   see \code{inches} argument description.
#' @param zcol 'integer' count or 'character' string.
#'   Attribute name or column number to extract from if \code{x} is of class 'SpatialGridDataFrame'.
#' @param crs 'character' string or 'CRS'.
#'   Coordinate reference system arguments
#' @param xlim 'numeric' vector of length 2.
#'   \emph{x} limits for the plot.
#' @param ylim 'numeric' vector of length 2.
#'   \emph{y} limits for the plot.
#' @param zlim 'numeric' vector of length 2.
#'   \emph{z} limits for the plot.
#' @param inches 'numeric' vector of length 2.
#'   Radii limits for the drawn circle symbol.
#'   Alternatively, a single number can be given resulting in
#'   a fixed radius being used for all circle symbols;
#'   this overrides proportional circles and the function
#'   behaves like the \code{\link{points}} function.
#' @param scaling 'character' string.
#'   Selects the scaling algorithm to use for symbol mapping.
#'   Specify \code{"perceptual"} or \code{"mathematical"} for
#'   proportional scaling (Tanimura and others, 2006),
#'   or "radius" for scaling symbol size to radius (usually a bad idea).
#' @param bg 'character' vector or 'function'.
#'   Fill color(s) for circle symbols.
#'   A color palette also may be specified.
#' @param bg.neg 'character' vector or 'function'.
#'   Fill color(s) for circle symbols corresponding to negative \code{z} values.
#'   A color palette also may be specified.
#'   For circle symbols corresponding to positive \code{z} values,
#'   the \code{bg} argument is used for color(s).
#' @param fg 'character' string.
#'   Outer-line color for circle symbols.
#'   Specify a value of \code{NA} to remove the symbols outer line, and
#'   \code{NULL} to match the outer-line color with the symbols fill color.
#' @param lwd 'numeric' number.
#'   Line width for drawing circle symbols.
#' @param cex 'numeric' number.
#'   Character expansion factor for legend labels.
#' @param format 'character' string.
#'   Formatting for legend values, see \code{\link{formatC}} for options.
#' @param legend.loc 'character' string.
#'   Position of the legend in the main plot region;
#'   see \code{\link{GetInsetLocation}} function for keyword descriptions.
#' @param inset 'numeric' number.
#'   Inset distance of the legend from the margins as a fraction of the main plot region.
#'   Defaults to 2 percent of the axis range.
#' @param bty 'character' string.
#'   Type of box to be drawn about the legend.
#'   A value of \code{"o"} (the default) results in a box and
#'   a value of \code{"n"} supresses the box.
#' @param breaks 'numeric' vector.
#'   Finite breakpoints for the legend circle symbols.
#' @param break.labels 'character' vector.
#'   Break labels with length equal to \code{breaks}.
#' @param quantile.breaks 'logical' flag.
#'   If true, \code{breaks} are set to the sample quantiles of \code{z}.
#' @param make.intervals 'logical' flag.
#'   If true, represent \code{z} within intervals.
#'   See \code{\link{findInterval}} function for details.
#'   Unused if \code{quantile.breaks} is true.
#' @param title 'character' string.
#'   Main title to be placed at the top of the legend.
#' @param subtitle 'character' string.
#'   Legend subtitle to be placed below the main title.
#' @param draw.legend 'logical' flag.
#'   If true, a legend is drawn.
#' @param draw.points 'logical' flag.
#'   If true, the circle symobls are drawn.
#' @param add 'logical' flag.
#'   If true, circle symbols (and an optional legend) are added to an existing plot.
#' @param ...
#'   Graphics parameters to be passed to \code{\link{PlotMap}}.
#'   Unused if \code{add = TRUE}.
#'
#' @return Used for the side-effect of a bubble map drawn on the current graphics device.
#'
#' @note To avoid overplotting, circle symbols are drawn in order of decreasing radius.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @references
#'   Tanimura, S., Kuroiwa, C., and Mizota, T., 2006, Proportional Symbol Mapping in R:
#'   Journal of Statistical Software, v. 15, no. 5, 7 p.
#'
#' @seealso \code{\link{symbols}}
#'
#' @keywords hplot
#'
#' @import rgdal
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
#' AddPoints(x, z = z, fg = "#00000080", lwd = 0.5, title = "Title",
#'           subtitle = "Subtitle", add = FALSE)
#'
#' idxs <- sample.int(n, floor(n / 2))
#' z[idxs] <- -z[idxs]
#' AddPoints(x, z = z, bg.neg = "#2A8FBDCB", breaks = pretty(z, n = 8),
#'           legend.loc = "bottomleft", add = FALSE)
#'
#' Pal1 <- colorRampPalette(c("#CA0020CB", "#F4A582CB"), alpha = TRUE)
#' Pal2 <- colorRampPalette(c("#0571B0CB", "#92C5DECB"), alpha = TRUE)
#' AddPoints(x, z = z, bg = Pal1, bg.neg = Pal2, add = FALSE)
#'
#' AddPoints(x, z = z, bg = Pal1, bg.neg = Pal2, add = FALSE, make.intervals = TRUE)
#'
#' AddPoints(x, z = z, bg = Pal1, bg.neg = Pal2, add = FALSE, make.intervals = TRUE,
#'           inches = 0.1)
#'
#' AddPoints(x, z = abs(z), title = "Quantiles", bg = topo.colors,
#'           quantile.breaks = TRUE, add = FALSE)
#'
#' z <- as.factor(rep(c("dog", "cat", "ant", "pig", "bat"), length.out = n))
#' bg <- GetColors(nlevels(z), scheme = "bright", alpha = 0.8)
#' AddPoints(x, z = z, bg = bg, add = FALSE)
#'
#' AddPoints(x, draw.legend = FALSE, add = FALSE)
#'

AddPoints <- function(x, y=NULL, z=NULL, zcol=1, crs=NULL,
                      xlim=NULL, ylim=NULL, zlim=NULL, inches=c(0, 0.2),
                      scaling=c("perceptual", "mathematical", "radius"),
                      bg="#1F1F1FCB", bg.neg=NULL, fg=NA, lwd=0.25,
                      cex=0.7, format=NULL, legend.loc="topright",
                      inset=0.02, bty=c("o", "n"), breaks=NULL, break.labels=NULL,
                      quantile.breaks=FALSE, make.intervals=FALSE,
                      title=NULL, subtitle=NULL, draw.legend=TRUE,
                      draw.points=FALSE, add=TRUE, ...) {

  scaling <- match.arg(scaling)
  bty <- match.arg(bty)

  if (is.character(crs)) crs <- try(sp::CRS(crs), silent=TRUE)
  if (!inherits(crs, "CRS")) crs <- sp::CRS(as.character(NA))

  if (inherits(x, "SpatialPoints")) {
    if (inherits(x, "SpatialPointsDataFrame") && is.null(z)) z <- x@data[, zcol]
    try(x <- sp::spTransform(x, crs), silent=TRUE)
    crs <- x@proj4string
    y <- sp::coordinates(x)[, 2]
    x <- sp::coordinates(x)[, 1]
  }

  if (is.null(z)) z <- factor(rep(1, length(x)), labels="missing")

  if (is.factor(z)) {
    z <- factor(z)  # drop levels that do not occur
    z <- factor(z, levels=rev(levels(z)))  # reverse order of levels
    break.labels <- levels(z)
    n <- length(break.labels)
    z <- as.numeric(z)
    breaks <- seq(0.5, max(z, na.rm=TRUE) - 0.5, by=1)
    zlim <- NULL
    quantile.breaks <- FALSE
    make.intervals <- FALSE
    if (length(inches) > 1) inches <- 0.03
  }

  # coordinates
  pts <- grDevices::xy.coords(x, y,
                              xlab=deparse(substitute(x)),
                              ylab=deparse(substitute(y)))
  x <- pts$x
  y <- pts$y

  # limits
  if (is.numeric(zlim)) {
    zran <- range(z, na.rm=TRUE)
    if (is.na(zlim[1])) zlim[1] <- zran[1]
    if (is.na(zlim[2])) zlim[2] <- zran[2]
    z[z < zlim[1] | z > zlim[2]] <- NA
  }

  is <- !is.na(z)
  if (!any(is)) stop("no fintie data to plot")

  xran <- if (length(x) == 1) c(x - 1, x + 1) else grDevices::extendrange(x[is])
  yran <- if (length(y) == 1) c(y - 1, y + 1) else grDevices::extendrange(y[is])
  if (is.numeric(xlim)) {
    if (is.na(xlim[1])) xlim[1] <- xran[1]
    if (is.na(xlim[2])) xlim[2] <- xran[2]
    z[x < xlim[1] | x > xlim[2]] <- NA
  } else {
    xlim <- xran
  }
  if (is.numeric(ylim)) {
    if (is.na(ylim[1])) ylim[1] <- yran[1]
    if (is.na(ylim[2])) ylim[2] <- yran[2]
    z[y < ylim[1] | y > ylim[2]] <- NA
  } else {
    ylim <- yran
  }
  is.lim <- !is.na(z)
  x <- x[is.lim]
  y <- y[is.lim]
  z <- z[is.lim]

  # breaks
  if (is.null(breaks)) breaks <- pretty(z)
  if (quantile.breaks) {
    breaks <- stats::quantile(z, probs=seq(0, 1, 0.25))
    if (is.null(break.labels)) {
      val <- formatC(breaks, format=format, big.mark=",")
      lab <- c("minimum", "25th quartile", "median", "75th quartile", "maximum")
      break.labels <- sprintf("%s (%s)", val, lab)
    }
    make.intervals <- FALSE
  } else if (make.intervals) {
    breaks <- sort(breaks)
    is_lt <- any(z < utils::head(breaks, 1))
    is_gt <- any(z > utils::tail(breaks, 1))
    interval <- findInterval(z, breaks, rightmost.closed=TRUE, left.open=TRUE)
    s <- formatC(breaks, format=format, big.mark=",")
    ss <- sprintf(">%s to %s", utils::head(s, -1), utils::tail(s, -1))
    if (is_gt) ss <- c(ss, sprintf(">%s", s[length(s)]))
    if (is_lt) {
      ss <- c(as.expression(bquote(""<=.(s[1]))), ss)
    } else {
      ss[1] <- sub("^>", "", ss[1])
    }
    if (is.null(break.labels)) break.labels <- ss

    # https://stackoverflow.com/questions/33930689
    SeqNext <- function(x, npred=1) {
      n <- length(x)
      d <- data.frame(x=seq_along(x), y=x)
      unname(stats::predict(stats::lm(y ~ poly(x, 2), data=d),
                            newdata=list("x"=seq(n + 1, n + npred))))
    }
    if (is_lt) {
      interval <- interval + 1L
      breaks <- c(SeqNext(rev(breaks)), breaks)
    }
    if (is_gt) breaks <- c(breaks, SeqNext(breaks))
    breaks <- utils::head(breaks, -1) + (diff(breaks) / 2)
    z <- breaks[interval]
  }
  if (is.null(break.labels))
    break.labels <- formatC(breaks, format=format, big.mark=",")

  # default plot
  if (!add) {
    PlotMap(crs, xlim=xlim, ylim=ylim, close.file=FALSE, ...)
    if (methods::hasArg(file) && !is.null(list(...)$file))
      on.exit(grDevices::dev.off())
  }

  # plot aspect ratio
  usr <- graphics::par("usr")
  pin <- graphics::par("pin")
  w <- pin[1] / diff(usr[1:2])
  h <- pin[2] / diff(usr[3:4])
  asp <- w / h

  # circle radii
  if (length(inches) > 1) {
    if (is.na(inches[1])) inches[1] <- 0
    if (is.na(inches[2])) inches[2] <- 0.2
    min.r <- diff(graphics::grconvertX(c(0, inches[1]), from="inches", to="user"))
    max.r <- diff(graphics::grconvertX(c(0, inches[2]), from="inches", to="user")) - min.r
    min.v <- min(abs(c(z, breaks)), na.rm=TRUE)
    max.v <- max(abs(c(z, breaks)), na.rm=TRUE)
    FUN <- function(v) {
      v <- abs(v)
      if (scaling == "perceptual") {
        r <- ((v / max.v)^0.57)
      } else if (scaling == "mathematical") {
        r <- sqrt(v / max.v)
      } else if (scaling == "radius") {
        r <- (v - min.v) / (max.v - min.v)
      }
      return(r * (max.r - min.r) + min.r)
    }
    r  <- FUN(z)
    r0 <- FUN(breaks)
  } else {
    fix.r <- diff(graphics::grconvertX(c(0, inches), from="inches", to="user"))
    r  <- rep(fix.r, length(z))
    r0 <- rep(fix.r, length(breaks))
  }

  # circle color
  cols  <- rep("#02080D", length(z))
  cols0 <- rep("#02080D", length(breaks))
  if (is.null(bg.neg)) {
    if (is.function(bg)) {
      if (make.intervals) {
        n <- length(breaks) + 1
        cols  <- .Map2Color(breaks, bg, n=n)[interval]
        cols0 <- .Map2Color(breaks, bg, n=n)
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
        idxs <- stats::na.omit(idxs - min(idxs, na.rm=TRUE) + 1)
        n <- length(breaks[breaks > 0]) + 1
        cols[z > 0] <- .Map2Color(breaks[breaks > 0], bg, n=n)[idxs]
        cols0[breaks > 0] <- .Map2Color(breaks[breaks > 0], bg, n=n)
      } else {
        cols[z > 0] <- .Map2Color(z[z > 0], bg)
        cols0[breaks > 0] <- .Map2Color(breaks[breaks > 0], bg)
      }
    } else {
      cols[z > 0] <- bg
      cols0[breaks > 0] <- bg
    }
    if (is.function(bg.neg)) {
      if (make.intervals) {
        idxs <- interval
        idxs[z > 0] <- NA
        idxs <- stats::na.omit(idxs - min(idxs, na.rm=TRUE) + 1)
        n <- length(breaks[breaks < 0]) + 1
        cols[z < 0] <- .Map2Color(abs(breaks[breaks < 0]), bg.neg, n=n)[idxs]
        cols0[breaks < 0] <- .Map2Color(abs(breaks[breaks < 0]), bg.neg, n=n)
      } else {
        cols[z < 0] <- .Map2Color(abs(z[z < 0]), bg.neg)
        cols0[breaks < 0] <- .Map2Color(abs(breaks[breaks < 0]), bg.neg)
      }

    } else {
      cols[z < 0] <- bg.neg
      cols0[breaks < 0] <- bg.neg
    }
  }

  # draw circles
  idxs <- order(r, decreasing=TRUE)
  fg.col  <- if (is.null(fg)) cols[idxs] else fg
  fg.col0 <- if (is.null(fg)) cols0 else fg
  graphics::symbols(x[idxs], y[idxs], circles=r[idxs], bg=cols[idxs],
                    fg=fg.col, inches=FALSE, lwd=lwd, add=TRUE)

  # add legend
  if (draw.legend) {
    ipadx <- graphics::strwidth("M", cex=cex)
    ipady <- ipadx * asp
    lab.width <- max(graphics::strwidth(break.labels, cex=cex))

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

    xy <- GetInsetLocation(dx, dy, loc=legend.loc, inset=inset)
    if (bty == "o")
      graphics::rect(xy[1], xy[2], xy[1] + dx, xy[2] + dy, col="#FFFFFFE7",
                     border="black", lwd=0.5)
    x <- rep(xy[1] + ipadx + max(r0), length(r0))
    y <- xy[2] + s
    graphics::symbols(x, y, circles=r0, bg=cols0, fg=fg.col0, inches=FALSE, lwd=lwd, add=TRUE)

    graphics::text(xy[1] + ipadx + max(r0) * 2 + ipadx, y, break.labels, adj=c(0, 0.5), cex=cex)
    if (!is.null(title))
      graphics::text(xy[1] + dx / 2, xy[2] + dy - ipady, title, adj=c(0.5, 0.5), cex=cex, font=2)
    if (!is.null(subtitle))
      graphics::text(xy[1] + dx / 2, xy[2] + dy - title.height - subtitle.height, subtitle,
                     adj=c(0.5, 0), cex=cex)
  }
  invisible()
}

##

.Map2Color <- function(x, Pal, xlim=NULL, n=100) {
  if (length(x) == 0) return(NULL)
  if (is.null(xlim)) xlim <- range(x)
  Pal(n)[findInterval(x, seq(xlim[1], xlim[2], length.out=n), all.inside=TRUE)]
}
