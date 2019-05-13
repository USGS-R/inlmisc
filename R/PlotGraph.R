#' Plot Graph
#'
#' Draw a sequence of points, lines, or box-and-whiskers.
#'
#' @param x,y 'Date' vector, 'numeric' vector, 'matrix', or 'data.frame'.
#'   Data for plotting where the vector length or number of rows should match.
#'   If \code{y} is missing, then \code{x = x[, 1]} and \code{y = x[, -1]}.
#' @param xlab 'character' string.
#'   Title for \emph{x} axis.
#' @param ylab 'character' vector of length 2.
#'   Title for the 1st and 2nd-\emph{y} axes.
#'   The title for the 2nd-\emph{y} axis is optional and requires \code{conversion.factor} be specified.
#' @param main 'character' string.
#'   Main title for the plot.
#' @param xlim 'numeric' or 'Date' vector of length 2.
#'   Minimum and maximum values for the \emph{x}-axis.
#' @param xn,yn 'integer' count.
#'   Desired number of intervals between tick-marks on the \emph{x}- and \emph{y}-axis, respectively.
#' @param ylog 'logical' flag.
#'   Whether a logarithm scale is used for the \emph{y} axis.
#' @param type 'character' string.
#'   Plot type, possible types are
#'   \itemize{
#'     \item "p" for \bold{p}oints,
#'     \item "l" for \bold{l}ines,
#'     \item "b" for \bold{b}oth points and lines,
#'     \item "s" for stair \bold{s}teps (default),
#'     \item "w" for box-and-\bold{w}hisker,
#'     \item "i" for \bold{i}nterval-censored data, see "Details" section below, and
#'     \item "n" for \bold{n}o plotting.
#'   }
#' @param lty 'integer' vector.
#'   Line type, see \code{\link{par}} function for all possible types.
#'   Line types are used cyclically.
#' @param lwd 'numeric' number.
#'   Line width
#' @param pch 'integer' count.
#'   Point type, see \code{\link{points}} function for all possible types.
#' @param col 'character' vector or 'function'.
#'   Point or line color, see \code{\link{par}} function for all possible ways this can be specified.
#'   Colors are used cyclically.
#' @param bg 'character' vector.
#'   Background colors for the open plot symbols given by \code{pch = 21:25} as in \code{\link{points}}.
#' @param fill 'character' string.
#'   Used to create filled area plots. Specify
#'   \code{"tozeroy"} to fill to zero on the \emph{y}-axis;
#'   \code{"tominy"} to fill to the minimum \emph{y} value in the plotting region; and
#'   \code{"tomaxy"} to fill to the maximum.
#'   Requires plot \code{type = "l"}, \code{"b"}, and \code{"s"}.
#' @param fillcolor 'character' vector.
#'   Colors for basic filled area plots.
#'   Defaults to a half-transparent variant of the line color (\code{col}).
#' @param pt.cex 'numeric' number.
#'   Expansion factor for the points.
#' @param xpd 'logical' flag.
#'   Whether to prevent point and (or) line symbols from being clipped to the plot region.
#' @param seq.date.by 'character' string, 'numeric' number, or 'difftime'.
#'   The increment of the date sequence, see the \code{by} argument
#'   in the \code{\link{seq.Date}} function for all possible ways this can be specified.
#' @param scientific 'logical' vector of length 1, 2, or 3, value is recycled as necessary.
#'   Whether axes labels should be encoded in nice scientific format.
#'   Vector elements correspond to the \emph{x}-axis, \code{y}-axis, and second \emph{y}-axis labels.
#'   Values are recycled as necessary.
#'   Missing values correspond to the current default penalty (see \code{\link{options}("scipen")})
#'   to be applied when deciding to print numeric values in fixed or scientific notation.
#' @param conversion.factor 'numeric' number.
#'   Conversion factor for the 2nd-\emph{y} axis.
#' @param boxwex 'numeric' number.
#'   Scale factor to be applied to all boxes, only applicable for box-and-whisker plots.
#' @param center.date.labels 'logical' flag.
#'   If true, date labels are horizontally centered between \emph{x}-axis tickmarks.
#' @param bg.polygon 'list'.
#'   If specified, a background polygon is drawn.
#'   The polygon is described using a list of arguments supplied to the \code{\link{polygon}} function.
#'   Passed arguments include \code{"x"} and \code{"col"}.
#' @inheritParams PlotMap
#'
#' @details Interval censored data (\code{type = "i"}) requires \code{y} be matrix of 2 columns.
#'   The first column contains the starting values, the second the ending values.
#'   Observations are represented using
#'     \code{(y0, Inf)} for right-censored value,
#'     \code{(y0, y0)} for exact value, and
#'     \code{(-Inf, y1)} for left-censored value, and
#'     \code{(y0, y1)} for an interval censored value.
#'   Where infinity is represented as \code{Inf} or \code{NA}, and \code{y} is a numeric value.
#'
#' @return Used for the side-effect of a new plot generated.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link[graphics]{matplot}}, \code{\link[graphics]{boxplot}}, \code{\link{AddIntervals}}
#'
#' @keywords hplot
#'
#' @export
#'
#' @examples
#' n <- 50L
#' x <- as.Date("2008-07-12") + 1:n
#' y <- sample.int(n, replace = TRUE)
#' PlotGraph(x, y, ylab = paste("Random number in", c("meters", "feet")),
#'           main = "Main Title", type = "p", pch = 16, scientific = FALSE,
#'           conversion.factor = 3.28)
#'
#' y <- data.frame(lapply(1:3, function(i) sample(n, replace = TRUE)))
#' PlotGraph(x, y, ylab = "Random number", pch = 1, seq.date.by = "days",
#'           scientific = TRUE)
#'
#' y <- sapply(1:3, function(i) sample((1:100) + i * 100, n, replace = TRUE))
#' m <- cbind(as.numeric(x), y)
#' col <- GetColors(3, scheme = "bright")
#' PlotGraph(m, xlab = "Number", ylab = "Random number", type = "b", pch = 15:17,
#'           col = col, pt.cex = 0.9)
#' legend("topright", LETTERS[1:3], inset = 0.05, col = col, lty = 1, pch = 15:17,
#'        pt.cex = 0.9, cex = 0.8, bg = "white")
#'
#' d <- data.frame(x = as.Date("2008-07-12") + 1:8 * 1000,
#'                 y0 = c(NA, NA, 1, 3, 1, 4, 2, pi),
#'                 y1 = c(1, 2, NA, NA, 4, 3, 2, pi))
#' PlotGraph(d, type = "i", ylim = c(0, 5), xpd = TRUE)
#'

PlotGraph <- function(x, y, xlab, ylab, main=NULL, asp=NA, xlim=NULL, ylim=NULL,
                      xn=5, yn=5, ylog=FALSE, type="s", lty=1, lwd=1,
                      pch=NULL, col=NULL, bg=NA, fill="none", fillcolor=NULL,
                      pt.cex=1, xpd=FALSE, seq.date.by=NULL, scientific=NA,
                      conversion.factor=NULL, boxwex=0.8,
                      center.date.labels=FALSE, bg.polygon=NULL) {

  fill <- match.arg(fill, c("none", "tozeroy", "tominy", "tomaxy"))
  checkmate::assertCharacter(fillcolor, null.ok=TRUE)

  scientific <- as.logical(scientific)
  scientific <- rep(scientific, length.out=3)
  scipen <- getOption("scipen", default=0)

  if (inherits(x, c("data.frame", "matrix"))) {
    if (inherits(x, "tbl_df")) x <- as.data.frame(x)
    if (missing(y)) y <- x[, -1]
    x <- x[, 1]
  } else if (missing(y)) {
    y <- x
    x <- seq_along(x)
  }
  y <- as.matrix(y)

  if (type == "i") {
    if (ncol(y) != 2) stop("interval-censored data requires 2 columns for argument y")
    y[!is.finite(y)] <- NA
  }

  if (inherits(x, "Date")) {
    if (!inherits(xlim, "Date")) xlim <- range(x, na.rm=TRUE)
    if (is.null(seq.date.by))
      xat <- seq(xlim[1], xlim[2], length.out=xn)
    else
      xat <- seq(xlim[1], xlim[2], by=seq.date.by)
  } else if (inherits(x, c("character", "factor"))) {
    x <- seq_along(x)
    xat <- x
    xlim <- grDevices::extendrange(x)
  } else {
    if (is.numeric(xlim)) {
      for (i in c(0, -1, 1, 0)) {
        xat <- grDevices::axisTicks(xlim, FALSE, nint=xn + i)
        if (xat[1] == xlim[1] & xat[length(xat)] == xlim[2]) break
      }
    } else {
      xat <- pretty(range(x, na.rm=TRUE), n=xn, min.n=2)
      xlim <- range(xat)
    }
  }

  if (is.null(ylim) || abs(diff(ylim)) < .Machine$double.eps^0.5) {
    yran <- grDevices::extendrange(y, f=0.001)
    if (abs(diff(yran)) < .Machine$double.eps^0.5) yran[2] <- yran[1]
    if (ylog && abs(diff(yran)) > 0)
      yat <- grDevices::axisTicks(log10(yran), TRUE, nint=yn)
    else
      yat <- pretty(yran, n=yn, min.n=2)
    ylim <- range(yat)
  } else {
    usr <- if (ylog) log10(ylim) else ylim
    if (usr[1] == -Inf) usr[1] <- 0
    for (i in c(0, -1, 1, 0)) {
      yat <- grDevices::axisTicks(usr, ylog, nint=yn + i)
      if (yat[1] == ylim[1] & yat[length(yat)] == ylim[2]) break
    }
  }

  n <- ifelse(type == "i", 1, ncol(y))
  if (is.null(col)) {
    scheme <- ifelse(n > 7, "smooth rainbow", "bright")
    col <- GetColors(n, scheme=scheme)
  } else if (is.function(col)) {
    col <- col(n)
  }

  n <- ifelse(type == "i", nrow(y), ncol(y))
  col <- rep_len(col, length.out=n)
  lty <- rep_len(lty, length.out=n)
  lwd <- rep_len(lwd, length.out=n)

  mar <- c(2.3, 4.1, 1.5, 4.1)
  if (is.null(main)) mar[3] <- mar[3] - 1
  if (is.null(conversion.factor)) mar[4] <- mar[4] - 2
  mgp <- c(3.2, 0.2, 0)  # cumulative axis margin line: title, labels, and line
  graphics::par(mar=mar, mgp=mgp, xpd=xpd)
  line.in.inches <- (graphics::par("mai") / graphics::par("mar"))[2]

  graphics::plot.new()
  graphics::plot.window(xlim=xlim, ylim=ylim, asp=asp, xaxt="n", yaxt="n",
                        xaxs="i", yaxs="i", log=ifelse(ylog, "y", ""))

  cex <- 0.7
  tcl <- 0.1 / graphics::par("csi")  # length for major ticks is 0.1 inches

  is.decreasing <- diff(graphics::par("usr")[1:2]) < 0

  if (is.list(bg.polygon)) {
    bg.col <- if (is.null(bg.polygon$col)) NA else bg.polygon$col
    graphics::polygon(bg.polygon$x, col=bg.col, border=NA)
  }

  graphics::abline(v=xat, col="lightgrey", lwd=0.5, xpd=FALSE)
  graphics::abline(h=yat, col="lightgrey", lwd=0.5, xpd=FALSE)

  if (type %in% c("l", "b", "s") && fill != "none") {
    if (is.null(fillcolor))
      fillcolor <- grDevices::adjustcolor(col, alpha.f=0.5)
    for (i in seq_len(ncol(y))) {
      xx <- as.numeric(x)
      yy <- as.numeric(y[, i])
      grp <- .GetSegmentGroup(yy)
      for (j in unique(stats::na.omit(grp))) {
        idxs <- which(grp %in% j)
        xxx <- xx[idxs]
        yyy <- yy[idxs]
        if (type == "s") {
          xxx <- sort(c(xxx, utils::tail(xxx, -1)), decreasing=is.decreasing)
          yyy <- utils::head(rep(yyy, each=2), -1)
          max.idx <- max(idxs)
          if (max.idx < length(xx)) {
            xxx <- c(xxx, xx[max.idx + 1])
            yyy <- c(yyy, utils::tail(yyy, 1))
          }
        }
        xxx <- c(xxx, utils::tail(xxx, 1), xxx[1])
        ylim <- sort(graphics::par("usr")[3:4])
        if (fill == "tozeroy") {
          ymin <- 0
        } else if (fill == "tominy") {
          ymin <- ylim[1]
        } else if (fill == "tomaxy") {
          ymin <- ylim[2]
        }
        yyy <- c(yyy, rep(ymin, 2))
        graphics::polygon(xxx, yyy, col=fillcolor[i], border=NA, xpd=FALSE)
      }
    }
  }

  if (inherits(x, "Date")) {
    if (center.date.labels) {
      if (utils::tail(xat, 1) < xlim[2])
        at <- xat + diff(c(xat, xlim[2])) / 2
      else
        at <- utils::head(xat, -1) + diff(xat) / 2
      graphics::axis.Date(1, at=xat, tcl=tcl, labels=FALSE, lwd=-1, lwd.ticks=0.5)
      graphics::axis.Date(1, at=at, tcl=0, cex.axis=cex, lwd=-1)
    } else {
      graphics::axis.Date(1, at=xat, tcl=tcl, cex.axis=cex, lwd=-1, lwd.ticks=0.5)
    }
    graphics::axis.Date(3, at=xat, tcl=tcl, labels=FALSE, lwd=-1, lwd.ticks=0.5)
  } else {
    if (is.na(scientific[1])) {
      xlabels <- ToScientific(xat, scipen=scipen, type="plotmath")
    } else if (scientific[1]) {
      digits <- format.info(as.numeric(xat))[2]
      while (TRUE) {
        xlabels <- ToScientific(xat, digits=digits, type="plotmath")
        if (!any(duplicated(unlist(lapply(xlabels, eval)))) | digits > 2) break
        digits <- digits + 1L
      }
    } else {
      s <- format(xat, big.mark=",", scientific=FALSE)
      xlabels <- sub("^\\s+", "", s)
    }
    graphics::axis(1, at=xat, labels=xlabels, tcl=tcl, las=1, cex.axis=cex,
                   lwd=-1, lwd.ticks=0.5)
    graphics::axis(3, at=xat, tcl=tcl, lwd=-1, lwd.ticks=0.5, labels=FALSE)
  }
  if (is.na(scientific[2])) {
    ylabels <- ToScientific(yat, scipen=scipen, type="plotmath")
  } else if (scientific[2]) {
      digits <- format.info(as.numeric(yat))[2]
      while (TRUE) {
        ylabels <- ToScientific(yat, digits=digits, type="plotmath")
        if (!any(duplicated(unlist(lapply(ylabels, eval)))) | digits > 2) break
        digits <- digits + 1L
      }
  } else {
    s <- format(yat, big.mark=",", scientific=FALSE)
    ylabels <- sub("^\\s+", "", s)
  }
  graphics::axis(2, at=yat, labels=ylabels, tcl=tcl, las=1, cex.axis=cex,
                 lwd=-1, lwd.ticks=0.5)

  if (!missing(xlab)) {
    mar.line <- sum(graphics::par("mgp")[2:3]) + graphics::par("mgp")[2] + cex
    graphics::title(xlab=xlab, cex.lab=cex, line=mar.line)
  }
  if (!missing(ylab)) {
    max.sw <- max(graphics::strwidth(ylabels, units="inches")) * cex
    mar.line <- max.sw / line.in.inches + sum(graphics::par("mgp")[2:3]) +
                graphics::par("mgp")[2]
    graphics::title(ylab=ylab[1], cex.lab=cex, line=mar.line)
  }
  if (!is.null(main)) graphics::title(main=list(main, "cex"=cex, "font"=1), line=0.5, adj=0)

  if (is.null(conversion.factor)) {
    graphics::axis(4, at=yat, tcl=tcl, lwd=-1, lwd.ticks=0.5, labels=FALSE)
  } else {

    usr <- graphics::par("usr")[3:4] * conversion.factor
    if (ylog) usr <- log10(usr)
    if (usr[1] == -Inf) usr[1] <- 0
    yat <- grDevices::axisTicks(usr, ylog, nint=yn)

    if (is.na(scientific[3])) {
      ylabels <- ToScientific(yat, scipen=scipen, type="plotmath")
    } else if (scientific[3]) {
      digits <- format.info(as.numeric(yat))[2]
      while (TRUE) {
        ylabels <- ToScientific(yat, digits=digits, type="plotmath")
        if (!any(duplicated(unlist(lapply(ylabels, eval)))) | digits > 2) break
        digits <- digits + 1L
      }
    } else {
      s <- format(yat, big.mark=",", scientific=FALSE)
      ylabels <- sub("^\\s+", "", s)
    }
    graphics::axis(4, at=(yat / conversion.factor), labels=ylabels, tcl=tcl,
                   las=1, cex.axis=cex, lwd=-1, lwd.ticks=0.5)
    if (!missing(ylab) && length(ylab) > 1) {
      max.sw <- max(graphics::strwidth(ylabels, units="inches")) * cex
      mar.line <- max.sw / line.in.inches + sum(graphics::par("mgp")[2:3])
      graphics::mtext(ylab[2], side=4, cex=cex, line=mar.line)
    }
  }

  graphics::box(lwd=0.5)

  x <- as.numeric(x)
  is.x <- x >= as.numeric(xlim[1]) & x <= as.numeric(xlim[2])
  x <- x[is.x]
  y <- y[is.x, , drop=FALSE]
  y[y < ylim[1] & y > ylim[2]] <- NA

  # box-and-whisker plot
  if (type %in% c("w", "box")) {
    graphics::boxplot(y, xaxt="n", yaxt="n", range=0, varwidth=TRUE, boxwex=boxwex,
                      col=col, border="#333333", add=TRUE, at=x)

  # interval censored plot
  } else if (type == "i") {
    AddIntervals(x, y[, 1], y[, 2], col=col, lty=1, lwd=lwd, cex=pt.cex, pch=pch, bg=bg)

  # stair steps plot
  } else if (type == "s") {
    for (i in seq_len(ncol(y))) {
      xx <- as.numeric(x)
      yy <- as.numeric(y[, i])
      grp <- .GetSegmentGroup(yy)
      for (j in unique(stats::na.omit(grp))) {
        idxs <- which(grp %in% j)
        xxx <- sort(c(xx[idxs], utils::tail(xx[idxs], -1)), decreasing=is.decreasing)
        yyy <- utils::head(rep(yy[idxs], each=2), -1)
        max.idx <- max(idxs)
        if (max.idx < length(xx)) {
          xxx <- c(xxx, xx[max.idx + 1])
          yyy <- c(yyy, utils::tail(yyy, 1))
        }
        graphics::lines(xxx, yyy, lty=lty[i], lwd=lwd[i], col=col[i])
      }
    }

  } else if (type != "n") {
    graphics::matplot(x, y, xaxt="n", yaxt="n", type=type, lty=lty, lwd=lwd,
                      pch=pch, col=col, bg=bg, cex=pt.cex, add=TRUE, verbose=FALSE)
  }

  invisible()
}


.GetSegmentGroup <- function (y) {
  grp <- as.integer(!is.na(y))
  mult <- 1L
  for (i in seq_along(grp)[-1]) {
    if (grp[i - 1] > 0 & grp[i] == 0) mult <- mult + 1L
    grp[i] <- grp[i] * mult
  }
  grp[grp == 0] <- NA
  grp
}
