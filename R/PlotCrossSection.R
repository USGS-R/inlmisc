#' Plot Method for Cross Sections
#'
#' This function creates a cross-section view of raster data.
#' A key showing how the colors map to raster values is shown below the map.
#'
#' @param transect SpatialLines.
#'   Piecewise linear transect line.
#' @param rs RasterStack.
#'   Collection of \code{RasterLayer} objects with the same extent and resolution.
#' @param geo.lays character.
#'   Vector of names in \code{rs} that specify the geometry raster layers;
#'   these must be given in decreasing order, that is,
#'   from the upper most (such as land surface) to the lowest (such as a bedrock surface).
#' @param val.lays character.
#'   Vector of names in \code{rs} that specify the value raster layers (optional).
#'   Values from the first layer are mapped as colors to the area between the first and second geometry layers;
#'   the second layer mapped between the second and third geometry layers, and so on.
#' @param wt.lay character.
#'   The name in \code{rs} that specifies the water-table raster layer (optional).
#' @param asp numeric.
#'   The \emph{y/x} aspect ratio for spatial axes.
#' @param ylim numeric.
#'   Vector of length 2 giving the minimum and maximum values for the \emph{y}-axis.
#' @param max.dev.dim numeric.
#'   Vector of length 2 giving the maximum width and height for the graphics device in picas, respectively.
#'   Suggested dimensions for single-column, double-column, and sidetitle figures are
#'   \code{c(21, 56)}, \code{c(43, 56)}, and \code{c(56, 43)}, respectively.
#' @param n integer.
#'   Desired number of intervals to partition the range of raster values (optional).
#' @param breaks numeric.
#'   Vector of break points used to partition the colors representing numeric raster values (optional).
#' @param pal function.
#'   A color palette to be used to assign colors in the plot, \code{rainbow} by default.
#' @param col character.
#'   Vector of colors to be used in the plot.
#'   This argument requires \code{breaks} specification for numeric raster values and overrides any palette function specification.
#'   For numeric values there should be one less color than breaks.
#'   Categorical data require a color for each category.
#' @param ylab character.
#'   Label for the \emph{y} axis.
#' @param unit character.
#'   Label for the measurement unit of the \emph{x}- and \emph{y}-axes.
#' @param id character.
#'   Vector of length 2 giving the labels for the end points of the transect line,
#'   defaults to \emph{A--A'}.
#' @param labels list.
#'   Describes the location and values of labels in the color key.
#'   This list may include components \code{at} and \code{labels}, numeric and character vectors, respectively.
#' @param explanation character.
#'   Label that describes the cell values.
#' @param features SpatialGridDataFrame.
#'   Point features adjacent to the transect line that are used as reference labels for the upper geometry layer.
#' @param max.feature.dist numeric.
#'   Maximum distance from a point feature to the transect line,
#'   specified in the units of the \code{rs} projection.
#' @param draw.key logical.
#'   If \code{FALSE}, a color key is not drawn.
#' @param draw.sep logical.
#'   If \code{TRUE}, lines separating geometry layers are drawn.
#' @param is.categorical logical.
#'   If \code{TRUE}, cell values in \code{val.lays} represent categorical data;
#'   otherwise, these data values are assumed continuous.
#' @param contour.lines list.
#'   If specified, contour lines are drawn.
#'   The contours are described using a list of arguments supplied to \code{contour}.
#'   Passed arguments include \code{"drawlables"}, \code{"method"}, and \code{"col"}.
#' @param bg.col character.
#'   Color used for the background of the area below the upper geometry raster layer.
#' @param wt.col character.
#'   Color used for the water-table line.
#'
#' @details The dimensions of a new graphics device is dependent on the argument values of \code{max.dev.dim} and \code{asp}.
#'
#' @return Used for the side-effect of a new plot generated.
#'   Returns a \code{list} object with the following graphical parameters:
#'   \describe{
#'     \item{din}{device dimensions \code{(width, height)}, in inches.}
#'     \item{usr}{extremes of the coordinates of the plotting region \code{(x1, x2, y1, y2)}.}
#'     \item{heights}{relative heights on the device \code{(upper, lower)} for the map and color-key plots.}
#'   }
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{AddScaleBar}}, \code{\link{AddColorKey}}, \code{\link{ExtractAlongTransect}}
#'
#' @keywords hplot
#'
#' @import sp
#' @import rgdal
#' @import raster
#'
#' @export
#'
#' @examples
#' library(raster)
#'
#' data(volcano)
#' x <- seq(from = 2667405, length.out = 61, by = 10)
#' y <- seq(from = 6478705, length.out = 87, by = 10)
#' r1 <- raster(volcano, xmn = min(x), xmx = max(x), ymn = min(y), ymx = max(y),
#'              crs = CRS("+init=epsg:27200"))
#' r2 <- min(r1[]) - r1 / 10
#' r3 <- r1 - r2
#' rs <- stack(r1, r2, r3)
#' names(rs) <- c("r1", "r2", "r3")
#'
#' xy <- rbind(c(2667508, 6479501), c(2667803, 6479214), c(2667508, 6478749))
#' transect <- SpatialLines(list(Lines(list(Line(xy)), ID = "Transect")),
#'                          proj4string = crs(rs))
#'
#' plot(r1)
#' lines(transect)
#' text(as(transect, "SpatialPoints"), labels = c("A", "BEND", "A'"), cex = 0.7,
#'      pos = c(3, 4, 1), offset = 0.1, font = 4)
#'
#' graphics.off()
#' PlotCrossSection(transect, rs, geo.lays = c("r1", "r2"), val.lays = "r3",
#'                  ylab="Elevation", asp = 5, unit = "METERS",
#'                  explanation = "Vertical thickness between layers, in meters.")
#'

PlotCrossSection <- function(transect, rs, geo.lays=names(rs), val.lays=NULL,
                             wt.lay=NULL, asp=1, ylim=NULL,
                             max.dev.dim=c(43, 56), n=NULL, breaks=NULL,
                             pal=NULL, col=NULL, ylab=NULL, unit=NULL,
                             id=c("A", "A'"), labels=NULL, explanation=NULL,
                             features=NULL, max.feature.dist=Inf, draw.key=TRUE,
                             draw.sep=TRUE, is.categorical=FALSE,
                             contour.lines=NULL, bg.col="#E1E1E1",
                             wt.col="#FFFFFFD8") {

  if (!inherits(transect, "SpatialLines"))
    stop("incorrect class for 'transect' argument")
  if (!inherits(rs, c("RasterStack", "RasterBrick")))
    stop("incorrect class for 'rs' argument")
  if (!all(c(geo.lays, val.lays) %in% names(rs)))
    stop("layer names not found in raster stack")
  if (length(val.lays) >= length(geo.lays))
    stop("number of value layers is greater than the number of geometry layers")

  transect <- spTransform(transect, crs(rs))
  rs <- crop(rs, extent(as.vector(t(bbox(transect)))), snap="out")
  layer.names <- unique(c(geo.lays, val.lays, wt.lay))
  eat <- ExtractAlongTransect(transect, subset(rs, layer.names))

  cell.values <- NULL
  cell.polys  <- list()
  for (i in seq_along(val.lays)) {
    for (j in seq_along(eat)) {
      seg <- as.matrix(eat[[j]]@data)
      for (k in seq(1, nrow(seg) - 1L, by=2)) {
        v <- as.numeric(seg[k, val.lays[i]])
        p <- rbind(seg[c(k, k + 1L), c("dist", geo.lays[i])],
                   seg[c(k + 1L, k), c("dist", geo.lays[i + 1L])],
                   seg[k, c("dist", geo.lays[i]), drop=FALSE])
        if (anyNA(p)) next
        cell.values <- c(cell.values, v)
        cell.polys  <- c(cell.polys, Polygon(p))
      }
    }
  }

  at <- NULL
  if (!is.null(cell.values)) {
    if (!is.function(pal)) {
      if (requireNamespace("colorspace", quietly=TRUE)) {
        pal <- function(n) {
          colorspace::rainbow_hcl(n, start=0.0, end=(360 * (n - 1) / n) * 0.8)
        }
      } else {
        pal <- function(n) grDevices::rainbow(n, start=0.0, end=0.8)
      }
    }
    if (is.categorical) {
      unique.vals <- sort(unique(cell.values))
      at <- seq_along(unique.vals)
      if (!is.character(col)) col <- pal(length(at))
      cell.cols <- col[match(cell.values, unique.vals)]
    } else {
      if (!is.numeric(n)) n <- 200L
      if (!is.numeric(breaks)) {
        at <- pretty(cell.values, n=8)
        breaks <- seq(min(at), max(at), length.out=n)
      }
      intervals <- findInterval(cell.values, breaks, all.inside=TRUE)
      if (!is.character(col)) col <- pal(length(breaks) - 1L)
      cell.cols <- col[intervals]
    }
    cols <- unique(cell.cols)

    FUN <- function(i) {
      p <- Polygons(cell.polys[which(cell.cols == cols[i])], i)
      p <- rgeos::gUnaryUnion(SpatialPolygons(list(p), i))
      p <- methods::slot(p, "polygons")[[1]]
      p@ID <- cols[i]
      return(p)
    }
    p <- lapply(seq_along(cols), FUN)
    cell.polys.merged <- SpatialPolygons(p, seq_along(cols))
  }

  x <- unlist(lapply(eat, function(i) i@data$dist))
  xlim <- range(x, na.rm=TRUE)
  xat <- pretty(xlim)

  y <- unlist(lapply(eat, function(i) unlist(i@data[, geo.lays])))
  if (is.numeric(ylim)) {
    yat <- pretty(ylim)
  } else {
    yat <- pretty(range(y, na.rm=TRUE))
    ylim <- range(yat, na.rm=TRUE)
  }

  inches.in.pica <- 1 / 6
  mar2 <- c(1, 4.6, 4, 2)
  if (draw.key) {
    y1 <- 1
    mar1 <- c(2, mar2[2], 1, mar2[4])
  } else {
    y1 <- 0
    mar1 <- c(0, 0, 0, 0)
  }
  if (grDevices::dev.cur() > 1) {
    dev.dim <- grDevices::dev.size() / inches.in.pica
    w <- dev.dim[1]
    h1 <- y1 + mar1[1] + mar1[3]
    h2 <- dev.dim[2] - h1
    h <- h1 + h2
  } else {
    w <- max.dev.dim[1]
    repeat {
      y2 <- (w - mar2[2] - mar2[4]) * (diff(ylim) / diff(xlim)) * asp
      h2 <- y2 + mar2[1] + mar2[3]
      h1 <- y1 + mar1[1] + mar1[3]
      h <- h1 + h2
      if (h > max.dev.dim[2]) w <- w - 0.01 else break
    }
    wi <- w * inches.in.pica
    hi <- h * inches.in.pica
    grDevices::dev.new(width=wi, height=hi)
  }

  if (draw.key) {
    graphics::layout(matrix(c(2, 1), nrow=2, ncol=1), heights=c(h2, h1) / h)
    if (!is.null(labels$at)) at <- labels$at
    labs <- if (is.null(labels$labels)) TRUE else labels$labels
    AddColorKey(mai=mar1 * inches.in.pica, is.categorical=is.categorical,
                breaks=breaks, col=col, at=at, labels=labs,
                explanation=explanation)
  } else {
    graphics::layout(matrix(1, nrow=1, ncol=1))
  }

  graphics::par(mai=mar2 * inches.in.pica, mgp=c(3, 0.6, 0))

  plot(NA, type="n", xlim=xlim, ylim=ylim, xaxs="i", yaxs="i", bty="n",
       xaxt="n", yaxt="n", xlab="", ylab="", asp=asp)

  lwd <- 0.5
  cex <- 0.7
  tcl <- -7.2 / graphics::par("cra")[2]
  usr <- graphics::par("usr")

  if (is.character(bg.col)) {
    FUN <- function(i) {
      m <- cbind(x=i@data[[1]], y=i@data[[2]])
      m <- rbind(m, cbind(rev(range(m[, "x"], na.rm=TRUE)), usr[3]),
                 m[1, , drop=FALSE])
      return(Polygon(m))
    }
    bg.poly <- SpatialPolygons(list(Polygons(lapply(eat, FUN), "bg")), 1L)
    plot(bg.poly, col=bg.col, border=NA, lwd=0.1, add=TRUE)
  }

  plot(cell.polys.merged, col=cols, border=cols, lwd=0.1, add=TRUE)

  if (!is.null(wt.lay) && wt.lay[1] %in% names(rs)) {
    for (s in eat)
      lines(s@data[["dist"]], s@data[, wt.lay[1]], lwd=lwd, col=wt.col)
  }

  lays <- if (draw.sep) geo.lays else c(head(geo.lays, 1), tail(geo.lays, 1))
  for (s in eat)
    graphics::matplot(s@data[["dist"]], s@data[, lays], xaxt="n", yaxt="n",
                      type="l", lty=1, lwd=lwd, col="#1F1F1F", add=TRUE)

  if (is.list(contour.lines)) {
    e <- extent(cell.polys.merged)
    nc <- 200
    dx <- diff(e[1:2]) / nc
    nr <- as.integer(diff(e[3:4]) / (dx / asp))
    r <- raster(e, nrows=nr, ncols=nc)
    FUN <- function(i) Polygons(list(cell.polys[[i]]), as.character(i))
    p <- lapply(seq_along(cell.polys), FUN)
    p <- SpatialPolygons(p, seq_along(cell.polys))
    r <- rasterize(p, r)
    r[] <- cell.values[r[]]

    color <- as.character(contour.lines[["col"]])
    drawl <- as.logical(contour.lines[["drawlabels"]])
    metho <- as.character(contour.lines[["method"]])
    color <- ifelse(length(color) == 1 && !is.na(color), color, "#1F1F1F")
    drawl <- ifelse(length(drawl) == 1 && !is.na(drawl), drawl, TRUE)
    metho <- ifelse(length(metho) == 1 && !is.na(metho), metho, "flattest")
    contour.breaks <- if (length(breaks) > 20) pretty(breaks, 20L) else breaks
    ncontours <- length(contour.breaks)
    raster::contour(r, maxpixels=length(r), levels=contour.breaks,
                    labels=formatC(contour.breaks, big.mark=","), xlim=xlim,
                    ylim=ylim, labcex=0.5, drawlabels=drawl, method=metho,
                    axes=FALSE, col=color, lwd=lwd, add=TRUE)
  }

  ylabs <- format(yat, big.mark=",")

  graphics::axis(4, at=yat, labels=FALSE, lwd=0, lwd.ticks=lwd, tcl=tcl)
  graphics::axis(2, at=yat, labels=ylabs, lwd=0, lwd.ticks=lwd, tcl=tcl,
                 cex.axis=cex, las=1)

  if (!is.null(ylab)) {
    line.in.inches <- (graphics::par("mai") / graphics::par("mar"))[2]
    max.sw <- max(graphics::strwidth(ylabs, units="inches")) * cex
    mar.line <- max.sw / line.in.inches + sum(graphics::par("mgp")[2:3]) +
                graphics::par("mgp")[2]
    graphics::title(ylab=ylab, cex.lab=cex, line=mar.line)
  }

  graphics::abline(v=xlim, col="black", lwd=lwd)
  graphics::abline(h=usr[3],   col="black", lwd=lwd)

  if (!is.null(unit)) graphics::mtext(unit, at=usr[1], cex=cex, line=0.2, adj=1)

  if (is.character(id)) {
    if (length(id) == 1) id <- c(id, paste0(id, "'"))
    graphics::mtext(id[1], at=usr[1], cex=cex, line=1, adj=0.5, font=4)
    graphics::mtext(id[2], at=usr[2], cex=cex, line=1, adj=0.5, font=4)
  }

  graphics::par(xpd=TRUE)
  y <- unlist(lapply(eat, function(i) i@data[[geo.lays[1]]]))
  GetGeoTop <- stats::approxfun(x, y)
  pady <- diff(usr[3:4]) * 0.02
  d <- as.matrix(stats::dist(coordinates(methods::as(transect, "SpatialPoints"))))[, 1]
  dist.to.bend <- head(d[-1], -1)
  for (d in dist.to.bend) {
    y <- GetGeoTop(d)
    lines(c(d, d), c(ylim[1], y + pady), lwd=0.3, col="#999999")
    text(d, y + pady, "BEND", adj=c(-0.1, 0.5), col="#999999", cex=0.6, srt=90)
  }
  if (!is.null(features)) {
    tran.pts <- do.call("rbind", eat)
    for (i in seq_len(length(features))) {
      pnt <- spTransform(features, crs(rs))[i, ]
      dist.to.transect <- rgeos::gDistance(pnt, tran.pts, byid=TRUE)
      idx <- which.min(dist.to.transect)
      if (dist.to.transect[idx] > max.feature.dist) next
      d <- x[idx]
      y <- GetGeoTop(d)
      lines(c(d, d), c(y, y + pady), lwd=0.3)
      label <- format(pnt@data[1, 1])
      text(d, y + pady, label, adj=c(-0.1, 0.5), cex=cex, srt=90)
    }
  }
  graphics::par(xpd=FALSE)

  AddScaleBar(asp, unit, loc="bottomright", offset=c(-0.3, 0))

  invisible(list(din=graphics::par("din"), usr=usr, heights=c(h2, h1) / h))
}
