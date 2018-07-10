#' Plot Method for Maps
#'
#' This function maps raster data and geographical features.
#' A key showing how the colors map to raster values is shown below the map.
#' The width and height of the graphics region will be automagically determined in some cases.
#'
#' @param r 'Raster*', 'Spatial*', or 'CRS'.
#'   An object that can be converted to a raster layer, or coordinate reference system (CRS).
#' @param layer 'integer'.
#'   Layer to extract from if \code{r} is of class 'RasterStack/Brick' or 'SpatialGridDataFrame'.
#' @param att 'integer' or 'character'.
#'   The levels attribute to use in the Raster Attribute Table (RAT);
#'   requires \code{r} values of class factor.
#' @param n 'integer'.
#'   Desired number of intervals to partition the range of raster values (or \code{zlim} if specified) (optional).
#' @param breaks 'numeric'.
#'   Vector of break points used to partition the colors representing numeric raster values (optional).
#' @param xlim 'numeric'.
#'   Vector of length 2 giving the minimum and maximum values for the \emph{x}-axis.
#' @param ylim 'numeric'.
#'   Vector of length 2 giving the minimum and maximum values for the \emph{y}-axis.
#' @param zlim 'numeric'.
#'   Vector of length 2 giving the minimum and maximum raster values for which colors should be plotted.
#' @param asp 'numeric'.
#'   \emph{y/x} aspect ratio for spatial axes.
#'   Defaults to 1 (one unit on the \emph{x}-axis equals one unit on the \emph{y}-axis) when \code{r} is projected,
#'   otherwise, a calculated value based on axes limits is used.
#' @param extend.xy 'logical'.
#'   If true, the spatial limits will be extended to the next tick mark on the axes beyond the grid extent.
#' @param extend.z 'logical'.
#'   If true, the raster value limits will be extended to the next tick mark on the color key beyond the measured range.
#'   Not used if the \code{zlim} argument is specified.
#' @param reg.axs 'logical'.
#'   If true, the spatial data range is extended.
#' @param dms.tick 'logical'.
#'   If true and \code{r} is projected, the axes tickmarks are specified in degrees, minutes, and decimal seconds (DMS).
#' @param bg.lines 'logical'.
#'   If true, grids or graticules are drawn in back of the raster layer using white lines and a grey background.
#' @param bg.image 'RasterLayer'.
#'   An image to be drawn in back of the main raster layer \code{r}, image colors are derived from a vector of gray levels.
#'   Raster values typically represent hill shading based on the slope and aspect of land-surface elevations,
#'   see \code{\link[raster]{hillShade}} function.
#' @param bg.image.alpha 'numeric'.
#'   Opacity of the background image from 0 to 1.
#' @param pal 'function'.
#'   Color palette to be used to assign colors in the plot, rainbow by default.
#' @param col 'character'.
#'   Vector of colors to be used in the plot.
#'   This argument requires \code{breaks} specification for numeric values of \code{r} and
#'   overrides any palette function specification.
#'   For numeric values there should be one less color than breaks.
#'   Factors require a color for each level.
#' @param max.dev.dim 'numeric'.
#'   Vector of length 2 giving the maximum width and height for the graphics device in picas, respectively.
#'   Where 1 pica is equal to 1/6 of an inch, 4.2333 of a millimetre, or 12 points.
#'   Suggested dimensions for single-column, double-column, and sidetitle figures are
#'   \code{c(21, 56)}, \code{c(43, 56)}, and \code{c(56, 43)}, respectively.
#'   This argument is only applicable when the \code{file} argument is specified.
#' @param labels 'list'.
#'   Describes the location and values of labels in the color key.
#'   This list may include components \code{at} and \code{labels}.
#' @param scale.loc 'character'.
#'   Position of the scale bar:
#'   \code{"bottomleft"}, \code{"topleft"}, \code{"topright"}, or \code{"bottomright"} to denote scale location,
#'   see \code{\link{AddScaleBar}} function.
#' @param arrow.loc 'character'.
#'   Position of the north arrow:
#'   \code{"bottomleft"}, \code{"topleft"}, \code{"topright"}, or \code{"bottomright"} to denote arrow location.
#' @param explanation 'character'.
#'   Label explaining the raster cell value.
#' @param credit 'character'.
#'   Label crediting the base map.
#' @param shade 'list'.
#'   If specified, a semi-transparent shade layer is drawn on top of the raster layer.
#'   This layer is described using a list of arguments supplied to the
#'   \code{\link[raster]{hillShade}} function.
#'   Passed arguments include \code{angle} and \code{direction}.
#'   Additional arguments also may be passed that control the vertical aspect ratio
#'   (\code{z.factor}) and color opacity (\code{alpha}).
#' @param contour.lines 'list'.
#'   If specified, contour lines are drawn.
#'   The contours are described using a list of arguments supplied to the \code{\link[raster]{contour}} function.
#'   Passed arguments include \code{drawlables}, \code{method}, and \code{col}.
#' @param rivers 'list'.
#'   If specified, lines are drawn.
#'   The lines are described using a list of arguments supplied to the plot method for
#'   class '\code{\link[=SpatialLines-class]{SpatialLines}}'.
#'   Passed arguments include \code{x}, \code{col}, and \code{lwd}.
#' @param lakes 'list'.
#'   If specified, polygons are drawn.
#'   The polygons are described using a list of arguments supplied to the plot method for
#'   class '\code{\link[=SpatialPolygons-class]{SpatialPolygons}}'.
#'   Passed arguments include \code{x}, \code{col}, \code{border}, and \code{lwd}.
#'   Bitmap images require a regular grid.
#' @param roads 'list'.
#'   If specified, lines are drawn.
#'   The lines are described using a list of arguments supplied to the plot method for
#'   class '\code{\link[=SpatialLines-class]{SpatialLines}}'.
#'   Passed arguments include \code{x}, \code{col}, and \code{lwd}.
#' @param draw.key 'logical'.
#'   If true, a color key should be drawn.
#' @param draw.raster 'logical'.
#'   If true, the raster image is drawn.
#' @param file 'character'.
#'   Name of the output file.
#'   Specifying this argument will start a graphics device driver for producing a
#'   PDF or PNG file format---the file extension determines the format type.
#'   The width and height of the graphics region will be automagically determined and
#'   included with the function's returned values, see "Value" section for details;
#'   these device dimensions can be useful when creating similar map layouts in dynamic reports.
#' @param close.file 'logical'.
#'   If true, the graphics device driver is shut down when the function exits.
#'   Unused if \code{file = NULL}
#' @param useRaster 'logical'.
#'   If true, a bitmap raster is used to plot \code{r} instead of using individual polygons for each raster cell.
#'   If \code{UseRaster} is not specified, raster images are used when the \code{\link{getOption}("preferRaster")} is true.
#'   Unused if \code{simplify = TRUE}.
#' @param simplify 'numeric'.
#'   Specifying this argument will convert the raster \code{r} to spatial polygons prior to plotting,
#'   see \code{\link{Grid2Polygons}} function for details.
#'   If \code{simplify > 0} the geometry of the spatial polygons is generalized using the
#'   Douglas-Peucker algorithm (Douglas and Peucker, 1961);
#'   and \code{simplify} is the numerical tolerance value to be used by the algorithm.
#'   See \code{gSimplify} function for additional information.
#'
#' @return Used for the side-effect of a new plot generated.
#'   Returns a 'list' object with the following graphical parameters:
#'   \describe{
#'     \item{din}{device dimensions \code{c(width, height)}, in inches.}
#'     \item{usr}{extremes of the coordinates of the plotting region \code{c(x1, x2, y1, y2)}.}
#'     \item{heights}{relative heights on the device \code{c(upper, lower)} for the map and color-key plots.}
#'   }
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @references
#'   Douglas, D., and Peucker, T., 1961,
#'   Algorithms for the reduction of the number of points required to represent a digitized line or its caricature:
#'   The Canadian Cartographer, v. 10, no. 2, p. 112--122.
#'
#' @seealso \code{\link{AddColorKey}}
#'
#' @keywords hplot
#'
#' @import rgdal
#'
#' @export
#'
#' @examples
#' r <- raster::raster(nrow = 10, ncol = 10, crs = NA)
#' r[] <- 1L
#' r[51:100] <- 2L
#' r[3:6, 1:5] <- 8L
#' r <- raster::ratify(r)
#' rat <- cbind(raster::levels(r)[[1]], land.cover = c("Pine", "Oak", "Meadow"))
#' levels(r) <- rat
#' PlotMap(r)
#'
#' m <- t(datasets::volcano)[61:1, ]
#' x <- seq(from = 6478705, length.out = 87, by = 10)
#' y <- seq(from = 2667405, length.out = 61, by = 10)
#' r <- raster::raster(m, xmn = min(x), xmx = max(x), ymn = min(y), ymx = max(y),
#'                     crs = "+init=epsg:27200")
#' PlotMap(r, pal = terrain.colors, scale.loc = "bottomright",
#'         explanation = "Topographic information on Auckland's Maunga Whau volcano.",
#'         credit = "Digitized from a topographic map by Ross Ihaka on a 10-m by 10-m grid.",
#'         shade = list(alpha = 0.3), contour.lines = list(col = "#1F1F1F"),
#'         useRaster = TRUE)
#'
#' r <- raster::raster(system.file("external/test.grd", package = "raster"))
#' Pal <- function(n) viridisLite::magma(n, begin = 0.3)
#' breaks <- seq(0, 2000, by = 200)
#' PlotMap(r, breaks = breaks, pal = Pal, dms.tick = TRUE, bg.lines = TRUE,
#'         contour.lines = list(col = "#1F1F1F"), draw.key = FALSE, simplify = 0)
#' AddScaleBar(unit = c("KILOMETER", "MILES"), conv.fact = c(0.001, 0.000621371), 
#'             loc = "bottomright", offset = c(-0.4, 0.1))
#' AddGradientLegend(breaks, Pal, at = breaks, title = "Explanation", loc = "topleft",
#'                   inset = 0.05, strip.dim = c(2, 20))
#'
#' out <- PlotMap(r, dms.tick = TRUE, file = "Rplots1.pdf")
#' print(out)
#'
#' pdf(file = "Rplots2.pdf", width = out$din[1], height = out$din[2])
#' PlotMap(r, dms.tick = TRUE)
#' data(meuse, package = "sp")
#' sp::coordinates(meuse) = ~ x + y
#' points(meuse)
#' dev.off()
#'
#' file.remove(c("Rplots1.pdf", "Rplots2.pdf"))
#' graphics.off()
#'

PlotMap <- function(r, layer=1, att=NULL, n=NULL, breaks=NULL,
                    xlim=NULL, ylim=NULL, zlim=NULL, asp=NULL,
                    extend.xy=FALSE, extend.z=FALSE,
                    reg.axs=TRUE, dms.tick=FALSE, bg.lines=FALSE, bg.image=NULL,
                    bg.image.alpha=1, pal=NULL, col=NULL, max.dev.dim=c(43, 56),
                    labels=NULL, scale.loc=NULL, arrow.loc=NULL, explanation=NULL,
                    credit=NULL, shade=NULL, contour.lines=NULL,
                    rivers=NULL, lakes=NULL, roads=NULL, draw.key=NULL, draw.raster=TRUE,
                    file=NULL, close.file=TRUE, useRaster, simplify) {

  if (!is.null(bg.image) && !inherits(bg.image, "RasterLayer"))
    stop("background image is invalid class")

  if (missing(useRaster)) {
    useRaster <- getOption("preferRaster")
    if (!is.logical(useRaster)) useRaster <- FALSE
  }

  if (inherits(r, c("RasterStack", "RasterBrick", "SpatialGrid", "SpatialPixelsDataFrame"))) {
    r <- raster::raster(r, layer=layer)
  } else if (inherits(r, "CRS")) {
    is.lim <- !is.null(xlim) && !is.null(ylim)
    if (!is.lim && is.null(bg.image)) stop("spatial limits must be specified")
    e <- raster::extent(if (is.lim) c(xlim, ylim) else bg.image)
    r <- raster::raster(e, crs=r)
    r[] <- NA
  }
  if (!inherits(r, "RasterLayer")) {
    r <- raster::raster(r)
    r[] <- NA
  }

  if (is.null(asp) && !is.na(rgdal::CRSargs(raster::crs(r)))) asp <- 1

  is.dms <- dms.tick && !is.na(rgdal::CRSargs(r@crs))

  if (raster::is.factor(r)) {
    att.tbl <- raster::levels(r)[[1]]
    ids <- att.tbl[, "ID"]
    att <- ifelse(is.null(att), ifelse(ncol(att.tbl) > 1, 2, 1), att)
    r.levels <- att.tbl[, att]
    rr <- r
    rr[] <- NA
    new.ids <- seq(along=ids)
    new.att.tbl <- as.data.frame(list(ID=new.ids, att=r.levels))
    suppressWarnings(levels(rr) <- list(new.att.tbl))
    for (i in new.ids) {
      rr[r == ids[i]] <- i
    }
    r <- rr
  }

  xl <- if (is.null(xlim)) c(NA, NA) else xlim
  yl <- if (is.null(ylim)) c(NA, NA) else ylim
  zl <- if (is.null(zlim)) c(NA, NA) else zlim
  e <- as.vector(raster::extent(r))
  if (!is.na(xl[1])) e[1] <- xl[1]
  if (!is.na(xl[2])) e[2] <- xl[2]
  if (!is.na(yl[1])) e[3] <- yl[1]
  if (!is.na(yl[2])) e[4] <- yl[2]
  r <- raster::crop(r, raster::extent(e), snap="near")

  if (all(is.na(r[]))) {
    n <- 0
  } else {
    zran <- range(r[], na.rm=TRUE)
    default.zl <- if (extend.z) range(pretty(zran, n=6)) else zran
    if (raster::is.factor(r)) {
      at1 <- raster::levels(r)[[1]][, "ID"]
      breaks <- c(0.5, at1 + 0.5)
      zl <- range(breaks, finite=TRUE)
    } else {
      if (all(is.na(zl)) && !is.null(breaks)) {
        zl <- range(breaks, finite=TRUE)
      } else {
        if (is.na(zl[1])) zl[1] <- default.zl[1]
        if (is.na(zl[2])) zl[2] <- default.zl[2]
      }
      if (is.null(breaks)) {
        if (is.null(n) || n > 200L) {
          breaks <- seq(zl[1], zl[2], length.out=200L)
          at1 <- pretty(if (extend.z) zran else zl, n=6)
        } else {
          breaks <- pretty(zl, n=n)
          at1 <- breaks
        }
      } else {
        at1 <- breaks
      }
    }
    n <- length(breaks) - 1L
    r[r[] < zl[1] | r[] > zl[2]] <- NA
  }

  if (!all(is.na(r[]))) r <- raster::trim(r)
  xran <- sp::bbox(r)[1, ]
  yran <- sp::bbox(r)[2, ]
  if (extend.xy) {
    default.xl <- range(pretty(xran))
    default.yl <- range(pretty(yran))
  } else {
    if (reg.axs) {
      aspect <- ifelse(is.null(asp), (diff(xran) / diff(yran)), asp)
      xbuf <- diff(xran) * 0.04
      ybuf <- xbuf / aspect
      default.xl <- c(xran[1] - xbuf, xran[2] + xbuf)
      default.yl <- c(yran[1] - ybuf, yran[2] + ybuf)
    } else {
      default.xl <- range(xran)
      default.yl <- range(yran)
    }
  }

  if (is.na(xl[1])) xl[1] <- default.xl[1]
  if (is.na(xl[2])) xl[2] <- default.xl[2]
  if (is.na(yl[1])) yl[1] <- default.yl[1]
  if (is.na(yl[2])) yl[2] <- default.yl[2]

  if (is.null(draw.key)) draw.key <- ifelse(n == 0, FALSE, TRUE)

  csi <- 0.2  # assumed line height in inches
  mai2 <- c(2, 3, 2, 2) * csi
  if (draw.key) {
    y1 <- csi
    FUN <- function(i) {if (is.null(i)) 0 else length(strsplit(i, "\n")[[1]])}
    mai2[1] <- (FUN(credit) + 1) * (csi * 0.6)
    mai1 <- c(1.2, 3, NA, 2) * csi
    mai1[3] <- FUN(explanation) * (csi * 0.7)
  } else {
    y1 <- 0
    mai1 <- c(0, 0, 0, 0)
  }

  inches.in.pica <- 1 / 6
  max.dev.dim <- max.dev.dim * inches.in.pica

  if (is.null(file)) {
    if (grDevices::dev.cur() == 1) grDevices::dev.new()
    dev.dim <- grDevices::dev.size()
    w <- dev.dim[1]
    h1 <- y1 + mai1[1] + mai1[3]
    h2 <- dev.dim[2] - h1
    h <- h1 + h2
  } else {
    w <- max.dev.dim[1]
    aspect <- ifelse(is.null(asp), (diff(xl) / diff(yl)), asp)
    repeat {
      x2 <- w - mai2[2] - mai2[4]
      y2 <- x2 * (diff(yl * aspect) / diff(xl))
      h2 <- y2 + mai2[1] + mai2[3]
      h1 <- y1 + mai1[1] + mai1[3]
      h <- h1 + h2
      if (h > max.dev.dim[2]) w <- w - 0.01 else break
    }
    ext <- tolower(tools::file_ext(file))
    if (ext == "pdf") {
      grDevices::pdf(file, width=w, height=h)
    } else if (ext == "png") {
      grDevices::png(file, width=w * 100, height=h * 100, res=100)
    } else {
      stop("file argument does not have a valid extension")
    }
    if (close.file) on.exit(grDevices::dev.off())
  }

  if (draw.key) {
    cm.in.inches <- 2.54
    heights <- c(h2/ h, graphics::lcm(h1 * cm.in.inches))
    graphics::layout(matrix(c(2, 1), nrow=2, ncol=1), heights=heights)
  } else {
    graphics::layout(matrix(1, nrow=1, ncol=1))
  }

  lwd <- 0.5
  cex <- 0.7
  tcl <- 0.1 / graphics::par("csi")  # length for major ticks is 0.1 inches

  if (is.character(col)) {
    if (length(col) != n) stop("number of specified colors is incorrect")
    cols <- col
  } else {
    if (n == 0) {
      cols <- as.character()
    } else if (is.function(pal)) {
      cols <- pal(n)
    } else if (raster::is.factor(r)) {
      cols <- GetTolColors(n)
    } else {
      if (requireNamespace("viridisLite", quietly=TRUE))
        cols <- viridisLite::viridis(n)
      else
        cols <- grDevices::rainbow(n, start=0.0, end=0.8)
    }
  }
  if (!all(.AreColors(cols))) stop("colors are not valid")

  # plot color key
  if (draw.key & n > 0) {
    at <- if (is.null(labels$at)) at1 else labels$at
    if (is.null(labels$labels))
      labels <- if (raster::is.factor(r)) raster::levels(r)[[1]][, "att"] else TRUE
    else
      labels <- labels$labels
    AddColorKey(mai=mai1, is.categorical=raster::is.factor(r),
                breaks=breaks, col=cols, at=at, labels=labels,
                explanation=explanation)
  } else if (draw.key) {
    op <- graphics::par(mai=c(0, 0, 0, 0))
    graphics::plot.new()
    graphics::par(op)
  }

  # plot map
  graphics::par(mai=mai2)
  graphics::plot(NA, type="n", xlim=xl, ylim=yl, xaxs="i", yaxs="i", bty="n",
                 xaxt="n", yaxt="n", xlab="", ylab="", asp=asp)
  usr <- graphics::par("usr")

  if (is.null(file)) {
    xl <- usr[1:2]
    yl <- usr[3:4]
  }

  if (is.dms) {
    al <- list()
    al[[1]] <- sp::Lines(list(sp::Line(rbind(c(xl[1], yl[1]),
                                             c(xl[2], yl[1])))), ID="al1")
    al[[2]] <- sp::Lines(list(sp::Line(rbind(c(xl[1], yl[1]),
                                             c(xl[1], yl[2])))), ID="al2")
    al[[3]] <- sp::Lines(list(sp::Line(rbind(c(xl[1], yl[2]),
                                             c(xl[2], yl[2])))), ID="al3")
    al[[4]] <- sp::Lines(list(sp::Line(rbind(c(xl[2], yl[1]),
                                             c(xl[2], yl[2])))), ID="al4")
    sl <- sp::SpatialLines(al, proj4string=r@crs)
    sl.dd <- sp::spTransform(sl, sp::CRS("+init=epsg:4326"))
    e.dd <- pretty(range(sp::bbox(sl.dd)[1, ]))
    n.dd <- pretty(range(sp::bbox(sl.dd)[2, ]))
    grd.dd <- sp::gridlines(sl.dd, easts=e.dd, norths=n.dd, ndiscr=1000)

    pts.dd <- rgeos::gIntersection(sl.dd, grd.dd, byid=TRUE)
    ids <- row.names(pts.dd)

    row.names(pts.dd) <- make.names(ids, unique=TRUE)
    pts <- sp::spTransform(pts.dd, r@crs)

    at2 <- list()
    at2[[1]] <- as.vector(sp::coordinates(pts[ids == "al1 NS", ])[, 1])
    at2[[2]] <- as.vector(sp::coordinates(pts[ids == "al2 EW", ])[, 2])
    at2[[3]] <- as.vector(sp::coordinates(pts[ids == "al3 NS", ])[, 1])
    at2[[4]] <- as.vector(sp::coordinates(pts[ids == "al4 EW", ])[, 2])

    xlabs <- .FormatDMS(sp::dd2dms(sp::coordinates(pts.dd[ids == "al3 NS", ])[, 1], NS=FALSE))
    ylabs <- .FormatDMS(sp::dd2dms(sp::coordinates(pts.dd[ids == "al2 EW", ])[, 2], NS=TRUE))
  } else {
    at2 <- list()
    at2[[1]] <- pretty(xl)
    at2[[2]] <- pretty(yl)
    at2[[3]] <- at2[[1]]
    at2[[4]] <- at2[[2]]
    scipen <- getOption("scipen", default=0)
    xlabs <- ToScientific(at2[[1]], scipen=scipen, type="plotmath")
    ylabs <- ToScientific(at2[[2]], scipen=scipen, type="plotmath")
    if (extend.xy) ylabs[length(ylabs)] <- ""
  }

  if (bg.lines) {
    graphics::rect(xleft=usr[1], ybottom=usr[3], xright=usr[2], ytop=usr[4],
                   col="#E7E7E7", border=NA)
    if (is.dms) {
      sp::plot(sp::spTransform(grd.dd, r@crs), lwd=lwd, col="#FFFFFF", add=TRUE)
    } else {
      graphics::abline(v=at2[[1]], lwd=lwd, col="#FFFFFF")
      graphics::abline(h=at2[[2]], lwd=lwd, col="#FFFFFF")
      graphics::abline(v=.LocateMidTicks(1), lwd=(lwd / 2), col="#FFFFFF59")
      graphics::abline(h=.LocateMidTicks(2), lwd=(lwd / 2), col="#FFFFFF59")
    }
  }

  if (!is.null(bg.image)) {
    bg.image <- raster::crop(bg.image, raster::extent(graphics::par("usr")), snap="out")
    if (!is.null(bg.image))
      raster::image(bg.image, maxpixels=length(bg.image), useRaster=TRUE,
                    col=grDevices::grey(0:255 / 255, alpha=bg.image.alpha), add=TRUE)
  }

  if (draw.raster & n > 0) {
    if (!missing(simplify)) {
      ply <- Grid2Polygons(r, level=TRUE, at=breaks, zlim=zl)
      if (simplify > 0) {
        simple.ply <- rgeos::gSimplify(ply, tol=simplify, topologyPreserve=TRUE)
        ids <- sapply(methods::slot(simple.ply, "polygons"), function(i) {
          methods::slot(i, "ID")
        })
        ply <- sp::SpatialPolygonsDataFrame(simple.ply, data=ply[ids, ]@data)
      }
      sp::plot(ply, col=cols, border=NA, add=TRUE)
    } else {
      raster::image(r, maxpixels=length(r), useRaster=useRaster, zlim=zl,
                    col=cols, add=TRUE, breaks=breaks)
    }
    if (is.list(shade)) {
      zfact <- as.numeric(shade[["z.factor"]])
      angle <- as.numeric(shade[["angle"]])
      direc <- as.numeric(shade[["direction"]])
      alpha <- as.numeric(shade[["alpha"]])
      zfact <- ifelse(length(zfact) == 1 && !is.na(zfact), zfact, 1)
      angle <- ifelse(length(angle) == 1 && !is.na(angle), angle, 45)
      direc <- ifelse(length(direc) == 1 && !is.na(direc), direc, 0)
      alpha <- ifelse(length(alpha) == 1 && !is.na(alpha), alpha, 1)
      rr <- r * zfact
      hs <- raster::hillShade(slope=raster::terrain(rr),
                              aspect=raster::terrain(rr, opt="aspect"),
                              angle=angle, direction=direc)
      raster::image(hs, maxpixels=length(hs), useRaster=TRUE,
                    col=grDevices::grey(0:255 / 255, alpha=alpha), add=TRUE)
    }
  }

  if (is.list(rivers)) {
    river <- sp::spTransform(rivers[["x"]], r@crs)
    river <- raster::crop(river, raster::extent(graphics::par("usr")))
    if (!is.null(river)) {
      color <- as.character(rivers[["col"]])
      width <- as.numeric(rivers[["lwd"]])
      color <- ifelse(length(color) == 1 && .AreColors(color), color, "#3399CC")
      width <- ifelse(length(width) == 1 && !is.na(width), width, 0.5)
      sp::plot(river, col=color, lwd=width, add=TRUE)
    }
  }

  if (is.list(lakes)) {
    lake <- sp::spTransform(lakes[["x"]], r@crs)
    lake <- raster::crop(lake, raster::extent(graphics::par("usr")))
    if (!is.null(lake)) {
      color <- as.character(lakes[["col"]])
      bordr <- as.character(lakes[["border"]])
      width <- as.numeric(lakes[["lwd"]])
      color <- ifelse(length(color) == 1 && .AreColors(color), color, "#CCFFFF")
      bordr <- ifelse(length(bordr) == 1 && .AreColors(bordr), bordr, "#3399CC")
      width <- ifelse(length(width) == 1 && !is.na(width), width, 0.5)
      sp::plot(lake, col=color, border=bordr, lwd=width, add=TRUE)
    }
  }

  if (is.list(roads)) {
    road <- sp::spTransform(roads[["x"]], r@crs)
    road <- raster::crop(road, raster::extent(graphics::par("usr")))
    if (!is.null(roads)) {
      color <- as.character(roads[["col"]])
      width <- as.numeric(roads[["lwd"]])
      color <- ifelse(length(color) == 1 && .AreColors(color), color, "#666666")
      width <- ifelse(length(width) == 1 && !is.na(width), width, 0.25)
      sp::plot(road, col=color, lwd=width, add=TRUE)
    }
  }

  if (is.list(contour.lines)) {
    color <- as.character(contour.lines[["col"]])
    drawl <- as.logical(contour.lines[["drawlabels"]])
    metho <- as.character(contour.lines[["method"]])
    color <- ifelse(length(color) == 1 && !is.na(color), color, "#1F1F1F")
    drawl <- ifelse(length(drawl) == 1 && !is.na(drawl), drawl, TRUE)
    metho <- ifelse(length(metho) == 1 && !is.na(metho), metho, "flattest")
    contour.breaks <- if (n + 1L > 20L) pretty(zl, 20L) else breaks
    raster::contour(r, maxpixels=length(r), levels=contour.breaks,
                    labels=formatC(contour.breaks, big.mark=","),
                    xlim=xl, ylim=yl, zlim=zl, labcex=0.5, drawlabels=drawl,
                    method=metho, axes=FALSE, col=color, lwd=lwd, add=TRUE)
  }

  graphics::axis(1, at=at2[[1]], labels=FALSE, lwd=-1, lwd.ticks=lwd, tcl=tcl,
                 cex.axis=cex)

  if (is.dms)
    graphics::axis(2, at=at2[[2]], labels=ylabs, lwd=-1, lwd.ticks=lwd, hadj=0,
                   tcl=tcl, cex.axis=cex, las=1, mgp=c(3, 1.1, 0))
  else
    graphics::axis(2, at=at2[[2]], labels=ylabs, lwd=-1, lwd.ticks=lwd,
                   tcl=tcl, cex.axis=cex, mgp=c(3, 0.2, 0))
  graphics::axis(3, at=at2[[3]], labels=xlabs, lwd=-1, lwd.ticks=lwd,
                 tcl=tcl, cex.axis=cex, mgp=c(3, 0.2, 0))
  graphics::axis(4, at=at2[[4]], labels=FALSE, lwd=-1, lwd.ticks=lwd, tcl=tcl, cex.axis=cex)
  graphics::box(lwd=lwd)

  if (is.character(credit)) {
    line <- -1 * graphics::strheight("M", "inches", cex=1) / csi
    graphics::mtext(credit, side=1, line=line, padj=1, adj=0, cex=0.6, col="#C0C0C0")
  }

  if (!is.null(scale.loc)) {
    txt <- strsplit(sp::proj4string(r), " ")[[1]]
    unit <- sub("^\\+units=", "", grep("^\\+units=", txt, value=TRUE))
    longlat <- "+proj=longlat" %in% txt
    AddScaleBar(unit=unit, longlat=longlat, loc=scale.loc)
  }

  if (!is.null(arrow.loc)) .AddNorthArrow(arrow.loc, r@crs, cex)

  invisible(list(din=graphics::par("din"), usr=usr, heights=c(h2, h1) / h))
}


.LocateMidTicks <- function(side) {
  usr <- graphics::par("usr")
  ran <- if (side %in% c(1, 3)) usr[1:2] else usr[3:4]
  at <- graphics::axTicks(side)
  inc <- diff(range(at)) / (length(at) - 1)
  at.mid <- seq(min(at) - inc, max(at) + inc, by=inc / 2)
  at.mid <- at.mid[!at.mid %in% at & at.mid > ran[1] & at.mid < ran[2]]
  return(at.mid)
}


.AddNorthArrow <- function(loc, crs, cex) {
  crs.dd <- sp::CRS("+init=epsg:4326")
  usr <- graphics::par("usr")
  x.mid <- (usr[2] + usr[1]) / 2
  y.mid <- (usr[4] + usr[3]) / 2
  d <- 0.05 * (usr[4] - usr[3])
  xy <- rbind(c(x.mid, y.mid), c(x.mid, y.mid + d))
  sp.dd <- sp::spTransform(sp::SpatialPoints(xy, proj4string=crs), crs.dd)
  dd <- sp.dd@coords
  d.dd <- sqrt((dd[2, 1] - dd[1, 1])^2 + (dd[2, 2] - dd[1, 2])^2)
  dd <- rbind(dd[1, ], c(dd[1, 1],  dd[1, 2] + d.dd))
  sp.xy <- sp::spTransform(sp::SpatialPoints(dd, proj4string=crs.dd), crs)
  xy <- sp.xy@coords
  padx <- 0.1 * (usr[2] - usr[1])
  pady <- 0.1 * (usr[4] - usr[3])
  if (loc %in% c("bottomleft", "topleft"))
    x0 <- usr[1] + padx
  else
    x0 <- usr[2] - padx
  if (loc %in% c("bottomleft", "bottomright"))
    y0 <- usr[3] + pady
  else
    y0 <- usr[4] - pady
  x1 <- xy[2, 1] + x0 - xy[1, 1]
  y1 <- xy[2, 2] + y0 - xy[1, 2]
  a <- atan((y1 - y0) / (x1 - x0)) * (180 / pi)
  if (a > 45 && a <= 135) {
    pos <- 3
  } else if (a > 135 && a <= 225) {
    pos <- 2
  } else if (a > 225 && a <= 315) {
    pos <- 1
  } else {
    pos <- 4
  }
  graphics::arrows(x0, y0, x1, y1, length=0.1)
  graphics::text(x1, y1, labels="N", pos=pos, cex=cex)
}


.FormatDMS <- function(dms) {
  d <- dms@deg
  m <- dms@min
  s <- dms@sec
  s.round <- round(s, digits=3)
  sep <- ifelse(dms@NS, "\n", "")
  txt <- paste0(d, "\u00B0")
  txt <- paste0(txt, ifelse(m != 0 | s.round != 0, paste0(sep, m, "'"), ""))
  txt <- paste0(txt, ifelse(s.round != 0, paste0(sep, s.round, "\""), ""))
  return(txt)
}


.AreColors <- function(x) {
  sapply(x, function(i) tryCatch({
    is.matrix(grDevices::col2rgb(i))
  }, error=function(e) FALSE))
}
