#' Plot Method for Maps
#'
#' This function maps raster layer values.
#' A key showing how the colors map to raster values is shown below the map.
#' The width and height of the graphics region will be automagically determined in some cases.
#'
#' @param r RasterLayer, SpatialGridDataFrame, or CRS.
#'   A raster layer with values to be plotted or a coordinate reference system (CRS).
#' @param layer integer.
#'   Column to use in the SpatialGridDataFrame.
#' @param att numeric or character.
#'   Variable identifying the levels attribute to use in the Raster Attribute Table (RAT).
#'   This argument requires \code{r} values that are of class factor.
#' @param n integer.
#'   Desired number of intervals to partition the range of raster values (or \code{zlim} if specified) (optional).
#' @param breaks numeric.
#'   Vector of break points used to partition the colors representing numeric raster values (optional).
#' @param xlim numeric.
#'   Vector of length 2 giving the minimum and maximum values for the \emph{x}-axis.
#' @param ylim numeric.
#'   Vector of length 2 giving the minimum and maximum values for the \emph{y}-axis.
#' @param zlim numeric.
#'   Vector of length 2 giving the minimum and maximum raster values for which colors should be plotted.
#' @param asp numeric.
#'   The \emph{y/x} aspect ratio for spatial axes.
#' @param extend.xy logical.
#'   If true, the spatial limits will be extended to the next tick mark on the axes beyond the grid extent.
#' @param extend.z logical.
#'   If true, the raster value limits will be extended to the next tick mark on the color key beyond the measured range.
#' @param reg.axs logical.
#'   If true, the spatial data range is extended.
#' @param trim.r logical.
#'   If true, the outer rows and columns that consist of all NA values will be removed.
#' @param dms.tick logical.
#'   If true, the axes tickmarks are specified in degrees, minutes, and decimal seconds.
#' @param bg.lines logical.
#'   If true, the graticule is drawn in back of the raster layer using white lines and a grey background.
#' @param bg.image RasterLayer.
#'   An image to drawn in back of the main raster layer \code{r}.
#' @param bg.image.alpha numeric.
#'   Opacity of the background image from 0 to 1.
#' @param pal function.
#'   Color palette to be used to assign colors in the plot, rainbow by default.
#' @param col character.
#'   Vector of colors to be used in the plot.
#'   This argument requires \code{breaks} specification for numeric values of \code{r} and
#'   overrides any palette function specification.
#'   For numeric values there should be one less color than breaks.
#'   Factors require a color for each level.
#' @param max.dev.dim numeric.
#'   Vector of length 2 giving the maximum width and height for the graphics device in picas, respectively.
#'   Suggested dimensions for single-column, double-column, and sidetitle figures are
#'   \code{c(21, 56)}, \code{c(43, 56)}, and \code{c(56, 43)}, respectively.
#'   This argument is only applicable when the \code{file} argument is specified.
#' @param labels list.
#'   Describes the location and values of labels in the color key.
#'   This list may include components \code{at} and \code{labels}.
#' @param scale.loc character.
#'   Position of the scale bar:
#'   "bottomleft", "topleft", "topright", or "bottomright" to denote scale location.
#' @param arrow.loc character.
#'   Position of the north arrow:
#'   "bottomleft", "topleft", "topright", or "bottomright" to denote arrow location.
#' @param explanation character.
#'   Label explaining the raster cell value.
#' @param credit character.
#'   Label crediting the base map.
#'   By default, the character string describing the raster layers (\code{r}) projection and datum in the PROJ.4 format.
#' @param shade list.
#'   If specified, a semi-transparent shade layer is drawn on top of the raster layer.
#'   This layer is described using a list of arguments supplied to \code{raster::hillShade} function.
#'   Passed arguments include \code{"angle"} and \code{"direction"}.
#'   Additional arguments also may be passed that control the vertical aspect ratio
#'   (\code{"z.factor"}) and color opacity (\code{"alpha"}).
#' @param contour.lines list.
#'   If specified, contour lines are drawn.
#'   The contours are described using a list of arguments supplied to \code{contour}.
#'   Passed arguments include \code{"drawlables"}, \code{"method"}, and \code{"col"}.
#' @param rivers list.
#'   If specified, lines are drawn.
#'   The lines are described using a list of arguments supplied to the plot method for SpatialLines.
#'   Passed arguments include \code{"x"}, \code{"col"}, and \code{"lwd"}.
#' @param lakes list.
#'   If specified, polygons are drawn.
#'   The polygons are described using a list of arguments supplied to the plot method for SpatialPolygons.
#'   Passed arguments include \code{"x"}, \code{"col"}, \code{"border"}, and \code{"lwd"}.
#'   Bitmap images require a regular grid.
#' @param roads list.
#'   If specified, lines are drawn.
#'   The lines are described using a list of arguments supplied to the plot method for SpatialLines.
#'   Passed arguments include \code{"x"}, \code{"col"}, and \code{"lwd"}.
#' @param draw.key logical.
#'   If true, a color key should be drawn.
#' @param draw.raster logical.
#'   If true, the raster image is drawn.
#' @param file character.
#'   Name of the output file.
#'   Specifying this argument will start a graphics device driver for producing a
#'   PDF or PNG file format---the file extension determines the format type.
#'   The width and height of the graphics region will be automagically determined and
#'   included with the function's returned values, see "Value" section for details.
#'   These graphical parameters can be used when creating similar map layouts in dynamic reports
#'   or whenever further elements will be added to an existing plot generated by this function
#'   (possible when \code{file = NULL}).
#' @param useRaster logical.
#'   If true, a bitmap raster is used to plot \code{r} instead of using polygons.
#'   If \code{UseRaster} is not specified, raster images are used when the \code{getOption("preferRaster")} is true.
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
#' @seealso \code{\link{AddScaleBar}}, \code{\link{AddColorKey}}
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
#' r <- raster::raster(nrow = 10, ncol = 10)
#' r[] <- 1L
#' r[51:100] <- 2L
#' r[3:6, 1:5] <- 8L
#' r <- raster::ratify(r)
#' rat <- raster::levels(r)[[1]]
#' rat$land.cover <- c("Pine", "Oak", "Meadow")
#' rat$code <- c(12, 25, 30)
#' levels(r) <- rat
#' PlotMap(r, att = "land.cover", col = c("grey", "orange", "purple"))
#' PlotMap(r, att = "code")
#'
#' r <- raster::raster(system.file("external/test.grd", package="raster"))
#' credit <- "Label crediting the base map."
#' explanation <- "Label explaining the raster cell value."
#' PlotMap(r, scale.loc = "topleft", dms.tick = TRUE, trim.r = TRUE,
#'         credit = credit, explanation = explanation)
#' data(meuse, package = "sp")
#' sp::coordinates(meuse) = ~ x + y
#' points(meuse)
#'
#' val <- PlotMap(r, scale.loc = "topleft", dms.tick = TRUE, trim.r = TRUE,
#'                credit = credit, explanation = explanation,
#'                file = "Rplots1.pdf")
#' print(val)
#'
#' pdf(file = "Rplots2.pdf", width = val$din[1], height = val$din[2])
#' PlotMap(r, scale.loc = "topleft", dms.tick = TRUE, trim.r = TRUE,
#'         credit = credit, explanation = explanation)
#' points(meuse)
#' dev.off()
#'
#' file.remove(c("Rplots1.pdf", "Rplots2.pdf"))
#' graphics.off()
#'

PlotMap <- function(r, layer=1, att=NULL, n, breaks, xlim=NULL, ylim=NULL,
                    zlim=NULL, asp=1, extend.xy=FALSE, extend.z=FALSE,
                    reg.axs=TRUE, trim.r=TRUE, dms.tick=FALSE, bg.lines=FALSE,
                    bg.image=NULL, bg.image.alpha=1, pal=NULL, col=NULL,
                    max.dev.dim=c(43, 56), labels=NULL, scale.loc="bottomleft",
                    arrow.loc=NULL, explanation=NULL, credit=proj4string(r),
                    shade=NULL, contour.lines=NULL, rivers=NULL, lakes=NULL,
                    roads=NULL, draw.key=NULL, draw.raster=TRUE, file=NULL,
                    useRaster) {

  if (!is.null(bg.image) && !inherits(bg.image, "RasterLayer"))
    stop("background image is the incorrect class")

  if (missing(useRaster)) {
    useRaster <- getOption("preferRaster")
    if (!is.logical(useRaster)) useRaster <- FALSE
  }

  if (inherits(r, "CRS")) {
    is.lim <- is.numeric(xlim) && length(xlim) == 2 && all(!is.na(xlim)) &&
              is.numeric(ylim) && length(ylim) == 2 && all(!is.na(ylim))
    if (!is.lim && is.null(bg.image))
      stop("spatial limits must be specified")
    e <- raster::extent(if (is.lim) c(xlim, ylim) else bg.image)
    r <- raster(e, crs=r)
    r[] <- NA
  }

  if (inherits(r, "SpatialGridDataFrame"))
    r <- raster(r, layer=layer)
  if (!inherits(r, "RasterLayer"))
    stop("raster layer is the incorrect class")

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

  if (is.null(xlim))
    xlim <- c(NA, NA)
  if (is.null(ylim))
    ylim <- c(NA, NA)
  if (is.null(zlim))
    zlim <- c(NA, NA)

  e <- as.vector(extent(r))
  if (!is.na(xlim[1]))
    e[1] <- xlim[1]
  if (!is.na(xlim[2]))
    e[2] <- xlim[2]
  if (!is.na(ylim[1]))
    e[3] <- ylim[1]
  if (!is.na(ylim[2]))
    e[4] <- ylim[2]
  r <- crop(r, extent(e))

  if (trim.r && !all(is.na(r[])))
    r <- trim(r)

  xran <- bbox(r)[1, ]
  yran <- bbox(r)[2, ]

  if (extend.xy) {
    default.xlim <- range(pretty(xran))
    default.ylim <- range(pretty(yran))
  } else {
    if (reg.axs) {
      buf <- diff(xran) * 0.04
      default.xlim <- c(xran[1] - buf, xran[2] + buf)
      default.ylim <- c(yran[1] - (buf * asp), yran[2] + (buf * asp))
    } else {
      default.xlim <- range(xran)
      default.ylim <- range(yran)
    }
  }

  if (is.na(xlim[1]))
    xlim[1] <- default.xlim[1]
  if (is.na(xlim[2]))
    xlim[2] <- default.xlim[2]
  if (is.na(ylim[1]))
    ylim[1] <- default.ylim[1]
  if (is.na(ylim[2]))
    ylim[2] <- default.ylim[2]

  zran <- range(r[], finite=TRUE)
  if (anyNA(zran)) {
    n <- 0
  } else {
    default.zlim <- range(if (extend.z) pretty(zran) else zran)
    if (raster::is.factor(r)) {
      at1 <- raster::levels(r)[[1]][, "ID"]
      breaks <- c(0.5, at1 + 0.5)
      zlim <- range(breaks)
    } else {
      if (all(is.na(zlim))) {
        if (missing(breaks)) {
          if (missing(n))
            zlim <- if (extend.z) range(pretty(zran)) else zran
        } else {
          zlim <- range(breaks)
        }
      } else {
        if (is.na(zlim[1]))
          zlim[1] <- default.zlim[1]
        if (is.na(zlim[2]))
          zlim[2] <- default.zlim[2]
      }
      if (missing(breaks)) {
        if (missing(n) || n > 200L) {
          breaks <- seq(zlim[1], zlim[2], length.out=200L)
          at1 <- if (extend.z) pretty(zran) else pretty(zlim)
        } else {
          if (all(is.na(zlim))) {
            breaks <- pretty(zran, n=n)
            zlim <- if (extend.z) range(breaks) else zran
          } else {
            breaks <- pretty(zlim, n=n)
          }
          at1 <- breaks
        }
      } else {
        at1 <- breaks
      }
    }
    n <- length(breaks) - 1L
  }

  if (!is.logical(draw.key)) draw.key <- if (n == 0) FALSE else TRUE

  if (dms.tick) {
    al <- list()
    al[[1]] <- Lines(list(Line(rbind(c(xlim[1], ylim[1]),
                                     c(xlim[2], ylim[1])))), ID="al1")
    al[[2]] <- Lines(list(Line(rbind(c(xlim[1], ylim[1]),
                                     c(xlim[1], ylim[2])))), ID="al2")
    al[[3]] <- Lines(list(Line(rbind(c(xlim[1], ylim[2]),
                                     c(xlim[2], ylim[2])))), ID="al3")
    al[[4]] <- Lines(list(Line(rbind(c(xlim[2], ylim[1]),
                                     c(xlim[2], ylim[2])))), ID="al4")
    sl <- SpatialLines(al, proj4string=r@crs)
    sl.dd <- spTransform(sl, CRS("+proj=longlat +datum=WGS84"))
    e.dd <- pretty(range(bbox(sl.dd)[1, ]))
    n.dd <- pretty(range(bbox(sl.dd)[2, ]))
    grd.dd <- gridlines(sl.dd, easts=e.dd, norths=n.dd, ndiscr=1000)

    pts.dd <- rgeos::gIntersection(sl.dd, grd.dd, byid=TRUE)
    ids <- row.names(pts.dd)

    row.names(pts.dd) <- make.names(ids, unique=TRUE)
    pts <- spTransform(pts.dd, r@crs)

    at2 <- list()
    at2[[1]] <- as.vector(coordinates(pts[ids == "al1 NS", ])[, 1])
    at2[[2]] <- as.vector(coordinates(pts[ids == "al2 EW", ])[, 2])
    at2[[3]] <- as.vector(coordinates(pts[ids == "al3 NS", ])[, 1])
    at2[[4]] <- as.vector(coordinates(pts[ids == "al4 EW", ])[, 2])

    xlabs <- .FormatDMS(dd2dms(coordinates(pts.dd[ids == "al3 NS", ])[, 1],
                               NS=FALSE))
    ylabs <- .FormatDMS(dd2dms(coordinates(pts.dd[ids == "al2 EW", ])[, 2],
                               NS=TRUE))
  } else {
    at2 <- list()
    at2[[1]] <- pretty(xlim)
    at2[[2]] <- pretty(ylim)
    at2[[3]] <- at2[[1]]
    at2[[4]] <- at2[[2]]
    xlabs <- prettyNum(at2[[1]])
    ylabs <- prettyNum(at2[[2]])
    if (extend.xy)
     ylabs[length(ylabs)] <- ""
  }

  inches.in.pica <- 1 / 6

  mar2 <- c(1, 3, 2, 2)
  if (is.character(credit)) mar2[1] <- 2
  if (draw.key) {
    y1 <- 1
    mar1 <- c(2, 3, 1, 2)
  } else {
    y1 <- 0
    mar1 <- c(0, 0, 0, 0)
  }

  if (is.null(file)) {
    if (grDevices::dev.cur() == 1) grDevices::dev.new()
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
    ext <- tolower(tools::file_ext(file))
    if (ext == "pdf") {
      grDevices::pdf(file, width=wi, height=hi)
    } else if (ext == "png") {
      grDevices::png(file, width=wi * 100, height=hi * 100, res=100)
    } else {
      stop("file argument does not have a valid extension")
    }
    on.exit(grDevices::dev.off())
  }

  if (draw.key)
    graphics::layout(matrix(c(2, 1), nrow=2, ncol=1), heights=c(h2, h1) / h)
  else
    graphics::layout(matrix(1, nrow=1, ncol=1))

  lwd <- 0.5  # line weight
  cex <- 0.7  # magnification of text relative to the default
  tcl <- 7.2 / graphics::par("cra")[2]  # length of ticks, fraction of text height

  if (is.character(col)) {
    if (length(col) != n) stop("number of specified colors is incorrect")
    cols <- col
  } else {
    if (is.function(pal)) {
      cols <- pal(n)
    } else {
      if (requireNamespace("colorspace", quietly=TRUE))
        cols <- colorspace::rainbow_hcl(n, start=0.0, end=(360 * (n - 1) / n) * 0.8)
      else
        cols <- grDevices::rainbow(n, start=0.0, end=0.8)
    }
  }
  if (!all(.AreColors(cols)))
    stop("colors are not valid")

  # Plot color key

  if (draw.key & n > 0) {
    is.categorical <- raster::is.factor(r)
    at <- if (is.null(labels$at)) at1 else labels$at
    if (is.null(labels$labels))
      labels <- if (is.categorical) raster::levels(r)[[1]][, "att"] else TRUE
    else
      labels <- labels$labels
    AddColorKey(mai=mar1 * inches.in.pica, is.categorical=raster::is.factor(r),
                breaks=breaks, col=cols, at=at, labels=labels,
                explanation=explanation)
  } else if (draw.key) {
    op <- graphics::par(mar=c(0, 0, 0, 0))
    graphics::plot.new()
    graphics::par(op)
  }

  # Plot map

  graphics::par(mai=mar2 * inches.in.pica)
  plot(NA, type="n", xlim=xlim, ylim=ylim, xaxs="i", yaxs="i", bty="n",
       xaxt="n", yaxt="n", xlab="", ylab="", asp=asp)

  if (bg.lines) {
    usr <- graphics::par()$usr
    graphics::rect(xleft=usr[1], ybottom=usr[3], xright=usr[2], ytop=usr[4],
                   col="#E7E7E7", border=NA)
    if (dms.tick) {
      plot(spTransform(grd.dd, r@crs), lwd=lwd, col="#FFFFFF", add=TRUE)
    } else {
      graphics::abline(v=at2[[1]], lwd=lwd, col="#FFFFFF")
      graphics::abline(h=at2[[2]], lwd=lwd, col="#FFFFFF")
      graphics::abline(v=.LocateMidTicks(1), lwd=(lwd / 2), col="#FFFFFF59")
      graphics::abline(h=.LocateMidTicks(2), lwd=(lwd / 2), col="#FFFFFF59")
    }
  }

  if (!is.null(bg.image)) {
    bg.image <- crop(bg.image, extent(e), snap="out")
    if (!is.null(bg.image))
      raster::image(bg.image, maxpixels=length(bg.image), useRaster=TRUE,
                    col=grDevices::grey(0:255 / 255, alpha=bg.image.alpha), add=TRUE)
  }

  if (draw.raster & n > 0) {
    raster::image(r, maxpixels=length(r), useRaster=useRaster, zlim=zlim,
                  col=cols, add=TRUE, breaks=breaks)
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
      hs <- hillShade(slope=terrain(rr), aspect=terrain(rr, opt="aspect"),
                      angle=angle, direction=direc)
      raster::image(hs, maxpixels=length(hs), useRaster=TRUE,
                    col=grDevices::grey(0:255 / 255, alpha=alpha), add=TRUE)
    }
  }

  if (is.list(rivers)) {
    river <- spTransform(rivers[["x"]], r@crs)
    river <- crop(river, extent(e))
    if (!is.null(rivers)) {
      color <- as.character(rivers[["col"]])
      width <- as.numeric(rivers[["lwd"]])
      color <- ifelse(length(color) == 1 && .AreColors(color), color, "#3399CC")
      width <- ifelse(length(width) == 1 && !is.na(width), width, 0.5)
      plot(river, col=color, lwd=width, add=TRUE)
    }
  }

  if (is.list(lakes)) {
    lakes <- spTransform(lakes[["x"]], r@crs)
    lakes <- crop(lakes, extent(e))
    if (!is.null(lakes)) {
      color <- as.character(lakes[["col"]])
      bordr <- as.character(lakes[["border"]])
      width <- as.numeric(lakes[["lwd"]])
      color <- ifelse(length(color) == 1 && .AreColors(color), color, "#CCFFFF")
      bordr <- ifelse(length(bordr) == 1 && .AreColors(bordr), bordr, "#3399CC")
      width <- ifelse(length(width) == 1 && !is.na(width), width, 0.5)
      plot(lakes, col=color, border=bordr, lwd=width, add=TRUE)
    }
  }

  if (is.list(roads)) {
    road <- spTransform(roads[["x"]], r@crs)
    road <- crop(road, extent(e))
    if (!is.null(roads)) {
      color <- as.character(roads[["col"]])
      width <- as.numeric(roads[["lwd"]])
      color <- ifelse(length(color) == 1 && .AreColors(color), color, "#666666")
      width <- ifelse(length(width) == 1 && !is.na(width), width, 0.25)
      plot(road, col=color, lwd=width, add=TRUE)
    }
  }

  if (is.list(contour.lines)) {
    color <- as.character(contour.lines[["col"]])
    drawl <- as.logical(contour.lines[["drawlabels"]])
    metho <- as.character(contour.lines[["method"]])
    color <- ifelse(length(color) == 1 && !is.na(color), color, "#1F1F1F")
    drawl <- ifelse(length(drawl) == 1 && !is.na(drawl), drawl, TRUE)
    metho <- ifelse(length(metho) == 1 && !is.na(metho), metho, "flattest")
    contour.breaks <- if (n + 1L > 20L) pretty(zlim, 20L) else breaks
    ncontours <- length(contour.breaks)
    raster::contour(r, maxpixels=length(r), levels=contour.breaks,
                    labels=formatC(contour.breaks, big.mark=","), xlim=xlim,
                    ylim=ylim, zlim=zlim, labcex=0.5, drawlabels=drawl,
                    method=metho, axes=FALSE, col=color, lwd=lwd, add=TRUE)
  }

  graphics::axis(1, at=at2[[1]], labels=FALSE, lwd=-1, lwd.ticks=lwd, tcl=tcl,
                 cex.axis=cex)
  if (dms.tick)
    graphics::axis(2, at=at2[[2]], labels=ylabs, lwd=-1, lwd.ticks=lwd, hadj=0,
                   tcl=tcl, cex.axis=cex, las=1)
  else
    graphics::axis(2, at=at2[[2]], labels=ylabs, lwd=-1, lwd.ticks=lwd, padj=2,
                   tcl=tcl, cex.axis=cex)
  graphics::axis(3, at=at2[[3]], labels=xlabs, lwd=-1, lwd.ticks=lwd, padj=2,
                 tcl=tcl, cex.axis=cex)
  graphics::axis(4, at=at2[[4]], labels=FALSE, lwd=-1, lwd.ticks=lwd, tcl=tcl,
                 cex.axis=cex)
  graphics::box(lwd=lwd)

  if (is.character(credit))
    graphics::mtext(credit, side=1, line=-0.5, padj=1, adj=0, cex=0.6, col="#C0C0C0")

  if (!is.null(scale.loc)) {
    txt <- strsplit(proj4string(r), " ")[[1]]
    unit <- sub("^\\+units=", "", grep("^\\+units=", txt, value=TRUE))
    lonlat <- "+proj=longlat" %in% txt
    AddScaleBar(asp, unit, lonlat, scale.loc)
  }

  if (!is.null(arrow.loc))
    .AddNorthArrow(arrow.loc, r@crs, cex)

  invisible(list(din=graphics::par("din"),
                 usr=graphics::par("usr"),
                 heights=c(h2, h1) / h))
}


.LocateMidTicks <- function(side) {
  ran <- if (side %in% c(1, 3)) graphics::par("usr")[1:2] else graphics::par("usr")[3:4]
  at <- graphics::axTicks(side)
  inc <- diff(range(at)) / (length(at) - 1)
  at.mid <- seq(min(at) - inc, max(at) + inc, by=inc / 2)
  at.mid <- at.mid[!at.mid %in% at & at.mid > ran[1] & at.mid < ran[2]]
  return(at.mid)
}


.AddNorthArrow <- function(loc, crs, cex) {
  usr <- graphics::par("usr")

  x.mid <- (usr[2] + usr[1]) / 2
  y.mid <- (usr[4] + usr[3]) / 2

  crs.dd <- CRS("+proj=longlat +datum=WGS84")

  d <- 0.05 * (usr[4] - usr[3])

  xy <- rbind(c(x.mid, y.mid), c(x.mid, y.mid + d))
  sp.dd <- spTransform(SpatialPoints(xy, proj4string=crs), crs.dd)
  dd <- sp.dd@coords

  d.dd <- sqrt((dd[2, 1] - dd[1, 1])^2 + (dd[2, 2] - dd[1, 2])^2)
  dd <- rbind(dd[1, ], c(dd[1, 1],  dd[1, 2] + d.dd))
  sp.xy <- spTransform(SpatialPoints(dd, proj4string=crs.dd), crs)
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
  text(x1, y1, labels="N", pos=pos, cex=cex)
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
  sapply(x, function(i) tryCatch(is.matrix(grDevices::col2rgb(i)),
                                 error=function(e) FALSE))
}

