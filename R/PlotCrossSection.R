#' Plot Method for Cross Sections
#'
#' This function creates a cross-section view of raster data.
#' A key showing how the colors map to raster values is shown below the map.
#' The width and height of the graphics region will be automagically determined in some cases.
#'
#' @param transect 'SpatialLines'.
#'   Piecewise linear transect line.
#' @param rs 'RasterStack' or 'RasterBrick'.
#'   Collection of raster layers with the same extent and resolution.
#' @param geo.lays 'character'.
#'   Vector of names in \code{rs} that specify the geometry-raster layers;
#'   these must be given in decreasing order, that is,
#'   from the upper most (such as land surface) to the lowest (such as a bedrock surface).
#' @param val.lays 'character'.
#'   Vector of names in \code{rs} that specify the value-raster layers (optional).
#'   Values from the first layer are mapped as colors to
#'   the area between the first and second geometry layers;
#'   the second layer mapped between the second and third geometry layers, and so on.
#' @param wt.lay 'character'.
#'   The name in \code{rs} that specifies the water-table raster layer (optional).
#' @param n 'integer'.
#'   Desired number of intervals to partition the range of raster values (optional).
#' @param breaks 'numeric'.
#'   Vector of break points used to partition the colors representing numeric raster values (optional).
#' @param col 'character'.
#'   Vector of colors to be used in the plot.
#'   This argument requires \code{breaks} specification for numeric raster values
#'   and overrides any palette function specification.
#'   For numeric values there should be one less color than breaks.
#'   Categorical data require a color for each category.
#' @param ylab 'character'.
#'   Label for the \emph{y} axis.
#' @param unit 'character'.
#'   Label for the measurement unit of the \emph{x}- and \emph{y}-axes.
#' @param id 'character'.
#'   Vector of length 2 giving the labels for the end points of the transect line,
#'   defaults to \emph{A--A'}.
#' @param features 'SpatialPointsDataFrame'.
#'   Point features adjacent to the transect line that are used
#'   as reference labels for the upper geometry layer.
#'   Labels taken from first column of embedded data table.
#' @param max.feature.dist 'numeric'.
#'   Maximum distance from a point feature to the transect line,
#'   specified in the units of the \code{rs} projection.
#' @param draw.sep 'logical'.
#'   If true, lines separating geometry layers are drawn.
#' @param is.categorical 'logical'.
#'   If true, cell values in \code{val.lays} represent categorical data;
#'   otherwise, these data values are assumed continuous.
#' @param bg.col 'character'.
#'   Color used for the background of the area below the bottom geometry-raster layer.
#' @param wt.col 'character'.
#'   Color used for the water-table line.
#' @param bend.label 'character'.
#'   Vector of labels to place at top of the bend-in-section lines,
#'   values are recycled as necessary to the number of bends.
#' @inheritParams PlotMap
#'
#' @return Used for the side-effect of a new plot generated.
#'   Returns a 'list' object with the following graphical parameters:
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
#' @import rgdal
#'
#' @export
#'
#' @examples
#' m <- datasets::volcano
#' m <- m[nrow(m):1, ncol(m):1]
#' x <- seq(from = 2667405, length.out = ncol(m), by = 10)
#' y <- seq(from = 6478705, length.out = nrow(m), by = 10)
#' r1 <- raster::raster(m, xmn = min(x), xmx = max(x), ymn = min(y), ymx = max(y),
#'                      crs = "+init=epsg:27200")
#' r2 <- min(r1[]) - r1 / 10
#' r3 <- r1 - r2
#' rs <- raster::stack(r1, r2, r3)
#' names(rs) <- c("r1", "r2", "r3")
#' xy <- rbind(c(2667508, 6479501), c(2667803, 6479214), c(2667508, 6478749))
#' transect <- sp::SpatialLines(list(sp::Lines(list(sp::Line(xy)), ID = "Transect")),
#'                              proj4string = raster::crs(rs))
#' xy <- rbind(c(2667705, 6478897), c(2667430, 6479178))
#' p <- sp::SpatialPoints(xy, proj4string = raster::crs(rs))
#' d <-  data.frame("label" = c("Peak", "Random"))
#' features <- sp::SpatialPointsDataFrame(p, d, match.ID = TRUE)
#' PlotMap(r1, pal = terrain.colors, scale.loc = "top", arrow.loc = "topright",
#'         shade = list(alpha = 0.3), contour.lines = list(col = "#1F1F1FA6"))
#' graphics::lines(transect)
#' raster::text(as(transect, "SpatialPoints"), labels = c("A", "BEND", "A'"),
#'              halo = TRUE, cex = 0.7, pos = c(3, 4, 1), offset = 0.1, font = 4)
#' graphics::points(features, pch = 19)
#' raster::text(features, labels = features@data$label, halo = TRUE,
#'              cex = 0.7, pos = 4, offset = 0.5, font = 4)
#'
#' dev.new()
#' explanation <- "Vertical thickness between layers, in meters."
#' PlotCrossSection(transect, rs, geo.lays = c("r1", "r2"), val.lays = "r3",
#'                  ylab = "Elevation", asp = 5, unit = "METERS",
#'                  explanation = explanation, features = features,
#'                  max.feature.dist = 100, bend.label = "BEND IN\nSECTION",
#'                  scale.loc = "bottomright")
#'
#' val <- PlotCrossSection(transect, rs, geo.lays = c("r1", "r2"), val.lays = "r3",
#'                         ylab = "Elevation", asp = 5, unit = "METERS",
#'                         explanation = explanation, file = "Rplots.png")
#' print(val)
#'
#' file.remove("Rplots.png")
#' graphics.off()
#'

PlotCrossSection <- function(transect, rs, geo.lays=names(rs), val.lays=NULL,
                             wt.lay=NULL, asp=1, ylim=NULL,
                             max.dev.dim=c(43, 56), n=NULL, breaks=NULL,
                             pal=NULL, col=NULL, ylab=NULL, unit=NULL,
                             id=c("A", "A'"), labels=NULL, explanation=NULL,
                             features=NULL, max.feature.dist=Inf, draw.key=TRUE,
                             draw.sep=TRUE, is.categorical=FALSE,
                             contour.lines=NULL, bg.col="#E1E1E1", wt.col="#FFFFFFD8",
                             bend.label="BEND", scale.loc="bottom", file=NULL) {

  # check arguments
  checkmate::assertClass(transect, "SpatialLines")
  stopifnot(inherits(rs, c("RasterStack", "RasterBrick")))
  checkmate::assertCharacter(geo.lays, any.missing=FALSE, min.len=2)
  checkmate::assertCharacter(val.lays, any.missing=FALSE, min.len=1, null.ok=TRUE)
  checkmate::assertSubset(c(geo.lays, val.lays), names(rs))
  stopifnot(length(geo.lays) > length(val.lays))
  checkmate::assertString(wt.lay, null.ok=TRUE)
  checkmate::assertNumber(asp, lower=0)
  checkmate::assertNumeric(ylim, len=2, unique=TRUE, sorted=TRUE, null.ok=TRUE)
  checkmate::assertNumeric(max.dev.dim, len=2)
  checkmate::assertCount(n, positive=TRUE, null.ok=TRUE)
  checkmate::assertNumeric(breaks, any.missing=FALSE, min.len=2, unique=TRUE, sorted=TRUE, null.ok=TRUE)
  checkmate::assertFunction(pal, null.ok=TRUE)
  checkmate::assertCharacter(col, null.ok=TRUE)
  checkmate::assertString(ylab, null.ok=TRUE)
  checkmate::assertString(unit, null.ok=TRUE)
  checkmate::assertCharacter(id, min.chars=1, any.missing=FALSE, min.len=1, max.len=2, null.ok=TRUE)
  checkmate::assertList(labels, null.ok=TRUE)
  checkmate::assertString(explanation, null.ok=TRUE)
  checkmate::assertClass(features, "SpatialPointsDataFrame", null.ok=TRUE)
  checkmate::assertNumber(max.feature.dist, lower=0)
  checkmate::assertFlag(draw.key)
  checkmate::assertFlag(draw.sep)
  checkmate::assertFlag(is.categorical)
  checkmate::assertList(contour.lines, null.ok=TRUE)
  checkmate::assertString(bg.col, null.ok=TRUE)
  checkmate::assertString(wt.col)
  checkmate::assertCharacter(bend.label , min.len=1, null.ok=TRUE)
  checkmate::assertString(scale.loc, null.ok=TRUE)
  if (!is.null(file)) checkmate::assertPathForOutput(file, overwrite=TRUE)

  transect <- sp::spTransform(transect, raster::crs(rs))
  rs <- raster::crop(rs, raster::extent(as.vector(t(sp::bbox(transect)))), snap="out")
  layer.names <- unique(c(geo.lays, val.lays, wt.lay))
  eat <- ExtractAlongTransect(transect, raster::subset(rs, layer.names))

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
        cell.polys  <- c(cell.polys, sp::Polygon(p))
      }
    }
  }

  at <- NULL
  if (!is.null(cell.values)) {
    if (is.null(pal))
      pal <- function(n) GetTolColors(n, start=0.3, end=0.9)
    if (is.categorical) {
      unique.vals <- sort(unique(cell.values))
      at <- seq_along(unique.vals)
      if (is.null(col)) col <- pal(length(at))
      cell.cols <- col[match(cell.values, unique.vals)]
    } else {
      if (is.null(n)) n <- 200L
      if (is.null(breaks)) {
        at <- pretty(cell.values)
        breaks <- seq(min(at), max(at), length.out=n)
      }
      intervals <- findInterval(cell.values, breaks, all.inside=TRUE)
      if (is.null(col)) col <- pal(length(breaks) - 1L)
      cell.cols <- col[intervals]
    }
    cols <- unique(cell.cols)

    p <- lapply(seq_along(cols), function(i) {
      p <- sp::Polygons(cell.polys[which(cell.cols == cols[i])], i)
      p <- rgeos::gUnaryUnion(sp::SpatialPolygons(list(p), i))
      p <- methods::slot(p, "polygons")[[1]]
      p@ID <- cols[i]
      return(p)
    })
    cell.polys.merged <- sp::SpatialPolygons(p, seq_along(cols))
  }

  x <- unlist(lapply(eat, function(i) i@data$dist))
  xlim <- range(x, na.rm=TRUE)
  xat <- pretty(xlim)

  y <- unlist(lapply(eat, function(i) unlist(i@data[, geo.lays])))

  ylim <- if (is.null(ylim)) c(NA, NA) else ylim
  default.ylim <- range(pretty(range(y, na.rm=TRUE)))
  if (is.na(ylim[1])) ylim[1] <- default.ylim[1]
  if (is.na(ylim[2])) ylim[2] <- default.ylim[2]

  csi <- 0.2  # assumed line height in inches
  mai2 <- c(0.6, 4.6, 4, 2) * csi
  if (draw.key) {
    y1 <- csi
    mai1 <- c(1.2, 4.6, NA, 2) * csi
    FUN <- function(i) {if (is.null(i)) 0 else length(strsplit(i, "\n")[[1]])}
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
    repeat {
      y2 <- (w - mai2[2] - mai2[4]) * (diff(ylim * asp) / diff(xlim))
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
    on.exit(grDevices::dev.off())
  }

  if (draw.key) {
    cm.in.inches <- 2.54
    heights <- c(h2/ h, graphics::lcm(h1 * cm.in.inches))
    graphics::layout(matrix(c(2, 1), nrow=2, ncol=1), heights=heights)
    if (!is.null(labels$at)) at <- labels$at
    labs <- if (is.null(labels$labels)) TRUE else labels$labels
    AddColorKey(mai=mai1, is.categorical=is.categorical,
                breaks=breaks, col=col, at=at, labels=labs,
                explanation=explanation)
  } else {
    graphics::layout(matrix(1, nrow=1, ncol=1))
  }

  lwd <- 0.5
  cex <- 0.7
  tcl <- -0.1 / graphics::par("csi")  # length for major ticks is 0.1 inches

  # plot map
  graphics::par(mai=mai2, mgp=c(3, 0.7, 0))
  graphics::plot(NA, type="n", xlim=xlim, ylim=ylim, xaxs="i", yaxs="i", bty="n",
                 xaxt="n", yaxt="n", xlab="", ylab="", asp=asp)
  usr <- graphics::par("usr")

  if (is.null(file)) {
    xlim <- usr[1:2]
    ylim <- usr[3:4]
  }

  if (!is.null(bg.col)) {
    bg.poly <- sp::SpatialPolygons(list(sp::Polygons(lapply(eat, function(i) {
      m <- cbind(x=i@data[[1]], y=i@data[[2]])
      m <- rbind(m, cbind(rev(range(m[, "x"], na.rm=TRUE)), usr[3]), m[1, , drop=FALSE])
      return(sp::Polygon(m))
    }), "bg")), 1L)
    sp::plot(bg.poly, col=bg.col, border=NA, lwd=0.1, add=TRUE)
  }

  sp::plot(cell.polys.merged, col=cols, border=cols, lwd=0.1, add=TRUE)

  if (!is.null(wt.lay) && wt.lay[1] %in% names(rs)) {
    for (s in eat)
      graphics::lines(s@data[["dist"]], s@data[, wt.lay[1]], lwd=lwd, col=wt.col)
  }

  lays <- if (draw.sep) geo.lays else c(utils::head(geo.lays, 1), utils::tail(geo.lays, 1))
  for (s in eat)
    graphics::matplot(s@data[["dist"]], s@data[, lays], xaxt="n", yaxt="n",
                      type="l", lty=1, lwd=lwd, col="#1F1F1F", add=TRUE)

  if (!is.null(contour.lines)) {
    e <- raster::extent(cell.polys.merged)
    nc <- 200
    dx <- diff(e[1:2]) / nc
    nr <- as.integer(diff(e[3:4]) / (dx / asp))
    r <- raster::raster(e, nrows=nr, ncols=nc)
    p <- lapply(seq_along(cell.polys), function(i) {
      sp::Polygons(list(cell.polys[[i]]), as.character(i))
    })
    p <- sp::SpatialPolygons(p, seq_along(cell.polys))
    r <- raster::rasterize(p, r)
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

  yat <- pretty(ylim)
  scipen <- getOption("scipen", default=0)
  ylabs <- ToScientific(yat, scipen=scipen, type="plotmath")
  graphics::axis(4, at=yat, labels=FALSE, lwd=0, lwd.ticks=lwd, tcl=tcl)
  graphics::axis(2, at=yat, labels=ylabs, lwd=0, lwd.ticks=lwd, tcl=tcl, cex.axis=cex, las=1)

  if (!is.null(ylab)) {
    line.in.inches <- (graphics::par("mai") / graphics::par("mar"))[2]
    max.sw <- max(graphics::strwidth(ylabs, units="inches", cex=cex))
    mar.line <- max.sw / line.in.inches + sum(graphics::par("mgp")[2:3]) +
                graphics::par("mgp")[2]
    graphics::title(ylab=ylab, cex.lab=cex, line=mar.line)
  }

  graphics::abline(v=xlim, col="black", lwd=lwd)
  graphics::abline(h=usr[3], col="black", lwd=lwd)

  if (!is.null(unit)) graphics::mtext(unit, at=usr[1], cex=cex, line=0.2, adj=1)

  if (!is.null(id)) {
    if (length(id) == 1) id <- c(id, paste0(id, "'"))
    graphics::mtext(id[1], at=usr[1], cex=cex, line=1, adj=0.5, font=4)
    graphics::mtext(id[2], at=usr[2], cex=cex, line=1, adj=0.5, font=4)
  }

  graphics::par(xpd=TRUE)
  y <- unlist(lapply(eat, function(i) i@data[[geo.lays[1]]]))
  GetGeoTop <- stats::approxfun(x, y)
  pady <- graphics::strheight("M", cex=1) * 1.5
  d <- as.matrix(stats::dist(sp::coordinates(methods::as(transect, "SpatialPoints"))))[, 1]
  dist.to.bend <- d[-c(1, length(d))]
  if (!is.null(bend.label))
    bend.label <- rep(bend.label, length.out=length(dist.to.bend))
  for (i in seq_along(dist.to.bend)) {
    d <- dist.to.bend[i]
    y <- GetGeoTop(d)
    graphics::lines(c(d, d), c(usr[3], y + pady), lwd=0.3, col="#999999")
    if (is.character(bend.label[i]))
      graphics::text(d, y + pady * 1.5, bend.label[i], adj=c(0, 0.5),
                     col="#999999", cex=0.6, srt=90)
  }
  if (!is.null(features)) {
    tran.pts <- do.call("rbind", eat)
    for (i in seq_len(length(features))) {
      pnt <- sp::spTransform(features, raster::crs(rs))[i, ]
      dist.to.transect <- rgeos::gDistance(pnt, tran.pts, byid=TRUE)
      idx <- which.min(dist.to.transect)
      if (dist.to.transect[idx] > max.feature.dist) next
      d <- x[idx]
      y <- GetGeoTop(d)
      graphics::lines(c(d, d), c(y, y + pady), lwd=0.3)
      label <- format(pnt@data[1, 1])
      graphics::text(d, y + pady * 1.5, label, adj=c(0, 0.5), cex=cex, srt=90)
    }
  }
  graphics::par(xpd=FALSE)

  vert.exag <- ifelse(asp == 1, NULL, asp)
  if (!is.null(scale.loc))
    AddScaleBar(unit=unit, vert.exag=vert.exag, loc=scale.loc, inset=c(0.1, 0.05))

  invisible(list(din=graphics::par("din"), usr=usr, heights=c(h2, h1) / h))
}
