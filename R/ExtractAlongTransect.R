#' Extract Raster Values Along Transect Line
#'
#' This function extracts values from raster layer(s) along a user defined transect line.
#'
#' @param transect 'SpatialPoints' or 'SpatialLines'.
#'   Transect line or its vertices.
#' @param r 'RasterLayer', 'RasterStack', or 'RasterBrick'.
#'   Raster layer(s)
#'
#' @details The transect line is described using a simple polygonal chain.
#'   The transect line and raster layer(s) must be specified in a coordinate reference system.
#'
#' @return A 'list' is returned with components of class 'SpatialPointsDataFrame'.
#'   These components represent continuous piecewise line segments along the transect.
#'   The following variables are specified for each coordinate point in the line segment:
#'   \describe{
#'     \item{dist}{distance along the transect line.}
#'     \item{2, \dots, n}{extracted value for each raster layer in \code{r}, where column names match their respective raster layer name.}
#'   }
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{PlotCrossSection}}
#'
#' @keywords utilities
#'
#' @import rgdal
#'
#' @export
#'
#' @examples
#' coords <- rbind(c(-100, -90), c(80, 90), c(80, 0), c(40, -40))
#' crs <- sp::CRS("+init=epsg:4326")
#' transect <- sp::SpatialPoints(coords, proj4string = crs)
#' r <- raster::raster(nrows = 10, ncols = 10, ymn = -80, ymx = 80, crs = crs)
#' names(r) <- "value"
#' set.seed(0)
#' r[] <- runif(raster::ncell(r))
#' r[4, 6] <- NA
#' PlotMap(r)
#' l <- sp::Lines(list(sp::Line(coords)), ID = "Transect")
#' lines(sp::SpatialLines(list(l), proj4string = crs))
#' points(transect, pch = 19)
#' segs <- ExtractAlongTransect(transect, r)
#' for (i in seq_along(segs)) points(segs[[i]])
#'
#' dev.new()
#' xlab <- "Distance along transect"
#' ylab <- "Raster value"
#' xlim <- range(vapply(segs, function(i) range(i@data[, "dist"]), c(0, 0)))
#' ylim <- range(vapply(segs, function(i) range(i@data[, "value"], na.rm = TRUE), c(0, 0)))
#' plot(NA, type = "n", xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim)
#' for (i in seq_along(segs)) lines(segs[[i]]@data[, c("dist", "value")],
#'                                  col = GetTolColors(length(segs))[i])
#' coords <- sp::coordinates(transect)
#' n <- length(transect)
#' d <- cumsum(c(0, as.matrix(dist((coords)))[cbind(1:(n - 1), 2:n)]))
#' abline(v = d, col = "grey", lty = 2)
#' mtext(paste0("(", paste(head(coords, 1), collapse = ", "), ")"), adj = 0)
#' mtext(paste0("(", paste(tail(coords, 1), collapse = ", "), ")"), adj = 1)
#'
#' graphics.off()
#'

ExtractAlongTransect <- function(transect, r) {

  if (!inherits(transect, c("SpatialPoints", "SpatialLines")))
    stop("incorrect class for 'transect' argument")
  if (!inherits(r, c("RasterLayer", "RasterStack", "RasterBrick")))
    stop("incorrect class for 'r' argument")

  if (inherits(transect, "SpatialLines"))
    transect <- methods::as(transect, "SpatialPoints")

  crs <- raster::crs(r)

  v <- sp::coordinates(sp::spTransform(transect, crs))
  if (length(v) < 2) stop("number of vertices in transect is < 2")

  r.xmin <- raster::xmin(r)
  r.xmax <- raster::xmax(r)
  r.ymin <- raster::ymin(r)
  r.ymax <- raster::ymax(r)

  rx <- seq(r.xmin, r.xmax, by=raster::xres(r))
  ry <- seq(r.ymin, r.ymax, by=raster::yres(r))

  dist.along.transect <- as.matrix(stats::dist(v))
  segs <- list()
  v.d <- 0

  for (i in seq_len(nrow(v) - 1L)) {

    v.x <- v[i:(i + 1L), 1]
    v.y <- v[i:(i + 1L), 2]

    m <- (v.y[2] - v.y[1]) / (v.x[2] - v.x[1])
    rx.x <- rx[rx > min(v.x) & rx < max(v.x)]
    ry.y <- ry[ry > min(v.y) & ry < max(v.y)]
    rx.y <- m * (rx.x - v.x[1]) + v.y[1]
    ry.x <- (ry.y - v.y[1]) / m + v.x[1]

    x <- c(v.x[1], rx.x, ry.x, v.x[2])
    y <- c(v.y[1], rx.y, ry.y, v.y[2])

    d <- as.matrix(stats::dist(cbind(x, y), diag=TRUE))[, 1]
    idxs <- order(d)
    x <- x[idxs]
    y <- y[idxs]
    d <- d[idxs]
    idxs <- which(x >= r.xmin & x <= r.xmax & y >= r.ymin & y <= r.ymax)
    x <- x[idxs]
    y <- y[idxs]
    d <- d[idxs] + sum(v.d)
    n <- length(d)
    mid.x <- vapply(seq_len(n - 1L), function(j) mean(c(x[j], x[j + 1L])), 0)
    mid.y <- vapply(seq_len(n - 1L), function(j) mean(c(y[j], y[j + 1L])), 0)

    x <- c(x[1], rep(x[2:(n - 1L)], each=2), x[n])
    y <- c(y[1], rep(y[2:(n - 1L)], each=2), y[n])
    d <- c(d[1], rep(d[2:(n - 1L)], each=2), d[n])

    n <- length(x)
    seg <- cbind(x, y, dist=d, matrix(NA, nrow=n, ncol=raster::nlayers(r),
                 dimnames=list(NULL, names(r))))
    colnames(seg) <- make.names(colnames(seg), unique=TRUE)

    for (j in seq_len(raster::nlayers(r))) {
      z <- raster::extract(r[[j]], sp::SpatialPoints(cbind(mid.x, mid.y)))
      seg[, j + 3L] <- rep(z, each=2)
    }
    rownames(seg) <- NULL

    idxs <- NULL
    for (j in seq_len(n)) {
      is.last <- j == n
      if (is.last || all(is.na(seg[j, ]))) {
        if (is.null(idxs)) next
        if (is.last) idxs <- c(idxs, j)
        nsegs <- length(segs)
        if (nsegs == 0 || max(segs[[nsegs]][, "dist"]) != min(d[idxs]))
          segs[[nsegs + 1L]] <- seg[idxs, ]
        else
          segs[[nsegs]] <- rbind(segs[[nsegs]], seg[idxs, ])
        idxs <- NULL
      } else {
        idxs <- c(idxs, j)
      }
    }
    v.d <- c(v.d, dist.along.transect[i, i + 1L])
  }

  return(lapply(segs, function(s) {
    sp::SpatialPointsDataFrame(s[, 1:2], data.frame(s[, -(1:2)], row.names=NULL),
                               proj4string=crs, match.ID=FALSE)
  }))
}
