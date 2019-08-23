#' Convert Spatial Grids to Polygons
#'
#' Convert gridded spatial data to spatial polygons.
#' Image files created with spatial polygons are reduced in size,
#' can easily be transformed from one coordinate reference system to another,
#' and result in much "cleaner" images when plotted.
#'
#' @param grd 'SpatialGridDataFrame', 'SpatialPixelsDataFrame', or 'Raster*'.
#'    Spatial grid
#' @param zcol 'character' string or 'integer' count.
#'    Layer to extract from a multi-layer spatial grid.
#' @param level 'logical' flag.
#'    If true, a set of levels is used to partition the range of attribute values,
#'    its default is false.
#' @param at 'numeric' vector.
#'    Breakpoints along the range of attribute values.
#' @param cuts 'integer' count.
#'    Number of levels the range of attribute values would be divided into.
#' @param pretty 'logical' flag.
#'    Whether to use pretty cut locations.
#' @param xlim 'numeric' vector of length 2.
#'    Left and right limits of the spatial grid, data outside these limits is excluded.
#' @param ylim 'numeric' vector of length 2.
#'    Lower and upper limits of the spatial grid,
#'    data outside these limits is excluded.
#' @param zlim 'numeric' vector of length 2.
#'    Minimum and maximum limits of the attribute variable,
#'    data outside these limits is excluded.
#' @param ply 'SpatialPolygons', or 'SpatialGridDataFrame'.
#'    Cropping polygon
#' @param check_validity 'logical' flag.
#'    If true (default), check the validity of polygons.
#'    If any of the polygons are invalid, try making them valid by zero-width buffering.
#'
#' @return An object of class 'SpatialPolygonsDataFrame'.
#'   The objects \code{data} slot is a data frame, number of rows equal to
#'   the number of \code{Polygons} objects and a single column containing attribute values.
#'   If \code{level} is true, attribute values are set equal to the midpoint between breakpoints.
#'   The status of the polygon as a hole or an island is taken from the ring direction,
#'   with clockwise meaning island, and counter-clockwise meaning hole.
#'
#' @note The traditional R graphics model does not draw polygon holes correctly,
#'   holes overpaint their containing 'Polygon' object using
#'   a user defined background color (white by default).
#'   Polygon holes are now rendered correctly using the \code{plot} method for
#'   spatial polygons (\code{\link{SpatialPolygons-class}}),
#'   see \code{\link{polypath}} for more details.
#'   The Trellis graphics model appears to rely on the traditional method so
#'   use caution when plotting with \code{\link[sp]{spplot}}.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso As an alternative, consider using the
#'   \code{\link[raster]{rasterToPolygons}} function
#'   in the \pkg{raster} package, setting \code{dissolve = TRUE}.
#'
#' @keywords manip
#'
#' @import rgdal
#'
#' @export
#'
#' @examples
#' # Example 1
#' z <- c(1.1,  1.5,  4.2,  4.1,  4.3,  4.7,
#'        1.2,  1.4,  4.8,  4.8,   NA,  4.1,
#'        1.7,  4.2,  1.4,  4.8,  4.0,  4.4,
#'        1.1,  1.3,  1.2,  4.8,  1.6,   NA,
#'        3.3,  2.9,   NA,  4.1,  1.0,  4.0)
#' m <- 5
#' n <- 6
#' x <- rep(0:n, m + 1)
#' y <- rep(0:m, each = n + 1)
#' xc <- c(rep(seq(0.5, n - 0.5, by = 1), m))
#' yc <- rep(rev(seq(0.5, m - 0.5, by = 1)), each = n)
#' grd <- data.frame(z = z, xc = xc, yc = yc)
#' sp::coordinates(grd) <- ~ xc + yc
#' sp::gridded(grd) <- TRUE
#' grd <- as(grd, "SpatialGridDataFrame")
#' image(grd, col = gray.colors(30), axes = TRUE)
#' grid(col = "black", lty = 1)
#' points(x = x, y = y, pch = 16)
#' text(cbind(xc, yc), labels = z)
#' text(cbind(x = x + 0.1, y = rev(y + 0.1)),
#'      labels = 1:((m + 1) * (n + 1)), cex = 0.6)
#' at <- 1:ceiling(max(z, na.rm = TRUE))
#' plys <- Grid2Polygons(grd, level = TRUE, at = at)
#' cols <- GetColors(length(plys), scheme = "bright", alpha = 0.3)
#' sp::plot(plys, add = TRUE, col = cols)
#' zz <- plys[[1]]
#' legend("top", legend = zz, fill = cols, bty = "n", xpd = TRUE,
#'        inset = c(0, -0.1), ncol = length(plys))
#'
#' v1 <- rbind(c( 1.2, 0.5), c(5.8, 1.7), c( 2.5, 5.1), c( 1.2, 0.5))
#' v2 <- rbind(c( 2.5, 2.5), c(3.4, 1.8), c( 3.7, 3.1), c( 2.5, 2.5))
#' v3 <- rbind(c(-0.3, 3.3), c(1.7, 5.1), c(-1.0, 7.0), c(-0.3, 3.3))
#' p1 <- sp::Polygon(v1, hole = FALSE)
#' p2 <- sp::Polygon(v2, hole = TRUE)
#' p3 <- sp::Polygon(v3, hole = FALSE)
#' p <- sp::SpatialPolygons(list(sp::Polygons(list(p1, p2, p3), 1)))
#' plys <- Grid2Polygons(grd, level = TRUE, at = at, ply = p)
#' cols <- GetColors(length(zz), scheme = "bright", alpha = 0.6)
#' cols <- cols[zz %in% plys[[1]]]
#' sp::plot(plys, col = cols, add = TRUE)
#' text(cbind(xc, yc), labels = z)
#'
#' # Example 2
#' data(meuse.grid, package = "sp")
#' sp::coordinates(meuse.grid) <- ~ x + y
#' sp::gridded(meuse.grid) <- TRUE
#' meuse.grid <- as(meuse.grid, "SpatialGridDataFrame")
#' meuse.plys <- Grid2Polygons(meuse.grid, "dist", level = FALSE)
#' op <- par(mfrow = c(1, 2), oma = rep(0, 4), mar = rep(0, 4))
#' sp::plot(meuse.plys, col = heat.colors(length(meuse.plys)))
#' title("level = FALSE", line = -7)
#' meuse.plys.lev <- Grid2Polygons(meuse.grid, "dist", level = TRUE)
#' sp::plot(meuse.plys.lev, col = heat.colors(length(meuse.plys.lev)))
#' title("level = TRUE", line = -7)
#' par(op)
#'
#' # Example 3
#' m <- datasets::volcano
#' m <- m[nrow(m):1, ncol(m):1]
#' x <- seq(from = 2667405, length.out = ncol(m), by = 10)
#' y <- seq(from = 6478705, length.out = nrow(m), by = 10)
#' r <- raster::raster(m, xmn = min(x), xmx = max(x), ymn = min(y), ymx = max(y),
#'                     crs = "+init=epsg:27200")
#' plys <- Grid2Polygons(r, level = TRUE)
#' cols <- GetColors(length(plys), scheme = "DEM screen")
#' sp::plot(plys, col = cols, border = "#515151")
#'

Grid2Polygons <- function(grd, zcol=1, level=FALSE, at=NULL, cuts=20,
                          pretty=FALSE, xlim=NULL, ylim=NULL, zlim=NULL,
                          ply=NULL, check_validity=TRUE) {

  # check arguments
  stopifnot(inherits(grd, c("BasicRaster",
                            "SpatialPixelsDataFrame",
                            "SpatialGridDataFrame")))
  checkmate::qassert(zcol, c("S1", "X1[0,)"))
  checkmate::assertFlag(level)
  checkmate::assertNumeric(at, any.missing=FALSE, min.len=2, null.ok=TRUE)
  checkmate::assertCount(cuts, positive=TRUE)
  checkmate::assertFlag(pretty)
  checkmate::assertNumeric(xlim, len=2, sorted=TRUE, null.ok=TRUE)
  checkmate::assertNumeric(ylim, len=2, sorted=TRUE, null.ok=TRUE)
  checkmate::assertNumeric(zlim, len=2, sorted=TRUE, null.ok=TRUE)
  checkmate::assertClass(ply, "SpatialPolygons", null.ok=TRUE)
  checkmate::assertFlag(check_validity)

  # convert grid to 'RasterLayer' class
  if (!inherits(grd, "RasterLayer"))
    grd <- raster::raster(grd, layer=zcol)

  # crop grid data using polygon argument
  if (!is.null(ply)) {
    crs <- raster::crs(grd)
    if (is.na(sp::proj4string(ply))) sp::proj4string(ply) <- crs
    if (!sp::identicalCRS(grd, ply)) ply <- sp::spTransform(ply, crs)
    grd <- raster::crop(grd, ply, snap="out")
  }

  # crop grid using xlim and ylim
  if (is.null(xlim)) xlim <- c(NA, NA)
  if (is.null(ylim)) ylim <- c(NA, NA)
  if (is.na(xlim[1])) xlim[1] <- raster::xmin(grd)
  if (is.na(xlim[2])) xlim[2] <- raster::xmax(grd)
  if (is.na(ylim[1])) ylim[1] <- raster::ymin(grd)
  if (is.na(ylim[2])) ylim[2] <- raster::ymax(grd)
  e <- raster::extent(c(xlim, ylim))
  grd <- raster::crop(grd, e, snap="in")

  # crop grid using zlim
  if (is.null(zlim)) zlim <- c(NA, NA)
  if (is.na(zlim[1])) zlim[1] <- min(grd[], na.rm=TRUE)
  if (is.na(zlim[2])) zlim[2] <- max(grd[], na.rm=TRUE)
  grd[grd[] < zlim[1] | grd[] > zlim[2]] <- NA
  grd <- raster::trim(grd)

  # determine break points
  if (level) {
    if (is.null(at)) {
      if (pretty)
        at <- pretty(zlim, cuts)
      else
        at <- seq(zlim[1], zlim[2], length.out=cuts)
    }
    zc <- at[seq_len(length(at) - 1L)] + diff(at) / 2
    z <- zc[findInterval(grd[], at, rightmost.closed=TRUE)]
  } else {
    z <- as.numeric(grd[])
  }

  # define nodes and elements
  m <- dim(grd)[1]
  n <- dim(grd)[2]
  dx <- raster::xres(grd)
  dy <- raster::yres(grd)
  xmin <- raster::xmin(grd)
  xmax <- raster::xmax(grd)
  ymin <- raster::ymin(grd)
  ymax <- raster::ymax(grd)
  x <- seq(xmin, xmax, by=dx)
  y <- seq(ymin, ymax, by=dy)
  nnodes <- (m + 1L) * (n + 1L)
  nelems <- m * n
  nodes <- seq_len(nnodes)
  elems <- seq_len(nelems)
  coords <- cbind(x=rep(x, m + 1L), y=rep(rev(y), each=n + 1L))
  n1 <- unlist(lapply(seq_len(m), function(i) seq(1, n) + (i - 1L) * (n + 1L)))
  n2 <- n1 + 1L
  n4 <- unlist(lapply(seq_len(m), function(i) seq(1, n) + i * (n + 1L)))
  n3 <- n4 + 1L
  elem_nodes <- cbind(n1, n2, n3, n4)

  # define segments in each element
  nsegs <- nelems * 4L
  segs <- matrix(data=NA, nrow=nsegs, ncol=4,
                 dimnames=list(1:nsegs, c("elem", "a", "b", "z")))
  segs[, 1] <- rep(seq_len(nelems), each=4)
  segs[, 2] <- c(t(elem_nodes))
  segs[, 3] <- c(t(elem_nodes[, c(2, 3, 4, 1)]))
  segs[, 4] <- rep(z, each=4)
  segs <- stats::na.omit(segs)

  # identify levels (or unique values)
  levs <- sort(unique(stats::na.omit(z)))

  # find polygon nodes for each level
  poly_nodes <- lapply(levs, function(x) {
    FindPolyNodes(segs[segs[, "z"] == x, c("a", "b")])
  })

  # build 'SpatialPolygons' object
  sp_polys <- sp::SpatialPolygons(lapply(seq_along(poly_nodes), function(i) {
    sp::Polygons(lapply(poly_nodes[[i]], function(x) {
      sp::Polygon(coords[x, ])
    }), ID=paste0("X", i))
  }), proj4string=raster::crs(grd))

  # assign ownership of holes to parent polygons
  sp_polys <- rgeos::createSPComment(sp_polys)

  # check validity of polygons, if invalid, try making valid by zero-width buffering
  if (check_validity) {
    if (isFALSE(all(suppressWarnings(rgeos::gIsValid(sp_polys, byid=TRUE)))))
      sp_polys <- rgeos::gBuffer(sp_polys, byid=TRUE, width=0)
    if (isFALSE(all(rgeos::gIsValid(sp_polys, byid=TRUE))))
      stop("invalid polygons found")
  }

  # convert to 'SpatialPolygonsDataFrame' object, add data frame of levels
  d <- data.frame(z=levs, row.names=row.names(sp_polys))
  sp_polys <- sp::SpatialPolygonsDataFrame(sp_polys, data=d, match.ID=TRUE)

  # crop 'SpatialPolygonsDataFrame' object using polygon argument
  if (!is.null(ply)) sp_polys <- SetPolygons(sp_polys, ply, "gIntersection")

  sp_polys
}


#' Find Polygon Nodes
#'
#' @param s 'matrix'.
#'    A 2-column table giving start- and end-node indexes for each segment in a level.
#'
#' @return An object of class 'list'.
#'   Vector components giving node indexes for each polygon ring.
#'   The status of the polygon as a hole or an island is taken from the ring direction,
#'   with clockwise meaning island, and counter-clockwise meaning hole.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @references \url{https://stackoverflow.com/questions/643995}
#'
#' @keywords internal
#'
#' @useDynLib inlmisc, .registration=TRUE, .fixes="C_"
#'
#' @export
#'

FindPolyNodes <- function(s) {
  mode(s) <- "integer"

  # remove duplicate segments
  id <- paste(pmin.int(s[, 1], s[, 2]), pmax.int(s[, 1], s[, 2]))
  s <- s[!id %in% id[duplicated(id)], , drop=FALSE]

  # call c program to define polygon rings
  out <- matrix(.Call(C_DefinePolygons, s[, 1], s[, 2]), nrow=nrow(s), ncol=2)

  # place returned array into list object
  poly_nodes <- lapply(unique(out[, 2]), function(x) out[out[, 2] == x, 1])

  # close polygon by joining the first point to the last point
  poly_nodes <- lapply(poly_nodes, function(x) c(x, x[1]))

  poly_nodes
}
