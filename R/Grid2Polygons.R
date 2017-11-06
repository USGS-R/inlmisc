#' Convert Spatial Grids to Polygons
#'
#' This function converts gridded spatial data to spatial polygons.
#' Image files created with spatial polygons are reduced in size,
#' can easily be transformed from one coordinate reference system to another,
#' and result in much "cleaner" images when plotted.
#'
#' @param grd 'SpatialGridDataFrame', 'SpatialPixelsDataFrame', or 'Raster*'.
#'    Spatial grid
#' @param zcol 'character' or 'integer'.
#'    Layer to extract from a multi-layer spatial grid.
#' @param level 'logical'.
#'    If true, a set of levels is used to partition the range of attribute values, its default is false.
#' @param at 'numeric'.
#'    Vector giving breakpoints along the range of attribute values.
#' @param cuts 'integer'.
#'    Number of levels the range of attribute values would be divided into.
#' @param pretty 'logical'.
#'    Whether to use pretty cut locations.
#' @param xlim 'numeric'.
#'    Vector of length 2 giving left and right limits of the spatial grid,
#'    data outside these limits is excluded.
#' @param ylim 'numeric'.
#'    Vector of length 2 giving lower and upper limits of the spatial grid,
#'    data outside these limits is excluded.
#' @param zlim 'numeric'.
#'    Vector of length 2 giving minimum and maximum limits of the attribute variable,
#'    data outside these limits is excluded.
#' @param ply 'SpatialPolygons', or 'SpatialGridDataFrame'.
#'    Cropping polygon
#'
#' @return Returns an object of 'SpatialPolygonsDataFrame'.
#'   The objects \code{data} slot is a data frame, number of rows equal to
#'   the number of \code{Polygons} objects and a single column containing attribute values.
#'   If \code{level} is true, attribute values are set equal to the midpoint between breakpoints.
#'   The status of the polygon as a hole or an island is taken from the ring direction,
#'   with clockwise meaning island, and counter-clockwise meaning hole.
#'
#' @note The traditional R graphics model does not draw polygon holes correctly,
#'   holes overpaint their containing 'Polygon' object using a user defined background color (white by default).
#'   Polygon holes are now rendered correctly using the \code{plot} method for
#'   spatial polygons (\code{\link{SpatialPolygons-class}}), see \code{\link{polypath}} for more details.
#'   The Trellis graphics model appears to rely on the traditional method so
#'   use caution when plotting with \code{\link[sp]{spplot}}.
#'
#' @note As an alternative, consider using the \code{\link[raster]{rasterToPolygons}} function
#'   in the \pkg{raster} package, setting \code{dissolve = TRUE}.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @references A general explanation of the algorithm provided
#'   \href{https://stackoverflow.com/questions/643995/algorithm-to-merge-adjacent-rectangles-into-polygon}{here};
#'   inspiration provided \href{https://menugget.blogspot.com/2012/04/create-polygons-from-matrix.html}{here}.
#'
#' @keywords manip
#'
#' @importMethodsFrom sp proj4string proj4string<- over bbox
#'
#' @useDynLib inlmisc, .registration=TRUE, .fixes="C_"
#'
#' @export
#'
#' @examples
#' # Example 1
#'
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
#'
#' at <- 1:ceiling(max(z, na.rm = TRUE))
#' plys <- Grid2Polygons(grd, level = TRUE, at = at)
#' cols <- rainbow(length(plys), alpha = 0.3)
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
#' cols <- rainbow(length(zz), alpha = 0.6)[zz %in% plys[[1]]]
#' sp::plot(plys, col = cols, add = TRUE)
#'
#' # Example 2
#'
#' data(meuse.grid, package = "sp")
#' sp::coordinates(meuse.grid) <- ~ x + y
#' sp::gridded(meuse.grid) <- TRUE
#' meuse.grid <- as(meuse.grid, "SpatialGridDataFrame")
#' meuse.plys <- Grid2Polygons(meuse.grid, "dist", level = FALSE)
#' op <- par(mfrow = c(1, 2), oma = rep(0, 4), mar = rep(0, 4))
#' sp::plot(meuse.plys, col = heat.colors(length(meuse.plys)))
#' title("level = FALSE", line = -7)
#'
#' meuse.plys.lev <- Grid2Polygons(meuse.grid, "dist", level = TRUE)
#' sp::plot(meuse.plys.lev, col = heat.colors(length(meuse.plys.lev)))
#' title("level = TRUE", line = -7)
#' par(op)
#'

Grid2Polygons <- function(grd, zcol=1L, level=FALSE, at=NULL, cuts=20L,
                          pretty=FALSE, xlim=NULL, ylim=NULL, zlim=NULL,
                          ply=NULL) {

  # check arguments
  if (!inherits(grd, c("BasicRaster", "SpatialPixelsDataFrame", "SpatialGridDataFrame")))
    stop("Assertion on 'grd' failed: Wrong class.")
  checkmate::qassert(zcol, c("S1[0,)", "X1[0,)"))
  checkmate::assertFlag(level)
  checkmate::assertNumeric(at, any.missing=FALSE, min.len=2, null.ok=TRUE)
  checkmate::assertCount(cuts, positive=TRUE)
  checkmate::assertFlag(pretty)
  checkmate::assertNumeric(xlim, len=2, sorted=TRUE, null.ok=TRUE)
  checkmate::assertNumeric(ylim, len=2, sorted=TRUE, null.ok=TRUE)
  checkmate::assertNumeric(zlim, len=2, sorted=TRUE, null.ok=TRUE)
  checkmate::assertClass(ply, "SpatialPolygons", null.ok=TRUE)

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
    zc <- at[1:(length(at) - 1L)] + diff(at) / 2
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
  nodes <- 1:nnodes
  elems <- 1:nelems
  coords <- cbind(x=rep(x, m + 1L), y=rep(rev(y), each=n + 1L))
  n1 <- unlist(lapply(1:m, function(i) seq(1L, n) + (i - 1L) * (n + 1L)))
  n2 <- n1 + 1L
  n4 <- unlist(lapply(1:m, function(i) seq(1L, n) + i * (n + 1L)))
  n3 <- n4 + 1L
  elem.nodes <- cbind(n1, n2, n3, n4)

  # define segments in each element
  nsegs <- nelems * 4L
  segs <- matrix(data=NA, nrow=nsegs, ncol=4,
                 dimnames=list(1:nsegs, c("elem", "a", "b", "z")))
  segs[, 1] <- rep(1:nelems, each=4)
  segs[, 2] <- c(t(elem.nodes))
  segs[, 3] <- c(t(elem.nodes[, c(2, 3, 4, 1)]))
  segs[, 4] <- rep(z, each=4)
  segs <- stats::na.omit(segs)

  # identify levels (or unique values)
  levs <- sort(unique(stats::na.omit(z)))

  # find polygon nodes for each level
  FUN <- function(i) .FindPolyNodes(segs[segs[, "z"] == i, c("a", "b")])
  poly.nodes <- lapply(levs, FUN)

  # build lists of 'Polygon' objects
  FUN <- function(i) lapply(i, function(j) sp::Polygon(coords[j, ]))
  poly <- lapply(poly.nodes, FUN)

  # build list of 'Polygons' objects
  ids <- make.names(1:length(poly), unique=TRUE)
  FUN <- function(i) sp::Polygons(poly[[i]], ID=ids[i])
  polys <- lapply(1:length(poly), FUN)

  # convert to 'SpatialPolygons' object, add datum and projection
  sp.polys <- sp::SpatialPolygons(polys, proj4string=raster::crs(grd))

  # convert to 'SpatialPolygonsDataFrame' object, add data frame of levels
  d <- data.frame(z=levs, row.names=row.names(sp.polys))
  sp.polys.df <- sp::SpatialPolygonsDataFrame(sp.polys, data=d, match.ID=TRUE)

  # crop 'SpatialPolygonsDataFrame' object using polygon argument
  if (!is.null(ply)) sp.polys.df <- raster::crop(sp.polys.df, ply)

  return(sp.polys.df)
}


# Find polygon nodes
#
#  input:  s          - 'matrix'.
#                       A 2-column table giving start- and end-node
#                       indexes for each segment in a level.
#  output: poly.nodes - 'list'.
#                       Vector components giving node indexes for each
#                       polygon ring. The status of the polygon as a hole or
#                       an island is taken from the ring direction, with
#                       clockwise meaning island, and counter-clockwise
#                       meaning hole.

.FindPolyNodes <- function(s) {

  # remove duplicate segments
  id <- paste(apply(s, 1, min), apply(s, 1, max))
  s <- s[!id %in% unique(id[duplicated(id)]), ]

  # call c program to define polygon rings
  out <- matrix(.Call(C_DefinePolygons, as.integer(s[, 1]), as.integer(s[, 2])),
                nrow=nrow(s), ncol=2)

  # place returned array into list object
  poly.nodes <- lapply(unique(out[, 2]), function(i) out[out[, 2] == i, 1])

  # close polygon by joining the first point to the last point
  poly.nodes <- lapply(poly.nodes, function(i) c(i, i[1]))

  return(poly.nodes)
}
