#' Convert Spatial Grids to Polygons
#'
#' Converts \pkg{sp} spatial objects from class '\code{\link{SpatialGridDataFrame}}' to '\code{\link{SpatialPolygonsDataFrame}}'.
#' Spatial polygons can then be transformed to a different projection or datum with \code{spTransform} in package \pkg{rgdal}.
#' Image files created with spatial polygons are reduced in size and result in a much "cleaner" version of your image.
#'
#' @param grd 'SpatialGridDataFrame'.
#'    Spatial grid data frame
#' @param zcol 'character' or 'integer'.
#'    Attribute name or column number in attribute table.
#' @param level 'logical'.
#'    If true, a set of levels is used to partition the range of \code{z}, its default is false.
#' @param at 'numeric'.
#'    A vector giving breakpoints along the range of \code{z}.
#' @param cuts 'integer'.
#'    Number of levels the range of \code{z} would be divided into.
#' @param pretty 'logical'.
#'    Whether to use pretty cut locations.
#' @param xlim 'numeric'.
#'    Vector of length 2 giving left and right limits of the spatial grid, data outside these limits is excluded.
#' @param ylim 'numeric'.
#'    Vector of length 2 giving lower and upper limits of the spatial grid, data outside these limits is excluded.
#' @param ply 'SpatialPolygons', or 'SpatialGridDataFrame'.
#'    Cropping polygon
#'
#' @return Returns an object of 'SpatialPolygonsDataFrame'.
#'   The objects \code{data} slot is a data frame, number of rows equal to
#'   the number of \code{Polygons} objects and a single column containing values of \code{z}.
#'   If \code{level} is true, \code{z} values are set equal to the midpoint between breakpoints.
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
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{SpatialPolygons}}
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
#' text(cbind(x = x + 0.1, y = rev(y + 0.1)), labels = 1:((m + 1) * (n + 1)), cex = 0.6)
#'
#' at <- 1:ceiling(max(z, na.rm = TRUE))
#' plys <- Grid2Polygons(grd, level = TRUE, at = at)
#' cols <- rainbow(length(plys), alpha = 0.3)
#' sp::plot(plys, add = TRUE, col = cols)
#' zz <- plys[[1]]
#' legend("top", legend = zz, fill = cols, bty = "n", xpd = TRUE,
#'        inset = c(0, -0.1), ncol = length(plys))
#'
#' p1 <- sp::Polygon(rbind(c(1.2, 0.5), c(5.8, 1.7), c(2.5, 5.1), c(1.2, 0.5)),
#'                   hole = FALSE)
#' p2 <- sp::Polygon(rbind(c(2.5, 2.5), c(3.4, 1.8), c(3.7, 3.1), c(2.5, 2.5)),
#'                   hole = TRUE)
#' p3 <- sp::Polygon(rbind(c(-0.3, 3.3), c(1.7, 5.1), c(-1.0, 7.0), c(-0.3, 3.3)),
#'                   hole = FALSE)
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

Grid2Polygons <- function(grd, zcol=1, level=FALSE, at, cuts=20,
                          pretty=FALSE, xlim=NULL, ylim=NULL, ply=NULL) {

  # check arguments
  if (!inherits(grd, "SpatialGridDataFrame"))
    stop("Grid object not of class SpatialGridDataFrame")
  if (is.character(zcol) && !(zcol %in% names(grd)))
    stop("Column name not in attribute table")
  if (is.numeric(zcol) && zcol > ncol(methods::slot(grd, "data")))
    stop("Column number outside bounds of attribute table")
  if (!is.null(ply) && !inherits(ply, c("SpatialPolygons", "SpatialPolygonsDataFrame")))
    stop("incorrect polygon class")

  # crop grid data using limit arguments
  if (!is.null(xlim) | !is.null(ylim)) {
    if (is.null(xlim))
      xlim <- sp::bbox(grd)[1, ]
    if (is.null(ylim))
      ylim <- sp::bbox(grd)[2, ]
    vertices <- matrix(c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1],
                         ylim[1], ylim[1], ylim[2], ylim[2], ylim[1]),
                         nrow=5, ncol=2)
    ply.box <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(vertices, hole=FALSE)), 1)))
    sp::proj4string(ply.box) <- sp::CRS(sp::proj4string(grd))
    grd[[zcol]][is.na(sp::over(grd, ply.box))] <- NA
  }

  # determine break points
  if (level) {
    if (missing(at)) {
      zlim <- range(grd[[zcol]], finite=TRUE)
      if (pretty)
        at <- pretty(zlim, cuts)
      else
        at <- seq(zlim[1], zlim[2], length.out=cuts)
    }
    zc <- at[1:(length(at) - 1L)] + diff(at) / 2
    z <- zc[findInterval(grd[[zcol]], at, rightmost.closed=TRUE)]
  } else {
    z <- grd[[zcol]]
  }

  # define nodes and elements
  grd.par <- sp::gridparameters(grd)
  n <- grd.par$cells.dim[1]
  m <- grd.par$cells.dim[2]
  dx <- grd.par$cellsize[1]
  dy <- grd.par$cellsize[2]
  xmin <- grd.par$cellcentre.offset[1] - dx / 2
  ymin <- grd.par$cellcentre.offset[2] - dy / 2
  xmax <- xmin + n * dx
  ymax <- ymin + m * dy
  x <- seq(xmin, xmax, by=dx)
  y <- seq(ymin, ymax, by=dy)
  nnodes <- (m + 1L) * (n + 1L)
  nelems <- m * n
  nodes <- 1L:nnodes
  elems <- 1L:nelems
  coords <- cbind(x=rep(x, m + 1L), y=rep(rev(y), each=n + 1L))
  n1 <- c(sapply(1L:m, function(i) seq(1L, n) + (i - 1L) * (n + 1L)))
  n2 <- n1 + 1L
  n4 <- c(sapply(1L:m, function(i) seq(1L, n) + i * (n + 1L)))
  n3 <- n4 + 1L
  elem.nodes <- cbind(n1, n2, n3, n4)

  # define segments in each element
  nsegs <- nelems * 4L
  segs <- matrix(data=NA, nrow=nsegs, ncol=4,
                 dimnames=list(1L:nsegs, c("elem", "a", "b", "z")))
  segs[, 1] <- rep(1:nelems, each=4)
  segs[, 2] <- c(t(elem.nodes))
  segs[, 3] <- c(t(elem.nodes[, c(2, 3, 4, 1)]))
  segs[, 4] <- rep(z, each=4)
  segs <- stats::na.omit(segs)

  # identify levels (or unique values)
  levs <- sort(unique(stats::na.omit(z)))

  # find polygon nodes for each level
  fun <- function(i) .FindPolyNodes(segs[segs[, "z"] == i, c("a", "b")])
  poly.nodes <- lapply(levs, fun)

  # build lists of Polygon objects
  fun <- function(i) lapply(i, function(j) sp::Polygon(coords[j, ]))
  poly <- lapply(poly.nodes, fun)

  # build list of Polygons objects
  ids <- make.names(1L:length(poly), unique=TRUE)
  fun <- function(i) sp::Polygons(poly[[i]], ID=ids[i])
  polys <- lapply(1L:length(poly), fun)

  # convert to SpatialPolygons object, add datum and projection
  sp.polys <- sp::SpatialPolygons(polys, proj4string=grd@proj4string)

  # convert to SpatialPolygonsDataFrame object, add data frame of levels
  d <- data.frame(z=levs, row.names=row.names(sp.polys))
  sp.polys.df <- sp::SpatialPolygonsDataFrame(sp.polys, data=d, match.ID=TRUE)

  # crop SpatialPolygonsDataFrame object using polygon argument
  if (!is.null(ply)) sp.polys.df <- raster::crop(sp.polys.df, ply)

  return(sp.polys.df)
}


# Find polygon nodes
#
#  input:  s          - matrix; 2-column table giving start- and end-node
#                       indexes for each segment in a level
#  output: poly.nodes - list; vector components giving node indexes for each
#                       polygon ring. The status of the polygon as a hole or
#                       an island is taken from the ring direction, with
#                       clockwise meaning island, and counter-clockwise
#                       meaning hole.

.FindPolyNodes <- function(s) {

  # remove duplicate segments
  id <- paste(apply(s, 1, min), apply(s, 1, max), sep="")
  duplicates <- unique(id[duplicated(id)])
  s <- s[!id %in% duplicates, ]

  # number of segments in level
  m <- nrow(s)

  # call c program to define polygon rings
  out <- matrix(.Call(C_DefinePolygons, as.integer(s[, 1]), as.integer(s[, 2])),
                nrow=m, ncol=2)

  # place returned array into list object
  poly.nodes <- lapply(unique(out[, 2]), function(i) out[out[, 2] == i, 1])

  # close polygon by joining the first point to the last point
  poly.nodes <- lapply(poly.nodes, function(i) c(i, i[1]))

  return(poly.nodes)
}
