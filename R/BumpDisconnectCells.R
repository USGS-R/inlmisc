#' Adjustment for Vertically Disconnected Cells
#'
#' This function decreases model cell values (such as land-surface elevations)
#' in the lower raster layer if they violate a minimum vertical overlap between adjacent cells.
#'
#' @param rs 'RasterStack'.
#'   A collection of two raster layers, the first and second layers represent the top and bottom of a model layer.
#' @param min.overlap 'numeric'.
#'   Minimum vertical overlap between adjacent cells.
#' @param bump.by 'numeric'.
#'   Amount to decrease a cell value by during each iteration of the algorithm.
#' @param max.itr 'numeric'.
#'   Maximum number of iterations.
#'
#' @details During each iteration of the algorithm:
#'   (1) Cells are identified that violate the minimum vertical overlap between adjacent cells; that is,
#'       the bottom of cell i is greater than or equal to the top of an adjacent cell j minus the
#'       minimum overlap specified by the \code{min.overlap} argument.
#'   (2) For cells violating the minimum vertical overlap, lower raster layer (\code{rs[[2]]}) values are
#'       decreased by the value specified in the \code{bump.by} argument.
#'
#' @return Returns a 'RasterLayer' that can be added to \code{rs[[2]]} to ensure connectivity between cells.
#'   Cell values in the returned raster grid represent vertical adjustments.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' set.seed(0)
#' r.top <- raster::raster(ncols = 10, nrows = 10)
#' r.bot <- raster::raster(ncols = 10, nrows = 10)
#' r.top[] <- rnorm(raster::ncell(r.top), mean = 12)
#' r.bot[] <- rnorm(raster::ncell(r.bot), mean = 10)
#' summary(r.top - r.bot)
#'
#' r <- BumpDisconnectCells(raster::stack(r.top, r.bot), min.overlap = 0.1)
#' raster::plot(r.bot + r)
#'

BumpDisconnectCells <- function(rs, min.overlap=2, bump.by=0.1, max.itr=1e+04) {
  r <- rs[[2]]

  cell <- which(!is.na(r[]))
  rows <- raster::rowFromCell(r, cell)
  cols <- raster::colFromCell(r, cell)

  d <- cbind(cell, c1=NA, c2=NA, c3=NA, c4=NA)
  d[, "c1"] <- raster::cellFromRowCol(r, rows + 1L, cols)
  d[, "c2"] <- raster::cellFromRowCol(r, rows, cols - 1L)
  d[, "c3"] <- raster::cellFromRowCol(r, rows - 1L, cols)
  d[, "c4"] <- raster::cellFromRowCol(r, rows, cols + 1L)

  itr <- 0L
  while(TRUE) {

    FUN <- function(i) {
      x <- rep(NA, nrow(d))
      is <- !is.na(d[, i])
      x[is] <- r[d[is, "cell"]] >= rs[[1]][d[is, i]] - min.overlap
      return(x)
    }
    m <- cbind(FUN("c1"), FUN("c2"), FUN("c3"), FUN("c4"))
    is.disconnected <- apply(m, 1, any, na.rm=TRUE)
    if (all(!is.disconnected)) break

    cells.to.bump <- d[is.disconnected, "cell"]
    r[cells.to.bump] <- r[cells.to.bump] - bump.by

    itr <- itr + 1L
    if (itr > max.itr) {
      warning("maximum iterations reached")
      break
    }

    d <- d[is.disconnected, , drop=FALSE]
  }
  return(r - rs[[2]])
}
