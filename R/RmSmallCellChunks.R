#' Remove Small Cell Chunks
#'
#' This function identifies cell chunks in a single raster grid layer,
#' where a cell chunk is defined as a group of connected cells with non-missing values.
#' The cell chunk with the largest surface area is preserved and all others removed.
#'
#' @param r RasterLayer.
#'   A raster grid layer with cell values.
#'
#' @return The raster grid layer \code{r} with cell values in the smaller cell chunks set to \code{NA}.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{clump}}
#'
#' @keywords utilities
#'
#' @import sp
#' @import raster
#' @importFrom igraph clusters
#'
#' @export
#'
#' @examples
#' library(raster)
#'
#' set.seed(0)
#' r <- raster(ncols = 10, nrows = 10)
#' r[] <- round(runif(ncell(r)) * 0.7)
#' r <- clump(r)
#' plot(r)
#'
#' r.new <- RmSmallCellChunks(r)
#' plot(r.new, zlim = range(r[], na.rm = TRUE))
#'

RmSmallCellChunks <- function(r) {

  ext <- extent(r)
  new.ext <- extent(c(ext@xmin - res(r)[1], ext@xmax + res(r)[1],
                      ext@ymin - res(r)[2], ext@ymax + res(r)[2]))
  new.r <- extend(r, new.ext)

  r.values <- new.r[]
  r.clump <- clump(new.r, directions=4)
  chunk.numbers <- r.clump[]
  chunks <- unique(stats::na.omit(chunk.numbers))
  FUN <- function(i) sum(chunk.numbers == i, na.rm=TRUE)
  chunk.sizes <- vapply(chunks, FUN, 0)

  biggest.chunk <- chunks[which(chunk.sizes == max(chunk.sizes))]
  n <- length(biggest.chunk)
  if (n > 1L)
    warning(paste("There are", n, "raster chunks with largest area."))

  chunk.numbers[!is.na(chunk.numbers) & !chunk.numbers %in% biggest.chunk] <- NA
  r.values[is.na(chunk.numbers)] <- NA
  new.r[] <- r.values

  r <- crop(new.r, ext)

  return(r)
}
