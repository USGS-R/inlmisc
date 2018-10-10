#' Adjust Implausible River Stage
#'
#' This function decreases stage values in river cells if they are implausible
#' with respect to water always flowing downhill.
#'
#' @param r 'RasterLayer'.
#'   Numeric cell values representing river stages.
#' @param outlets 'SpatialPoints*', 'SpatialLines*', 'SpatialPolygons*' or 'Extent'.
#'   Designates the location of discharge outlets.
#'   The \code{\link{rasterize}} function is used to locate outlet cells in the raster grid \code{r}.
#' @param min.drop 'numeric' number.
#'   Minimum drop in stage between adjacent river cells.
#'
#' @details The \href{https://en.wikipedia.org/wiki/Lee_algorithm}{Lee algorithm} (Lee, 1961)
#'   is used to identify flow paths among the modeled river cells.
#'   An analysis of river cell stage values along a flow path identifies any problematic cells
#'   that are obstructing downhill surface-water flow.
#'   Stage values for these problematic cells are then lowered to an acceptable elevation.
#'
#' @return Returns a 'RasterLayer' with cell values representing the vertical change in stream stage.
#'   These changes can be added to \code{r} to ensure that water always flows downhill.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @references
#'   Lee, C.Y., 1961, An algorithm for path connections and its applications:
#'   IRE Transactions on Electronic Computers, v. EC-10, no. 2, p. 346--365.
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' print("notyet")
#'

BumpRiverStage <- function(r, outlets, min.drop=1e-06) {

  r.save <- r
  r.out <- raster::rasterize(outlets, r)

  riv.cells <- which(!is.na(r[]))
  out.cells <- which(!is.na(r.out[]))

  adj.cells <- raster::adjacent(raster::raster(r), cells=out.cells, pairs=FALSE)
  out.cells <- riv.cells[riv.cells %in% adj.cells]

  adj <- unique(raster::adjacent(r, riv.cells, sorted=TRUE))
  adj <- cbind(adj, is=as.integer(adj[, "to"] %in% riv.cells))
  adj <- adj[adj[, "is"] == 1, c("from", "to")]

  # Move up the river system finding all possible source (stuck) cells off the main paths.

  FUN <- function(path) {
    i <- 1L
    repeat {
      cells <- adj[adj[, "from"] == path[i], "to"]
      cells <- cells[!cells %in% visited.cells & !cells %in% out.cells]
      if (length(cells) == 0) {
        stuck.cells <<- c(stuck.cells, utils::tail(path, 1))
        return(NULL)
      }
      i <- i + 1L
      if (length(cells) == 1) {
        path[i] <- cells
        visited.cells <<- c(visited.cells, cells)
      } else {
        visited.cells <<- c(visited.cells, cells)
        return(cells)
      }
    }
  }

  source.cells <- list()
  for (i in seq_along(out.cells)) {
    breaks <- out.cells[i]
    stuck.cells <- breaks
    visited.cells <- breaks
    repeat {
      if (is.null(breaks)) break
      breaks <- unique(unlist(lapply(breaks, FUN)))
    }
    stuck.cells <- stuck.cells[!stuck.cells %in% out.cells]
    if (length(stuck.cells) > 0) source.cells[[i]] <- stuck.cells
  }

  sink.cells <- out.cells[!vapply(source.cells, is.null, FALSE)]
  source.cells <- unlist(source.cells)

  # Find paths by finding the shortest path through the river maze.
  # Lee algorithm, https://en.wikipedia.org/wiki/Lee_algorithm

  CalcWaveExpansion <- function(cells) {
    visited.cells <- NULL
    i <- 0L
    dist <- rep(NA, length(riv.cells))
    dist[which(riv.cells %in% cells)] <- i
    repeat {
      visited.cells <- c(visited.cells, cells)
      adj.cells <- adj[adj[, "from"] %in% cells, "to"]
      adj.cells <- adj.cells[!adj.cells %in% source.cells &
                             !adj.cells %in% visited.cells]
      if (length(adj.cells) > 0) {
        cells <- adj.cells
        i <- i + 1L
        dist[which(riv.cells %in% cells)] <- i
      } else {
        break
      }
    }
    dist
  }
  dists <- CalcWaveExpansion(sink.cells)

  BacktracePath <- function(cell) {
    path <- NULL
    repeat {
      path <- c(path, cell)
      adj.cells <- adj[adj[, "from"] == cell, "to"]
      adj.cells <- adj.cells[!adj.cells %in% source.cells &
                             !adj.cells %in% path]
      cell <- adj.cells[.WhichMin(dists[riv.cells %in% adj.cells])]
      if (cell %in% sink.cells) break
    }
    path
  }
  paths <- lapply(source.cells, BacktracePath)

  # Drop stage along each path

  DropStageAlongPath <- function(path) {
    repeat {
      difs <- diff(r[path])
      i <- match(TRUE, difs > 0)
      if (is.na(i))
        break
      cell <- path[i + 1]
      r[cell] <<- r[cell] - difs[i] - min.drop
    }
    invisible(difs)
  }
  lapply(paths, DropStageAlongPath)

  # Set stage for river cells not on any of the paths

  SetNonPathCells <- function(cell) {
    adj.cells <- adj[adj[, "from"] %in% cell, "to"]
    adj.cells <- adj.cells[!adj.cells %in% source.cells]
    ave.top <- mean(r[adj.cells])
    if (ave.top < r[cell]) r[cell] <<- ave.top
    dif <- r[cell] -  ave.top
    invisible(dif)
  }
  lapply(source.cells, SetNonPathCells)

  r - r.save
}


.WhichMin <- function(x) {
  y <- seq_along(x)[x == min(x)]
  if (length(y) > 1) {
    set.seed(42)
    y <- sample(y, 1)
  }
  y
}
