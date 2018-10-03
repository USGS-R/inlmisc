#' Read MODFLOW Binary File
#'
#' This is a utility function for \href{https://water.usgs.gov/ogw/modflow/}{MODFLOW}-based models,
#' the U.S. Geological Survey's three-dimensional finite-difference groundwater model.
#' It reads data from binary files produced by MODFLOW.
#'
#' @param path 'character'.
#'   Path to a MODFLOW binary file.
#' @param data.type 'character'.
#'    Description of how the data were saved.
#'    Specify \code{"array"} for array data (such as hydraulic heads or drawdowns) and
#'    \code{"flow"} for cell-by-cell flow data (budget data).
#' @param endian 'character'.
#'    The endian-ness (or byte-order) of the binary file.
#' @param rm.totim.0 'logical'.
#'    If true, data associated with the stress period at time zero are removed.
#'
#' @return Returns a 'list' object of length equal to the
#'   number of times data are written to the binary file.
#'   The following list components are returned:
#'   \describe{
#'     \item{d}{matrix of values.
#'       The matrix dimensions typically coincide with the horizontal model grid.
#'       The exception is for flow data (\code{data.type = "flow"}) when the
#'       cell-by-cell budget file is saved using the \emph{\bold{"COMPACT BUDGET"}} output option;
#'       for this case, matrix columns are: cell index (\code{"icell"}),
#'       model-grid layer (\code{"layer"}), model-grid row (\code{"row"}),
#'       model-grid column (\code{"column"}), cell-by-cell flow (\code{"flow"}),
#'       and any auxiliary variables saved using the \emph{\bold{"AUXILIARY"}} output option.}
#'     \item{kstp}{time step}
#'     \item{kper}{stress period}
#'     \item{desc}{description of data-type, such as "wells".}
#'     \item{layer}{model-grid layer}
#'     \item{delt}{time-step size}
#'     \item{pertim}{elapsed time in the current stress period.}
#'     \item{totim}{total elapsed time}
#'   }
#'   The layer component (layer) and time components (delt, pertim, totim) are only available
#'   for flow data when the cell-by-cell budget file is saved using the
#'   \emph{\bold{"COMPACT BUDGET"}} output option.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{SummariseBudget}}
#'
#' @keywords IO
#'
#' @export
#'
#' @examples
#' path <- system.file("extdata", "ex.hds", package = "inlmisc")
#' heads <- ReadModflowBinary(path, "array")
#' image(heads[[1]]$d)
#' str(heads[[1]])
#'
#' path <- system.file("extdata", "ex.bud", package = "inlmisc")
#' budget <- ReadModflowBinary(path, "flow")
#' image(budget[[1]]$d)
#' str(budget[[1]])
#' str(budget[[11]])
#'

ReadModflowBinary <- function(path, data.type=c("array", "flow"),
                              endian=c("little", "big"), rm.totim.0=FALSE) {

  checkmate::assertFileExists(path)
  data.type <- match.arg(data.type)
  endian <- match.arg(endian)
  checkmate::assertFlag(rm.totim.0)

  ans <- try(.ReadBinary(path, data.type, endian, nbytes=4L), silent=TRUE)
  if (inherits(ans, "try-error"))
    ans <- .ReadBinary(path, data.type, endian, nbytes=8L)
  if (rm.totim.0)
    ans <- ans[vapply(ans, function(i) i$totim, 0) != 0]
  ans
}


.ReadBinary <- function(path, data.type, endian, nbytes) {

  checkmate::assertFileExists(path)
  checkmate::assertString(data.type)
  checkmate::assertString(endian)
  stopifnot(nbytes %in% c(4L, 8L))

  con <- file(path, open="rb", encoding="bytes")
  on.exit(close(con, type="rb"))

  # kstp:   time step number
  # kper:   stress period number
  # pertim: time in current stress period
  # totim:  total elapsed time
  # desc:   data-type description
  # ncol:   number of columns in the model grid
  # nrow:   number of rows in the model grid
  # nlay:   number of layers in the model grid
  # layer:  single layer number
  # itype:  data storage type
  # delt:   length of time step
  # nval:   number of values for each cell
  # ctmp:   description of additional values
  # nlist:  number of cells for which values will be stored

  if (data.type == "array")
    valid.desc <- c("center elevation",
                    "change in eff-st",
                    "change in g-strs",
                    "change in pcstrs",
                    "compaction",
                    "critical head",
                    "d critical head",
                    "drawdown",
                    "dsys compaction",
                    "effective stress",
                    "geostatic stress",
                    "head",
                    "head in hgu",
                    "layer compaction",
                    "nd critical head",
                    "ndsys compaction",
                    "preconsol stress",
                    "subsidence",
                    "system compaction",
                    "thickness",
                    "void ratio",
                    "z displacement")
  else
    valid.desc <- c("constant head",
                    "drains",
                    "flow front face",
                    "flow lower face",
                    "flow right face",
                    "lake seepage",
                    "river leakage",
                    "storage",
                    "wells")
  lst <- list()
  repeat {
    kstp <- readBin(con, "integer", n=1L, size=4L, endian=endian)
    if (length(kstp) == 0) break
    kper <- readBin(con, "integer", n=1L, size=4L, endian=endian)

    if (data.type == "array") {
      pertim <- readBin(con, "numeric", n=1L, size=nbytes, endian=endian)
      totim  <- readBin(con, "numeric", n=1L, size=nbytes, endian=endian)
      desc   <- readBin(readBin(con, "raw", n=16L, size=1L, endian=endian),
                        "character", n=1L, endian=endian)
      desc <- .TidyDescription(desc)
      if (!desc %in% valid.desc) break
      ncol  <- readBin(con, "integer", n=1L, size=4L, endian=endian)
      nrow  <- readBin(con, "integer", n=1L, size=4L, endian=endian)
      layer <- readBin(con, "integer", n=1L, size=4L, endian=endian)
      v     <- readBin(con, "numeric", n=nrow * ncol, size=nbytes, endian=endian)
      d <- matrix(v, nrow=nrow, ncol=ncol, byrow=TRUE)
      lst[[length(lst) + 1]] <- list(d=d, kstp=kstp, kper=kper, desc=desc,
                                     layer=layer, pertim=pertim, totim=totim)

    } else if (data.type == "flow") {
      desc <- readBin(readBin(con, "raw", n=16L, size=1L, endian=endian),
                      "character", n=1L, endian=endian)
      desc <- .TidyDescription(desc)
      if (!desc %in% valid.desc) break
      ncol <- readBin(con, "integer", n=1L, size=4L, endian=endian)
      nrow <- readBin(con, "integer", n=1L, size=4L, endian=endian)
      nlay <- readBin(con, "integer", n=1L, size=4L, endian=endian)

      if (nlay > 0) {
        x <- .Read3dArray(con, nrow, ncol, nlay, nbytes, endian)
        for (i in seq_len(nlay)) {
          lst[[length(lst) + 1]] <- list(d=x[[i]], kstp=kstp, kper=kper,
                                         desc=desc, layer=i)
        }

      } else {  # compact form is used
        nlay <- abs(nlay)
        itype  <- readBin(con, "integer", n=1L, size=4L, endian=endian)
        delt   <- readBin(con, "numeric", n=1L, size=nbytes, endian=endian)
        pertim <- readBin(con, "numeric", n=1L, size=nbytes, endian=endian)
        totim  <- readBin(con, "numeric", n=1L, size=nbytes, endian=endian)

        if (itype == 5L)
          nval <- readBin(con, "integer", n=1L, size=4L, endian=endian)
        else
          nval <- 1L
        if (nval > 100) stop("more than one-hundred varaiables for each cell")
        if (nval > 1) {
          ctmp <- readBin(readBin(con, "raw", n=16L, size=1L, endian=endian),
                          "character", n=nval - 1, endian=endian)
          ctmp <- .TidyDescription(ctmp)
        } else {
          ctmp <- NULL
        }

        if (itype %in% c(0L, 1L)) {
          nvalues <- ncol * nrow * nlay
          d <- .Read3dArray(con, nrow, ncol, nlay, nbytes, endian)
          for (i in seq_along(d)) {
            lst[[length(lst) + 1]] <- list(d=d[[i]], kstp=kstp, kper=kper,
                                           desc=desc, layer=i, delt=delt,
                                           pertim=pertim, totim=totim)
          }

        } else if (itype %in% c(2L, 5L)) {
            nlist <- readBin(con, "integer", n=1L, size=4L, endian=endian)
            if (nlist > (nrow * ncol * nlay))
              stop("large number of cells for which values will be stored")
            if (nlist > 0) {
              d <- matrix(0, nrow=nlist, ncol=nval + 4)
              colnames(d) <- make.names(c("icell", "layer", "row", "column", "flow", ctmp),
                                        unique=TRUE)
              for (i in seq_len(nlist)) {
                d[i, 1] <- readBin(con, "integer", n=1L, size=4L, endian=endian)
                d[i, seq_len(nval) + 4] <- readBin(con, "numeric", n=nval,
                                                   size=nbytes, endian=endian)
              }
              nrc <- nrow * ncol
              d[, "layer"] <- as.integer((d[, "icell"] - 1) / nrc + 1)
              d[, "row"]  <- as.integer(((d[, "icell"] - (d[, "layer"] - 1) * nrc) - 1) / ncol + 1)
              d[, "column"] <- as.integer(d[, "icell"] - (d[, "layer"] - 1) * nrc - (d[, "row"] - 1) * ncol)
              lst[[length(lst) + 1]] <- list(d=d, kstp=kstp, kper=kper, desc=desc,
                                             delt=delt, pertim=pertim, totim=totim)
            }

        } else if (itype == 3L) {
          layers <- readBin(con, "integer", n=nrow * ncol, size=4L, endian=endian)
          values <- readBin(con, "numeric", n=nrow * ncol, size=nbytes, endian=endian)
          for (i in sort(unique(layers))) {
            v <- values[layers == i]
            d <- matrix(v, nrow=nrow, ncol=ncol, byrow=TRUE)
            lst[[length(lst) + 1]] <- list(d=d, kstp=kstp, kper=kper, desc=desc,
                                           layer=i, delt=delt, pertim=pertim, totim=totim)
          }

        } else if (itype == 4L) {
          v <- readBin(con, "numeric", n=nrow * ncol, size=nbytes, endian=endian)
          d <- matrix(v, nrow=nrow, ncol=ncol, byrow=TRUE)
          lst[[length(lst) + 1]] <- list(d=d, kstp=kstp, kper=kper, desc=desc,
                                         layer=1L, delt=delt, pertim=pertim, totim=totim)
          d[, ] <- 0
          for (i in seq_len(nlay)[-1]) {
            lst[[length(lst) + 1]] <- list(d=d, kstp=kstp, kper=kper, desc=desc,
                                           layer=i, delt=delt, pertim=pertim, totim=totim)
          }

        } else {
          stop("data storage type is not recognized")
        }
      }
    }
  }
  lst
}


.Read3dArray <- function(con, nrow, ncol, nlay, nbytes, endian) {

  checkmate::assertClass(con, c("file", "connection"))
  checkmate::assertCount(nrow, positive=TRUE)
  checkmate::assertCount(ncol, positive=TRUE)
  checkmate::assertCount(nlay, positive=TRUE)
  checkmate::assertInt(nbytes)
  checkmate::assertString(endian)

  lapply(seq_len(nlay), function(i) {
    v <- readBin(con, "numeric", n=nrow * ncol, size=nbytes, endian=endian)
    matrix(v, nrow=nrow, ncol=ncol, byrow=TRUE)
  })
}


.TidyDescription <- function(desc) {
  checkmate::assertCharacter(desc)
  tolower(gsub("(^ +)|( +$)", "", desc))
}
