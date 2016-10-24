#' Read MODFLOW Binary File
#'
#' This is a utility function for \href{http://water.usgs.gov/ogw/modflow/}{MODFLOW}.
#' It reads binary output data from a model run.
#'
#' @param path character.
#'   Path name of the binary file.
#' @param data.type character.
#'    Description of how the data are saved.
#' @param rm.totim.0 logical.
#'    If true, stress-periods at time zero are removed.
#'
#' @details This function reads binary head (\file{.hds}), drawdown (\file{.ddn}),
#'   and budget (\file{.bud}) files generated from a MODFLOW run.
#'
#' @return Returns a \code{list} object of length equal to the
#'   number of times the data type is written to the binary file.
#'   The following list components are returned:
#'   \describe{
#'     \item{d}{matrix of data values.}
#'     \item{kstp}{time step}
#'     \item{kper}{stress period}
#'     \item{desc}{variable name}
#'     \item{ilay}{model-grid layer}
#'     \item{delt}{length of the current time step.}
#'     \item{pertim}{time in the stress period.}
#'     \item{totim}{total elapsed time}
#'   }
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
#' path <- system.file("extdata", "ex3D.hds", package = "inlmisc")
#' hds <- ReadModflowBinary(path, "array")
#'

ReadModflowBinary <- function(path, data.type=c("array", "flow"),
                              rm.totim.0=FALSE) {
  if (!file.exists(path))
    stop("binary file can not be found")

  data.type <- match.arg(data.type)

  ans <- try(.ReadBinary(path, data.type, nbytes=4L), silent=TRUE)
  if (inherits(ans, "try-error"))
    ans <- .ReadBinary(path,  data.type, nbytes=8L)

  if (rm.totim.0)
    ans <- ans[vapply(ans, function(i) i$totim, 0) != 0]

  return(ans)
}


.Read3dArray <- function(con, nrow, ncol, nlay, nbytes) {
  FUN <- function(i) {
    v <- readBin(con, "numeric", n=nrow * ncol, size=nbytes)
    return(matrix(v, nrow=nrow, ncol=ncol, byrow=TRUE))
  }
  return(lapply(seq_len(nlay), FUN))
}


.TidyDescription <- function(desc) {
  return(tolower(gsub("(^ +)|( +$)", "", desc)))
}


.ReadBinary <- function(path, data.type, nbytes) {
  con <- file(path, open="rb", encoding="bytes")
  on.exit(close(con, type="rb"))

  ## kstp:   time step number
  ## kper:   stress period number
  ## pertim: time in current stress period
  ## totim:  total elapsed time
  ## desc:   variable name
  ## ncol:   number of columns in the model grid
  ## nrow:   number of rows in the model grid
  ## nlay:   number of layers in the model grid
  ## ilay:   single layer number
  ## itype:  data storage type
  ## delt:   length of time step
  ## nval:   number of values for each cell
  ## ctmp:   description of additional values
  ## nlist:  number of cells for which values will be stored

  if (data.type == "array")
    valid.desc <- c("head", "drawdown", "subsidence", "compaction",
                    "critical head", "head in hgu", "ndsys compaction",
                    "z displacement", "d critical head", "layer compaction",
                    "dsys compaction", "nd critical head", "system compaction",
                    "preconsol stress", "change in pcstrs", "effective stress",
                    "change in eff-st", "void ratio", "thickness",
                    "center elevation", "geostatic stress", "change in g-strs")
  else
    valid.desc <- c("storage", "constant head", "flow right face",
                    "flow front face", "flow lower face", "wells", "drains",
                    "river leakage")
  lst <- list()
  repeat {
    kstp <- readBin(con, "integer", n=1L, size=4L)
    if (length(kstp) == 0) break
    kper <- readBin(con, "integer", n=1L, size=4L)

    if (data.type == "array") {
      pertim <- readBin(con, "numeric", n=1L, size=nbytes)
      totim  <- readBin(con, "numeric", n=1L, size=nbytes)
      desc   <- readBin(readBin(con, "raw", n=16L, size=1L), "character", n=1L)
      desc <- .TidyDescription(desc)
      if (!desc %in% valid.desc) break
      ncol   <- readBin(con, "integer", n=1L, size=4L)
      nrow   <- readBin(con, "integer", n=1L, size=4L)
      ilay   <- readBin(con, "integer", n=1L, size=4L)
      v      <- readBin(con, "numeric", n=nrow * ncol, size=nbytes)
      d <- matrix(v, nrow=nrow, ncol=ncol, byrow=TRUE)
      lst[[length(lst) + 1L]] <- list(d=d, kstp=kstp, kper=kper, desc=desc,
                                      ilay=ilay, pertim=pertim, totim=totim)

    } else {
      desc <- readBin(readBin(con, "raw", n=16L, size=1L), "character", n=1L)
      desc <- .TidyDescription(desc)
      if (!desc %in% valid.desc) break
      ncol <- readBin(con, "integer", n=1L, size=4L)
      nrow <- readBin(con, "integer", n=1L, size=4L)
      nlay <- readBin(con, "integer", n=1L, size=4L)
      if (nlay > 0) {
        d <- .Read3dArray(con, nrow, ncol, nlay, nbytes)
      } else {
        nlay <- abs(nlay)
        itype  <- readBin(con, "integer", n=1L, size=4L)
        delt   <- readBin(con, "numeric", n=1L, size=nbytes)
        pertim <- readBin(con, "numeric", n=1L, size=nbytes)
        totim  <- readBin(con, "numeric", n=1L, size=nbytes)

        if (itype == 5L)
          nval <- readBin(con, "integer", n=1L, size=4L)
        else
          nval <- 1L
        if (nval > 100L)
          stop("more than one-hundred varaiables for each cell")
        if (nval > 1L) {
          ctmp <- readBin(readBin(con, "raw", n=16L, size=1L), "character",
                          n=nval - 1L)
          ctmp <- .TidyDescription(ctmp)
        } else {
          ctmp <- NULL
        }
        if (itype %in% c(2L, 5L)) {
          nlist <- readBin(con, "integer", n=1L, size=4L)
          if (nlist > (nrow * ncol * nlay))
            stop("large number of cells for which values will be stored")
        }
        if (itype %in% c(0L, 1L)) {
          nvalues <- ncol * nrow * nlay
          d <- .Read3dArray(con, nrow, ncol, nlay, nbytes)
          for (i in seq_along(d)) {
            lst[[length(lst) + 1L]] <- list(d=d[[i]], kstp=kstp, kper=kper,
                                            desc=desc, ilay=i, delt=delt,
                                            pertim=pertim, totim=totim)
          }
        } else if (itype %in% c(2L, 5L)) {
            if (nlist > 0L) {
              d <- matrix(0, nrow=nlist, ncol=4L + nval)
              colnames(d) <- make.names(c("icell", "layer", "row", "column",
                                          desc, ctmp), unique=TRUE)
              for (i in seq_len(nlist)) {
                d[i, 1] <- readBin(con, "integer", n=1L, size=4L)
                d[i, 4L + seq_len(nval)] <- readBin(con, "numeric", n=nval,
                                                    size=nbytes)
              }
              nrc <- nrow * ncol
              d[, "layer"] <- as.integer((d[, "icell"] - 1L) / nrc + 1L)
              d[, "row"] <- as.integer(((d[, "icell"] - (d[, "layer"] - 1L) *
                                        nrc) - 1L) / ncol + 1L)
              d[, "column"] <- as.integer(d[, "icell"] - (d[, "layer"] - 1L) *
                                          nrc - (d[, "row"] - 1L) * ncol)
              lst[[length(lst) + 1L]] <- list(d=d, kstp=kstp, kper=kper,
                                              desc=desc, delt=delt,
                                              pertim=pertim, totim=totim)
            }
        } else if (itype == 3L) {
          layers <- readBin(con, "integer", n=nrow * ncol, size=4L)
          values <- readBin(con, "numeric", n=nrow * ncol, size=nbytes)
          for (i in sort(unique(layers))) {
            v <- values[layers == i]
            d <- matrix(v, nrow=nrow, ncol=ncol, byrow=TRUE)
            lst[[length(lst) + 1L]] <- list(d=d, kstp=kstp, kper=kper,
                                            desc=desc, ilay=i, delt=delt,
                                            pertim=pertim, totim=totim)
          }
        } else if (itype == 4L) {
          v <- readBin(con, "numeric", n=nrow * ncol, size=nbytes)
          d <- matrix(v, nrow=nrow, ncol=ncol, byrow=TRUE)
          lst[[length(lst) + 1L]] <- list(d=d, kstp=kstp, kper=kper, desc=desc,
                                          ilay=1L, delt=delt, pertim=pertim,
                                          totim=totim)
          d[, ] <- 0
          for (i in seq_len(nlay)[-1]) {
            lst[[length(lst) + 1L]] <- list(d=d, kstp=kstp, kper=kper,
                                            desc=desc, ilay=i, delt=delt,
                                            pertim=pertim, totim=totim)
          }
        } else {
          stop("data storage type is not recognized")
        }
      }
    }
  }
  return(lst)
}
