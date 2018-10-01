#' Summarize MODFLOW Water Budget
#'
#' This is a utility function for \href{https://water.usgs.gov/ogw/modflow/}{MODFLOW}-based models,
#' the U.S. Geological Survey's three-dimensional finite-difference groundwater model.
#' It summarizes volumetric flow rates by boundary condition types.
#' That is, it splits the MODFLOW cell-by-cell flow data into subsets,
#' computes summary statistics for each, and returns a resulting summary table.
#'
#' @param budget 'character' or 'list'.
#'   Either the path to a MODFLOW cell-by-cell budget file or
#'   the object returned from the \code{\link{ReadModflowBinary}} function.
#' @param desc 'character'.
#'    Vector of data-type descriptors, such as \code{c("wells", "drains")}.
#'    If missing, all data types are summarized.
#' @param id 'character'.
#'    Name of auxiliary variable, a variable of additional values associated with each cell
#'    saved using the \emph{\bold{"AUXILIARY"}} output option.
#'
#' @details Subsets are grouped by data type (desc), stress period
#'   (kper), time step (kstp), and optional auxiliary variable.
#'   Data in the MODFLOW cell-by-cell budget file must be saved using the
#'   \emph{\bold{"COMPACT BUDGET"}} output option.
#'
#' @return Returns a 'data.frame' object with the following variables:
#'   \describe{
#'     \item{desc}{description of data type, such as "wells".}
#'     \item{kper}{stress period}
#'     \item{kstp}{time step}
#'     \item{id}{auxiliary variable name}
#'     \item{delt}{length of the current time step.}
#'     \item{pertim}{time in the stress period.}
#'     \item{totim}{total elapsed time}
#'     \item{count}{number of cells in each subset.}
#'     \item{flow.sum}{total volumetric flow rate}
#'     \item{flow.mean}{mean volumetric flow rate}
#'     \item{flow.median}{median volumetric flow rate}
#'     \item{flow.sd}{standard deviation of volumetric flow rate.}
#'     \item{flow.dir}{flow direction where "in" and "out" indicate
#'       water entering and leaving the groundwater system, respectively.}
#'   }
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{ReadModflowBinary}}
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' path <- system.file("extdata", "ex.bud", package = "inlmisc")
#' out <- SummariseBudget(path, desc = "river leakage", id = "iface")
#' print(out)
#'

SummariseBudget <- function(budget, desc=NULL, id=NULL) {

  stopifnot(inherits(budget, c("character", "list")))
  if (is.character(budget)) {
    checkmate::assertFileExists(budget)
    budget <- ReadModflowBinary(budget, "flow")
  }
  checkmate::assertCharacter(desc, any.missing=FALSE, min.len=1, unique=TRUE, null.ok=TRUE)
  checkmate::assertString(id, null.ok=TRUE)

  budget.desc <- vapply(budget, function(i) i$desc, "")
  if (is.null(desc)) {
    desc <- unique(budget.desc)
  } else {
    is <- desc %in% budget.desc
    if (all(!is)) stop("data type(s) not found in budget")
    if (any(!is))
      warning(sprintf("data type(s) not found in budget: %s",
                      paste(paste0("\"", desc[!is], "\""), collapse=", ")))
    budget <- budget[budget.desc %in% desc]
  }

  is <- vapply(budget, function(x) !is.null(colnames(x$d)), FALSE)
  if (all(!is)) stop("data type(s) not saved using correct form")
  if (any(!is)) {
    x <- unique(vapply(budget[!is], function(i) i$desc, ""))
    warning(sprintf("removed data type(s): %s not saved using correct form",
            paste(paste0("\"", x, "\""), collapse=", ")))
  }
  budget <- budget[is]
  desc <- vapply(budget, function(i) i$desc, "")

  b <- budget
  for (i in seq_along(b)) b[[i]]$d[b[[i]]$d[, "flow"] < 0, "flow"] <- 0
  d <- dplyr::mutate(.Summarise(b, desc, id), flow.dir="in")

  b <- budget
  for (i in seq_along(b)) b[[i]]$d[b[[i]]$d[, "flow"] > 0, "flow"] <- 0
  d <- dplyr::bind_rows(d, dplyr::mutate(.Summarise(b, desc, id), flow.dir="out"))

  d$flow.dir <- as.factor(d$flow.dir)

  return(d)
}


.Summarise <- function(b, desc, id) {

  checkmate::assertList(b)
  checkmate::assertCharacter(desc)
  checkmate::assertString(id, null.ok=TRUE)

  d <- dplyr::bind_rows(lapply(desc, function(i) {
    dplyr::bind_rows(lapply(b[desc == i], function(j) {
      d <- data.frame(desc=j$desc, kper=j$kper, kstp=j$kstp, id=NA,
                      flow=j$d[, "flow"], delt=j$delt,
                      pertim=j$pertim, totim=j$totim, stringsAsFactors=FALSE)
      if (!is.null(id) && id %in% colnames(j$d)) d$id <- j$d[, id]
      return(d)
    }))
  }))
  d$desc <- as.factor(d$desc)
  if ("id" %in% colnames(d))
    grps <- dplyr::group_by_(d, "desc", "kper", "kstp", "id")
  else
    grps <- dplyr::group_by_(d, "desc", "kper", "kstp")
  d <- dplyr::summarise_(grps,
                         delt        = "delt[1]",
                         pertim      = "pertim[1]",
                         totim       = "totim[1]",
                         count       = "length(flow)",
                         flow.sum    = "sum(flow)",
                         flow.mean   = "mean(flow)",
                         flow.median = "stats::median(flow)",
                         flow.sd     = "sd(flow)")
  return(d)
}
