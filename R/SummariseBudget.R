#' Summarize MODFLOW Water Budget
#'
#' Summarize \href{https://water.usgs.gov/ogw/modflow/}{MODFLOW}
#' volumetric flow rates by boundary condition types.
#' Cell-by-cell flow data is split into subsets,
#' summary statistics computed for each subset, and a summary table returned.
#'
#' @param budget 'character' string or 'list'.
#'   Either the path to a MODFLOW cell-by-cell budget file or
#'   the object returned from the \code{\link{ReadModflowBinary}} function.
#' @param desc 'character' vector.
#'   Data-type descriptors, such as \code{c("wells", "drains")}.
#'   If missing, all data types are summarized.
#' @param id 'character' string.
#'   Name of auxiliary variable, a variable of additional values associated with each cell
#'   saved using the \emph{\bold{"AUXILIARY"}} output option.
#'
#' @details Subsets are grouped by data type (desc), stress period
#'   (kper), time step (kstp), and optional auxiliary variable.
#'   Data in the MODFLOW cell-by-cell budget file must be saved using the
#'   \emph{\bold{"COMPACT BUDGET"}} output option.
#'
#' @return Returns a 'data.table' with the following variables:
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
#' @importFrom data.table data.table
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
  checkmate::assertCharacter(desc, any.missing=FALSE, min.len=1,
                             unique=TRUE, null.ok=TRUE)
  checkmate::assertString(id, null.ok=TRUE)

  budget.desc <- vapply(budget, function(i) i$desc, "")
  if (!is.null(desc)) {
    is <- desc %in% budget.desc
    if (all(!is))
      stop("data type(s) not found in budget")
    if (any(!is))
      warning(sprintf("data type(s) not found in budget: %s",
                      paste(paste0("\"", desc[!is], "\""), collapse=", ")))
    budget <- budget[budget.desc %in% desc]
  }

  is <- vapply(budget, function(x) !is.null(colnames(x$d)), FALSE)
  if (all(!is))
    stop("data type(s) not saved using correct form")
  if (any(!is)) {
    x <- unique(vapply(budget[!is], function(i) i$desc, ""))
    warning(sprintf("removed data type(s): %s not saved using correct form",
                    paste(paste0("\"", x, "\""), collapse=", ")))
  }
  budget <- budget[is]

  dt <- data.table::rbindlist(lapply(budget, function(x) {
    d <- data.table::data.table(desc=x$desc, kper=x$kper, kstp=x$kstp, id=NA,
                                flow=x$d[, "flow"], delt=x$delt,
                                pertim=x$pertim, totim=x$totim, flow.dir="in")
    if (!is.null(id) && id %in% colnames(x$d)) d$id <- x$d[, id]
    d$flow.dir[d$flow < 0] <- "out"
    d
  }))
  dt$desc <- as.factor(dt$desc)
  dt$flow.dir <- as.factor(dt$flow.dir)

  # due to NSE notes in R CMD check
  delt <- flow <- flow.dir <- kper <- kstp <- pertim <- totim <- NULL

  dt[, list(delt        = utils::head(delt, 1),
            pertim      = utils::head(pertim, 1),
            totim       = utils::head(totim, 1),
            count       = length(flow),
            flow.sum    = sum(flow),
            flow.mean   = mean(flow),
            flow.median = stats::median(flow),
            flow.sd     = stats::sd(flow)),
     by=list(desc, kper, kstp, flow.dir, id)]
}
