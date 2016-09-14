#' Summarize MODFLOW Water Budget
#'
#' This is a utility function for \href{http://water.usgs.gov/ogw/modflow/}{MODFLOW}.
#' It summarizes volumetric flow rates by boundary condition types.
#' That is, it splits the MODFLOW water-budget data into subsets,
#' computes summary statistics for each, and
#' returns the result in a summary table.
#'
#' @param budget character or list.
#'   Either a description of the path to the MODFLOW Budget File or
#'   the returned results from a call to the \code{\link{ReadModflowBinary}} function.
#' @param desc character.
#'    Vector of MODFLOW package identifiers.
#'    Data of this package type is included in the summary table.
#'
#' @details The \code{budget[[i]]$d} data table component must contain a numeric \code{id} field.
#'   Subsets are grouped by the MODFLOW package identifier (\code{desc}), stress period
#'   (\code{kper}), time step (\code{kstp}), and location identifier (\code{id}).
#'
#' @return Returns a data.frame object with the following variables:
#'   \describe{
#'     \item{desc}{MODFLOW package identifier}
#'     \item{kper}{stress period}
#'     \item{kstp}{time step}
#'     \item{id}{location identifier}
#'     \item{delt}{length of the current time step.}
#'     \item{pertim}{time in the stress period.}
#'     \item{totim}{total elapsed time}
#'     \item{count}{number of cells in each subset.}
#'     \item{flow.sum}{total volumetric flow rate}
#'     \item{flow.mean}{mean volumetric flow rate}
#'     \item{flow.median}{median volumetric flow rate}
#'     \item{flow.sd}{tandard deviation of the volumetric flow rate.}
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
#' \dontrun{
#'   path <- file.path(getwd(), "modflow.bud")
#'   d <- SummariseBudget(path)
#' }
#'

SummariseBudget <- function(budget,
                            desc=c("wells", "drains", "river leakage")) {

  if (!inherits(budget, "list")) {
    budget <- budget[1]
    if (is.character(budget) & file.access(budget) == 0)
      budget <- ReadModflowBinary(budget, "flow")
    else
      stop("problem with 'budget' argument")
  }
  desc <- match.arg(desc, several.ok=TRUE)

  budget.desc <- as.factor(vapply(budget, function(i) i$desc, ""))
  is.desc.not.included <- !desc %in% levels(budget.desc)
  if (any(is.desc.not.included ))
    warning(paste("missing flow variable(s) in budget file:",
                  paste(desc[is.desc.not.included], collapse=", ")))
  budget <- budget[budget.desc %in% desc]
  if (length(budget) == 0)
    stop("flow variable(s) can not be found in the budget file")

  descs <- vapply(budget, function(i) make.names(i$desc), "")

  .Summarise <- function(b, desc) {
    FUN <- function(j) {
      d <- data.frame(desc=j$desc, kper=j$kper, kstp=j$kstp, id=NA,
                      flow=j$d[, make.names(j$desc)], delt=j$delt,
                      pertim=j$pertim, totim=j$totim, stringsAsFactors=FALSE)
      if ("id" %in% colnames(j$d)) d$id <- as.integer(j$d[, "id"])
      return(d)
    }
    d <- dplyr::bind_rows(lapply(desc, function(i) dplyr::bind_rows(lapply(b[desc == i], FUN))))
    d$desc <- as.factor(d$desc)
    d <- dplyr::summarise_(dplyr::group_by_(d, "desc", "kper", "kstp", "id"),
                           delt="delt[1]", pertim="pertim[1]", totim="totim[1]",
                           count="length(flow)",
                           flow.sum="sum(flow)",
                           flow.mean="mean(flow)",
                           flow.median="stats::median(flow)",
                           flow.sd="sd(flow)")
    return(d)
  }

  b <- budget
  for (i in seq_along(b)) {
    b[[i]]$d[b[[i]]$d[, descs[i]] < 0, descs[i]] <- 0
  }
  d <- dplyr::mutate(.Summarise(b, desc), flow.dir="in")

  b <- budget
  for (i in seq_along(b)) {
    b[[i]]$d[b[[i]]$d[, descs[i]] > 0, descs[i]] <- 0
  }
  d <- dplyr::bind_rows(d, dplyr::mutate(.Summarise(b, desc), flow.dir="out"))

  d$flow.dir <- as.factor(d$flow.dir)

  return(d)
}
