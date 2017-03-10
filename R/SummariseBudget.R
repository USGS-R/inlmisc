#' Summarize MODFLOW Water Budget
#'
#' This is a utility function for \href{https://water.usgs.gov/ogw/modflow/}{MODFLOW},
#' a U.S. Geological Survey groundwater-flow model.
#' It summarizes volumetric flow rates by boundary condition types.
#' That is, it splits the MODFLOW cell-by-cell budget data into subsets,
#' computes summary statistics for each, and returns the resulting summary table.
#'
#' @param budget character or list.
#'   Either the path to a MODFLOW cell-by-cell budget file or
#'   the object returned from the \code{\link{ReadModflowBinary}} function.
#' @param desc character.
#'    Vector of data type descriptors, such as \code{c("wells", "drains")}.
#' @param id character.
#'    Name of auxiliary variable, additional values associated with each cell.
#'
#' @details Subsets are grouped by the data type (\code{"desc"}), stress period
#'   (\code{"kper"}), time step (\code{"kstp"}), and optional auxiliary variable.
#'   The MODFLOW cell-by-cell budget file must be saved using the
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
#' bud <- SummariseBudget(path, "river leakage", "iface")
#' print(bud)
#'

SummariseBudget <- function(budget, desc, id=NULL) {
  if (!inherits(budget, "list")) {
    budget <- budget[1]
    if (is.character(budget) & file.access(budget) == 0)
      budget <- ReadModflowBinary(budget, "flow")
    else
      stop("problem with 'budget' argument")
  }

  budget.desc <- as.factor(vapply(budget, function(i) i$desc, ""))
  is.desc.not.included <- !desc %in% levels(budget.desc)
  if (any(is.desc.not.included))
    warning(paste("missing flow variable(s) in budget file:",
                  paste(desc[is.desc.not.included], collapse=", ")))
  budget <- budget[budget.desc %in% desc]
  if (length(budget) == 0) stop("flow variable(s) can not be found in the budget file")

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
  FUN <- function(j) {
    d <- data.frame(desc=j$desc, kper=j$kper, kstp=j$kstp, id=NA,
                    flow=j$d[, "flow"], delt=j$delt,
                    pertim=j$pertim, totim=j$totim, stringsAsFactors=FALSE)
    if (!is.null(id) && id %in% colnames(j$d)) d$id <- j$d[, id]
    return(d)
  }
  d <- dplyr::bind_rows(lapply(desc, function(i) dplyr::bind_rows(lapply(b[desc == i], FUN))))
  d$desc <- as.factor(d$desc)
  if ("id" %in% colnames(d))
    grps <- dplyr::group_by_(d, "desc", "kper", "kstp", "id")
  else
    grps <- dplyr::group_by_(d, "desc", "kper", "kstp")
  d <- dplyr::summarise_(grps,
                         delt="delt[1]",
                         pertim="pertim[1]",
                         totim="totim[1]",
                         count="length(flow)",
                         flow.sum="sum(flow)",
                         flow.mean="mean(flow)",
                         flow.median="stats::median(flow)",
                         flow.sd="sd(flow)")
  return(d)
}
