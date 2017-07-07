#' Recreate R Library
#'
#' This function can be used to recreate an existing library on a new installation of \R.
#'
#' @param current_version 'character'.
#'   Current version of the \R installation.
#'   Specify as \code{"old"} when running in the older installation,
#'   and \code{"new"} in the freshly installed version.
#' @param file 'character'.
#'   Name of the file for writing (or reading) the list of package names.
#' @param repos 'character'.
#'   Vector of base URL(s) of the repositories to use;
#'   only required if \code{current_version = "new"}.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link[utils]{update.packages}}, \code{\link[utils]{install.packages}}
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' # Run on old version of R
#' RecreateLibrary()
#'
#' \dontrun{
#' # Run on new version of R (assumes working directory has not changed)
#' RecreateLibrary("new", repos = c(CRAN = "https://cloud.r-project.org/",
#'                                  GRAN = "https://owi.usgs.gov/R"))
#' }
#'
#' unlink("packagelist.txt")
#'

RecreateLibrary <- function(current_version=c("old", "new"), file="packagelist.txt",
                            repos=getOption("repos")) {

  current_version <- match.arg(current_version)

  if (current_version == "old") {
    pkgs <- utils::installed.packages()[, 1]
    writeLines(pkgs, file)
    sprintf("List of package names written to: %s\n", file)

  } else {
    if(!file.exists(file)) stop("package-list file not found")
    type <- ifelse(Sys.info()["sysname"] == "Windows", "win.binary", "source")
    utils::update.packages(ask=FALSE, repos=repos, type=type)
    pkgs <- unique(readLines(file))
    pkgs <- pkgs[!pkgs %in% utils::installed.packages()[, "Package"]]
    if (length(pkgs) == 0) return()
    contriburl <- utils::contrib.url(repos=repos, type=getOption("pkgType"))

    is <- pkgs %in% utils::available.packages(contriburl, type=type)
    if (length(pkgs[!is]) > 0) {
      msg <- sprintf("The following packages are missing from the repositories:\n  %s\n",
                     paste(pkgs[!is], collapse=", "))
      warning(msg)
    }
    pkgs <- pkgs[is]
    if (length(pkgs) == 0) return()
    utils::install.packages(pkgs, repos=repos, type=type)
  }

  invisible(NULL)
}
