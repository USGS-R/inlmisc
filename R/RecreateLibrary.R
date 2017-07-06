#' Recreate R Library
#'
#' This function can be used to recreate an existing library on a new installation of \R.
#'
#' @param current_version 'character'.
#'   Current version of the \R installation.
#'   Set to \code{"old"} in the older installation,
#'   and \code{"new"} in the freshly installed version.
#' @param file 'character'.
#'   Name of the file for reading or writing the list of package names.
#' @param repos 'character'.
#'   Vector of base URL(s) of the repositories to use;
#'   only required if \code{current_version = "new"}.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' # run on old version of R
#' RecreateLibrary()
#'
#' # run on new version of R (assumes working directory has not changed)
#' RecreateLibrary("new", repos = c(CRAN = "https://cloud.r-project.org/",
#'                                  GRAN = "https://owi.usgs.gov/R"))
#'

RecreateLibrary <- function(current_version=c("old", "new"), file="packagelist.txt",
                            repos=getOption("repos")) {

  current_version <- match.arg(current_version)

  if (current_version == "old") {
    pkgs <- utils::installed.packages()[, 1]
    writeLines(pkgs, file)

  } else {
    utils::update.packages(ask=FALSE, repos=repos)
    pkgs <- readLines(file)
    pkgs <- pkgs[!pkgs %in% utils::installed.packages()[, "Package"]]
    if (length(pkgs) > 0) {
      contriburl <- utils::contrib.url(repos=repos, type=getOption("pkgType"))
      repos_pkgs <- utils::available.packages(contriburl)
      pkgs <- pkgs[pkgs %in% repos_pkgs]
      if (length(pkgs) > 0) {
        type <- ifelse(Sys.info()["sysname"] == "Windows", "win.binary", "source")
        utils::install.packages(pkgs, repos=repos, type=type)
      }
    }
  }

  invisible(NULL)
}
