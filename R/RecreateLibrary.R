#' Recreate R Library
#'
#' This function can be used to recreate an existing library on a new installation of \R.
#'
#' @param desc 'character'.
#'   Description of current \R installation.
#'   Specify as \code{"old"} when running on an older installation,
#'   and \code{"new"} when on a newer installation.
#' @param file 'character'.
#'   Name of the file for writing (or reading) the list of package names.
#' @param lib 'character'.
#'   If \code{desc = "old"}, the library tree(s) to search through when locating installed packages (see \code{\link{.libPaths}});
#'   otherwise, it is the library directory to install packages.
#' @param repos 'character'.
#'   Vector of base URL(s) of the repositories to use when installing packages.
#'   For example, the URL of the Geological Survey R Archive Network (GRAN) is \code{"https://owi.usgs.gov/R"}.
#'
#' @details A typical workflow is as follows:
#' Run the \code{RecreateLibrary("old")} command on an older version of \R
#' to print to a text file a complete list of names for packages located under your current \R library tree(s).
#' If desired, uninstall the older version of \R.
#' On a freshly installed version of \R, with the \pkg{inlmisc} package available,
#' run the \code{RecreateLibrary("new")} command to download and install the packages listed in the package-list text file.
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
#' RecreateLibrary("old")
#'
#' \dontrun{
#' # Run on new version of R (assumes working directory has not changed)
#' RecreateLibrary("new", repos = c(CRAN = "https://cloud.r-project.org/",
#'                                  GRAN = "https://owi.usgs.gov/R"))
#' }
#'
#' unlink("packagelist.txt")
#'

RecreateLibrary <- function(desc=c("old", "new"), file="packagelist.txt", lib=NULL,
                            repos=getOption("repos")) {

  if (is.null(lib)) lib <- .libPaths()
  desc <- match.arg(desc)

  if (desc == "old") {
    pkgs <- utils::installed.packages(lib, noCache=TRUE)[, 1, drop=FALSE]
    meta <- c(sprintf("# Date modified: %s %s", Sys.time(), Sys.timezone()),
              with(R.version, sprintf("# R version: %s.%s (%s-%s-%s)", major, minor, year, month, day)),
              sprintf("# Platform: %s", R.version$platform),
              sprintf("# Running under: %s", utils::sessionInfo()$running),
              sprintf("# User: %s", Sys.info()["user"]))
    m <- rbind(matrix(meta), pkgs)
    utils::write.table(m, file, quote=FALSE, row.names=FALSE, col.names=FALSE)
    cat(sprintf("Package list written to: \"%s\"\n", normalizePath(path.expand(file))))

  } else if (desc == "new") {
    if(!file.exists(file)) stop("package-list file not found")
    type <- ifelse(Sys.info()["sysname"] == "Windows", "win.binary", "source")
    utils::update.packages(ask=FALSE, repos=repos, type=type)
    pkgs <- unique(utils::read.table(file, colClasses="character", flush=TRUE)[, 1])
    installed_pkgs <- utils::installed.packages()[, "Package"]
    pkgs <- pkgs[!pkgs %in% installed_pkgs]
    if (length(pkgs) == 0) return()
    contriburl <- utils::contrib.url(repos=repos, type=getOption("pkgType"))

    available_pkgs <- utils::available.packages(contriburl, type=type)
    is <- pkgs %in% available_pkgs
    if (length(pkgs[!is]) > 0) {
      fmt <- "\nThe following packages are missing from the repositories and can't be installed:\n    %s\n"
      warning(sprintf(fmt, paste(pkgs[!is], collapse=", ")))
    }
    pkgs <- pkgs[is]
    if (length(pkgs) == 0) return()
    utils::install.packages(pkgs, lib[1], repos=repos, type=type)
  }

  invisible(NULL)
}
