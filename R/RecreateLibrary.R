#' Recreate R Library
#'
#' These functions can be used to recreate an existing library on a new installation of \R.
#'
#' @param file 'character'.
#'   Name of the file for reading (or writing) the list of package names.
#'   For file names that do not contain an absolute path,
#'   the name is assumed relative to the current working directory (see \code{\link{getwd}()} command).
#' @param lib 'character'.
#'   The library tree(s) to search through when locating installed packages (see \code{\link{.libPaths}}),
#'   or the library directory where to install packages.
#' @param repos 'character'.
#'   Vector of base URL(s) of the repositories to use when installing packages.
#'   For example, the URL of the Geological Survey R Archive Network (GRAN) is \code{"https://owi.usgs.gov/R"}.
#'
#' @details A typical workflow is as follows:
#' Run the \code{SavePackageNames()} command on an older version of \R.
#' It will print to a text file a complete list of names for packages located under your current \R library tree(s).
#' If no longer needed, uninstall the older version of \R.
#' On a freshly installed version of \R, with the \pkg{inlmisc} package available,
#' run the \code{RecreateLibrary()} command.
#' It will download and install the packages listed in the package-names text file.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link[utils]{installed.packages}}, \code{\link[utils]{install.packages}}
#'
#' @keywords utilities
#'
#' @examples
#' # Run on old version of R
#' SavePackageNames()
#'
#' \dontrun{
#' # Run on new version of R, and ensure inlmisc package is available.
#' repos <- c(CRAN = "https://cloud.r-project.org/", GRAN = "https://owi.usgs.gov/R")
#' if (!"inlmisc" %in% rownames(utils::installed.packages()))
#'   utils::install.packages("inlmisc", repos = repos["CRAN"])
#' inlmisc::RecreateLibrary(repos = repos)
#' }
#'
#' @rdname RecreateLibrary
#' @export

RecreateLibrary <- function(file="package-names.txt", lib=NULL, repos=getOption("repos")) {

  if (is.null(lib)) lib <- .libPaths()[1]

  if (!file.exists(file)) {
    msg <- sprintf("Can't find package-names file: ", normalizePath(path.expand(file)))
    stop(msg)
  }
  type <- ifelse(Sys.info()["sysname"] == "Windows", "win.binary", "source")
  utils::update.packages(ask=FALSE, repos=repos, type=type)
  pkgs <- unique(utils::read.table(file, colClasses="character", flush=TRUE)[, 1])
  installed_pkgs <- utils::installed.packages()[, "Package"]
  pkgs <- pkgs[!pkgs %in% installed_pkgs]
  if (length(pkgs) == 0) return(invisible(NULL))
  contriburl <- utils::contrib.url(repos=repos, type=getOption("pkgType"))
  available_pkgs <- utils::available.packages(contriburl, type=type)
  is <- pkgs %in% available_pkgs
  if (length(pkgs[!is]) > 0) {
    fmt <- "\nThe following packages are missing from the repositories and can't be installed:\n    %s\n"
    warning(sprintf(fmt, paste(pkgs[!is], collapse=", ")))
  }
  pkgs <- pkgs[is]
  if (length(pkgs) == 0) return(invisible(NULL))
  utils::install.packages(pkgs, lib[1], repos=repos, type=type)

  invisible(NULL)
}

#' @rdname RecreateLibrary
#' @export

SavePackageNames <- function(file="package-names.txt", lib=NULL) {

  if (is.null(lib)) lib <- .libPaths()

  pkgs <- utils::installed.packages(lib, noCache=TRUE)[, 1]
  pkgs <- sort(unique(pkgs))
  meta <- c(sprintf("# Date modified: %s UTC", format(Sys.time(), tz="GMT")),
            sprintf("# Running under: %s", utils::sessionInfo()$running),
            with(R.version, sprintf("# R version: %s.%s (%s-%s-%s)", major, minor, year, month, day)),
            sprintf("# Platform: %s", R.version$platform),
            sprintf("# User: %s", Sys.info()["user"]))
  m <- matrix(c(meta, pkgs), ncol=1)
  utils::write.table(m, file, quote=FALSE, row.names=FALSE, col.names=FALSE)
  cat(sprintf("Package list written to: \"%s\"\n", normalizePath(path.expand(file))))

  invisible(NULL)
}
