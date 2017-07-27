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
#' @param snapshot 'logical', 'character', or 'Date'.
#'   Calendar date for the Comprehensive R Archive Network (CRAN) snapshot in time,
#'   see the Microsoft R Application Network
#'   (\href{https://mran.microsoft.com/timemachine/}{MRAN}) website for details.
#'   If true, the snapshot date is read from the first line of \code{file}.
#'   A snapshot date can also be specified directly using the required date format, \code{"\%Y-\%m-\%d"}.
#'   This argument masks all CRAN mirrors in \code{repos}.
#' @param pkg 'character'.
#'   The name of an installed package.
#'   Only packages that \code{pkg} depend on/link to/import/suggest are included in the package-names file;
#'   \code{pkg} is also included in this list.
#'
#' @details A typical workflow is as follows:
#' Run the \code{SavePackageNames()} command on an older version of \R.
#' It will print to a text file a complete list of names for packages located under your current \R library tree(s).
#' If no longer needed, uninstall the older version of \R.
#' On a freshly installed version of \R, with the \pkg{inlmisc} package available,
#' run the \code{RecreateLibrary()} command.
#' It will download and install the packages listed in the package-names text file.
#'
#' To achieve reproducibility, daily snapshots of CRAN are stored on MRAN
#' and available as far back as September 17, 2014.
#' Use the \code{snapshot} argument to install packages from a daily snapshot.
#' Note that newer versions of \R may not be compatible with older versions of packages.
#' To avoid any installation issues with packages,
#' install the version \R that was available from CRAN on the
#' \href{https://mran.microsoft.com/snapshot/}{snapshot date}.
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

RecreateLibrary <- function(file="pkg-names.txt", lib=NULL,
                            repos=getOption("repos"), snapshot=FALSE) {

  if (is.null(lib)) lib <- .libPaths()[1]

  is <- substr(repos, nchar(repos), nchar(repos)) != "/"
  repos[is] <- paste0(repos[is], "/")
  repos <- repos[!duplicated(repos)]

  if (!file.exists(file)) {
    msg <- paste("Can't find package-names file:", normalizePath(path.expand(file)))
    stop(msg)
  }

  meta <- readLines(file, n=2)

  if (any(is <- grep("# Date modified: ", meta))) {
    fmt <- "# Date modified: %Y-%m-%d %H:%M:%S"
    date_modified <- as.Date(strptime(meta[is], fmt, tz="GMT"))
  }

  if (any(is <- grep("R version: ", meta))) {
    r_ver_new <- strsplit(meta[is], ": ")[[1]][2]
    r_ver_old <- sub("R version ", "", R.version$version.string)

    if (!identical(r_ver_old, r_ver_new)) {
      fmt <- paste("The R version running [%s] is different from the R version read from the file.",
                   "If compatiblity is an issue, consider installing R version %s.")
      msg <- sprintf(fmt, r_ver_new, r_ver_old)
      message(paste(strwrap(msg), collapse="\n"))
      ans <- readline("Would you like to continue (y/n)? ")
      if (tolower(substr(ans, 1, 1)) == "n") return(invisible(NULL))
    }
  }

  if (is.character(snapshot)) {
    snapshot <- as.Date(snapshot, tz="GMT")
  } else if (is.logical(snapshot) && snapshot) {
    snapshot <- date_modified
  } else {
    snapshot <- NULL
  }

  if (inherits(snapshot, "Date")) {
    if (is.na(snapshot))
      stop("Problem with snapshot date format.")
    if (snapshot < as.Date("2014-09-17"))
      stop("Daily CRAN snapshots only go back as far as September 17, 2014.")
    cran_mirrors <- utils::getCRANmirrors(all=TRUE)$URL
    repos <- repos[!repos %in% cran_mirrors]
    url <- sprintf("https://mran.revolutionanalytics.com/snapshot/%s/", snapshot)
    repos <- c(repos, MRAN=url)
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

SavePackageNames <- function(file="pkg-names.txt", lib=NULL, pkg=NULL) {

  if (is.null(lib)) lib <- .libPaths()

  pkgs <- utils::installed.packages(lib, noCache=TRUE)[, 1]
  if (!is.null(pkg) && pkg %in% pkgs) {
    desc <- utils::packageDescription(pkg, lib)
    x <- gsub("\\n", " ", with(desc, paste(Depends, Imports, Suggests, sep=", ")))
    x <- strsplit(x, ", ")[[1]]
    pkgs <- c(x[x %in% pkgs], pkg)
  }

  pkgs <- pkgs[!duplicated(pkgs)]
  meta <- c(sprintf("# Date modified: %s UTC", format(Sys.time(), tz="GMT")),
            with(R.version, sprintf("# R version: %s.%s (%s-%s-%s)", major, minor, year, month, day)),
            sprintf("# Running under: %s", utils::sessionInfo()$running),
            sprintf("# Platform: %s", R.version$platform),
            sprintf("# User: %s", Sys.info()["user"]))
  m <- matrix(c(meta, pkgs), ncol=1)
  utils::write.table(m, file, quote=FALSE, row.names=FALSE, col.names=FALSE)

  cat(sprintf("Package names written to: \"%s\"\n", normalizePath(path.expand(file))))

  invisible(NULL)
}
