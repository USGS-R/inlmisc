#' Recreate R Library
#'
#' These functions can be used to recreate an existing library on a new installation of \R.
#' The \code{SavePackageDetails} function writes the details of installed packages to a text file.
#' And the \code{RecreateLibrary} function reads this file and downloads and installs any
#' \sQuote{missing} packages from the Comprehensive R Archive Network (CRAN),
#' CRAN-like repositories, local package-installation files, and GitHub.
#'
#' @param file 'character'.
#'   Name of the file for reading (or writing) the list of package details.
#'   For a file name that does not contain an absolute path,
#'   the name is assumed relative to the current working directory (see \code{\link{getwd}} function).
#'   A \file{.gz} file extension indicates the file is compressed by \emph{gzip}.
#' @param lib 'character'.
#'   The library tree(s) to search through when locating installed packages (see \code{\link{.libPaths}} function),
#'   or the library directory where to install packages.
#' @param repos 'character'.
#'   Vector of base URL(s) of the CRAN-like repositories (includes CRAN) to use when installing packages.
#'   For example, the URL of the RStudio sponsored CRAN mirror is \code{"https://cloud.r-project.org/"}.
#'   And the URL of the Geological Survey R Archive Network (\href{https://owi.usgs.gov/R/gran.html}{GRAN})
#'   is \code{"https://owi.usgs.gov/R"}.
#' @param snapshot 'logical', 'Date', or 'character'.
#'   Calendar date for a CRAN snapshot in time,
#'   see the Microsoft R Application Network
#'   (\href{https://mran.microsoft.com/}{MRAN}) website for details.
#'   If true, the snapshot date is read from the first line of the package-details \code{file}.
#'   A snapshot date can also be specified directly using the required date format, YYYY-MM-DD.
#'   This argument masks any CRAN mirror specified in \code{repos}.
#' @param local 'character'.
#'   Vector of paths to local repositories.
#'   Packages are installed from local files in these directories.
#'   Files can contain \emph{binary} builds of packages (\file{.zip} on Windows and \file{.tgz} on macOS)
#'   or be \emph{source} packages (\file{.tar.gz}).
#' @param versions 'logical'.
#'   If true, installed package versions will be identical to version numbers stored in the package-details \code{file}.
#'   Only applies to packages from CRAN-like repositories and local package-installation files.
#'   Requires that the \pkg{devtools} package is available,
#'   see \code{\link[devtools]{install_version}} function.
#' @param github 'logical'.
#'   If true, an attempt is made to install a subset of packages from \href{https://github.com/}{GitHub}.
#'   Only applies to packages missing from the CRAN-like repositories (see \code{repos} argument).
#'   Requires that the \pkg{githubinstall} package is available,
#'   see \code{\link[githubinstall]{gh_install_packages}} function.
#'   Note that locating \R packages hosted on GitHub using nothing but the package name can be difficult.
#'   The user will be prompted with suggested repository names to identify the correct package to install.
#'   Package vignettes are not built using this option.
#'   An example of an \R package that is only available on GitHub is \pkg{AnomalyDetection},
#'   located at \href{https://github.com/twitter/AnomalyDetection}{twitter/AnomalyDetection}.
#' @param quiet 'logical'.
#'   If true, reduce the amount of output.
#' @param pkg 'character'.
#'   One or more names of packages located under \code{lib}.
#'   Only packages specified in \code{pkg}, and the packages that \code{pkg} depend on/link to/import/suggest,
#'   are included in the package-details \code{file}.
#'
#' @details A typical workflow is as follows:
#'   Run the \code{SavePackageDetails()} command on an older version of \R.
#'   It will print to a text file a complete list of details for packages located under your current \R library tree(s).
#'   If the older version of \R no longer needed, uninstall it.
#'   Then, on a freshly installed version of \R with the \pkg{inlmisc} package available,
#'   run the \code{RecreateLibrary()} command.
#'   It will download and install the packages listed in the package-details \code{file}.
#'
#'   The type of package to download and install from CRAN-like repositories is
#'   \emph{binary} on Windows and some macOS builds, and \emph{source} on all others.
#'   Package installation from GitHub or a local \file{.tar.gz} file is always a source installation.
#'   If a package is installed from source, and it contains code that needs compiling,
#'   you must have a working development environment.
#'   On Windows, install the \href{https://cran.r-project.org/bin/windows/Rtools/}{Rtools} collection
#'   and have the PATH environment variable set up as required by Rtools.
#'   On macOS, install Xcode from the Mac App Store.
#'   And on Linux, install a compiler and various development libraries.
#'
#'   Daily snapshots of CRAN are stored on MRAN and available as far back as September 17, 2014.
#'   Use the \code{snapshot} argument to install older package versions from MRAN.
#'   Note that newer versions of \R may not be compatible with older versions of packages.
#'   To avoid any package installation issues,
#'   install the \R version that was available from CRAN on the
#'   \href{https://mran.microsoft.com/snapshot/}{snapshot date}.
#'
#'   The package-details \code{file} is of the following format:
#'
#'   # Date modified: YYYY-MM-DD HH:MM:SS UTC \cr
#'   # R version 9.9.9 (YYYY-MM-DD)
#'   \tabular{ll}{
#'   Package \tab Version \cr
#'   name \tab 9.9.9 \cr
#'   ... \tab ...
#'   }
#'
#'   The format is flexible enough to add additional extraneous metadata and table fields.
#'   For example,
#'
#'   # Date modified: 2017-08-12 05:14:33 UTC \cr
#'   # R version 3.4.1 (2017-06-30) \cr
#'   # Running under: Windows 10 x64 (build 14393) \cr
#'   # Platform: x86_64-w64-mingw32
#'   \tabular{lllll}{
#'   Package \tab Version \tab Priority \tab Depends \tab Imports \cr
#'   akima \tab 0.6-2 \tab NA \tab R (>= 2.0.0) \tab sp \cr
#'   animation \tab 2.5 \tab NA \tab R (>= 2.14.0) \tab NA
#'   }
#'
#' @return The \code{SavePackageDetails} function returns (invisibly) the MD5 checksum of the package-details \code{file} content.
#'   Any changes in the file content will produce a different MD5 checksum.
#'   Use the \code{\link[tools]{md5sum}} function to verify that a file has not been tampered with.
#'   The \code{RecreateLibrary} function returns (invisibly) \code{NULL}.
#'
#' @note This package-installation method does not offer one-hundred percent reproducibility of existing \R libraries.
#'   Alternative methods, that offer better reproducibility, are available using the
#'   \pkg{checkpoint} and \pkg{packrat} packages;
#'   both of which provide robust tools for dependency management in \R.
#'
#'   If affiliated with the U.S. Department of Interior (DOI), you may receive the following error message:
#'   "SSL certificate problem: unable to get local issuer certificate".
#'   The error results from a missing X.509 certificate that permits the DOI to scan encrypted data for security reasons.
#'   A fix for this error is given in the \code{\link{AddCertificate}} function documentation.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link[utils]{installed.packages}}, \code{\link[utils]{install.packages}}
#'
#' @keywords utilities
#'
#' @examples
#' # Run on old version of R
#' SavePackageDetails()
#'
#' \dontrun{
#' # Run on new version of R, and ensure 'inlmisc' package is available.
#' repos <- c(CRAN = "https://cloud.r-project.org/", GRAN = "https://owi.usgs.gov/R")
#' if (system.file(package = "inlmisc") == "")
#'   utils::install.packages("inlmisc", repos = repos["CRAN"], dependencies = TRUE)
#' inlmisc::RecreateLibrary(repos = repos, github = TRUE)
#' }
#'
#' # Clean up example
#' unlink("R-packages.tsv")
#'
#' @rdname RecreateLibrary
#' @export

RecreateLibrary <- function(file="R-packages.tsv", lib=.libPaths()[1],
                            repos=getOption("repos"), snapshot=FALSE,
                            local=NULL, versions=FALSE, github=FALSE,
                            quiet=FALSE) {

  # set environment variable for certificates path
  if (.Platform$OS.type == "windows" && Sys.getenv("CURL_CA_BUNDLE") == "") {
    bundle <- system.file("cacert.pem", package="openssl")
    if (file.exists(bundle)) Sys.setenv(CURL_CA_BUNDLE=bundle)
  }

  # confirm file exists
  if (!file.exists(file)) {
    msg <- sprintf("Can't find package-list file:\n %s",
                   normalizePath(path.expand(file)))
    stop(msg, call.=FALSE)
  }

  # read meta data
  meta <- readLines(file)
  meta <- meta[substr(meta, 1, 2) == "# "]
  meta <- sub("# ", "", meta)

  # save modification date
  if (any(is <- grep("Date modified: ", meta))) {
    fmt <- "Date modified: %Y-%m-%d %H:%M:%S"
    date_modified <- as.Date(strptime(meta[is], fmt, tz="GMT"))
  }

  # save r version
  if (any(is <- grep("R version ", meta))) {
    r_ver_new <- meta[is]
    r_ver_old <- R.version$version.string
    if (!identical(r_ver_old, r_ver_new)) {
      fmt <- paste("Your %s is different from the R version read from the file.",
                   "If compatiblity is an issue, consider installing %s.")
      msg <- sprintf(fmt, r_ver_new, r_ver_old)
      message(paste(strwrap(msg), collapse="\n"))
      ans <- readline("Would you like to continue (y/n)? ")
      if (tolower(substr(ans, 1, 1)) == "n") return(invisible(NULL))
    }
  }

  # tidy url's for package repositories
  is <- substr(repos, nchar(repos), nchar(repos)) != "/"
  repos[is] <- paste0(repos[is], "/")
  repos <- repos[!duplicated(repos)]

  # configure repositories if snapshot date is specified
  if (is.character(snapshot)) {
    snapshot <- as.Date(snapshot, tz="GMT")
  } else if (is.logical(snapshot) && snapshot) {
    snapshot <- date_modified
  } else {
    snapshot <- NULL
  }
  if (inherits(snapshot, "Date")) {
    if (is.na(snapshot))
      stop("Problem with snapshot date format.", call.=FALSE)
    if (snapshot < as.Date("2014-09-17")) {
      msg <- "Daily CRAN snapshots only go back as far as September 17, 2014."
      stop(msg, call.=FALSE)
    }
    repos <- repos[!repos %in% utils::getCRANmirrors(all=TRUE)$URL]
    url <- sprintf("https://mran.revolutionanalytics.com/snapshot/%s/", snapshot)
    repos <- c(repos, MRAN=url)
  }

  # set the type of package to download and install
  if (.Platform$OS.type == "windows")
    type <- "win.binary"
  else
    type <- ifelse(Sys.info()["sysname"] == "Darwin",
                              "mac.binary.el-capitan", "source")

  # update packages
  if (!versions) utils::update.packages(repos=repos, ask=FALSE, type=type)

  # read package list
  pkgs <- utils::read.table(file, header=TRUE, sep="\t", colClasses="character",
                            stringsAsFactors=FALSE)

  # filter out packages that are already installed
  pkgs <- pkgs[!.IsPackageInstalled(pkgs$Package, lib), , drop=FALSE]
  if (nrow(pkgs) == 0) return(invisible(NULL))

  # install packages from local files
  if (!is.null(local)) {
    if (!all(is <- (file.info(local)$isdir %in% TRUE))) {
      msg <- sprintf("The following local directories do not exist:\n %s",
                     paste(local[!is], collapse="\n "))
      stop(msg, call.=FALSE)
    }
    ext <- "tar.gz"
    if (.Platform$OS.type == "windows") {
      ext <- c(ext, "zip")
    } else if (Sys.info()["sysname"] == "Darwin") {
      ext <- c(ext, "tgz")
    }
    path <- normalizePath(list.files(local, full.names=TRUE), winslash="/")
    path <- path[grepl(paste(sprintf("\\.%s$", ext), collapse="|"), path)]
    path <- path[grepl("_[0-9]", basename(path))]
    name <- basename(tools::file_path_sans_ext(path, compression=TRUE))
    if (versions) {
      path <- path[name %in% sprintf("%s_%s", pkgs$Package, pkgs$Version)]
      path <- path[order(basename(path), decreasing=TRUE)]
      nam <- unlist(lapply(strsplit(basename(path), "_"), function(x) x[1]))
      path <- path[!duplicated(nam)]
    } else {
      txt <- strsplit(name, "_")
      nam <- unlist(lapply(txt, function(x) x[1]))
      ver <- unlist(lapply(txt, function(x) x[2]))
      ext <- tools::file_ext(path)
      path <- path[order(nam, ver, ext, decreasing=TRUE)]
      nam <- unlist(lapply(strsplit(basename(path), "_"), function(x) x[1]))
      path <- path[!duplicated(nam) & nam %in% pkgs$Package]
    }
    if (length(path) > 0) {
      utils::install.packages(path, lib[1], repos=NULL, quiet=quiet)

      # filter out packages that were installed from local files
      pkgs <- pkgs[!.IsPackageInstalled(pkgs$Package, lib), , drop=FALSE]
      if (nrow(pkgs) == 0) return(invisible(NULL))
    }
  }

  # identify packages that are available on repositories
  contriburl <- utils::contrib.url(repos=repos, type=getOption("pkgType"))
  available_pkgs <- utils::available.packages(contriburl, type=type)
  is_on_repos <- pkgs$Package %in% available_pkgs

  # install packages from cran-like repositories
  if (any(is_on_repos)) {
    if (versions && requireNamespace("devtools", quietly=TRUE)) {
      for (i in which(is_on_repos)) {
        if (.IsPackageInstalled(pkgs$Package[i], lib)) next
        ans <- try(devtools::install_version(pkgs$Package[i], pkgs$Version[i],
                                             type=type, quiet=quiet), silent=TRUE)
        if (inherits(ans, "try-error")) {
          is_on_repos[i] <- FALSE
          next
        }
      }
    } else {
      utils::install.packages(pkgs$Package[is_on_repos], lib[1], repos=repos,
                              type=type, quiet=quiet)
    }
  }

  # install packages from github
  if (any(!is_on_repos) && github && requireNamespace("githubinstall", quietly=TRUE))
    githubinstall::gh_install_packages(pkgs$Package[!is_on_repos],
                                       quiet=quiet, lib=lib[1])

  # warn about packages that could not be installed
  if (any(is <- !.IsPackageInstalled(pkgs$Package, lib))) {
    msg <- sprintf("The following packages could not be installed:\n %s\n",
                   paste(pkgs$Package[is], collapse=", "))
    warning(msg, call.=FALSE)
  }

  invisible(NULL)
}

#' @rdname RecreateLibrary
#' @export

SavePackageDetails <- function(file="R-packages.tsv", lib=.libPaths(), pkg=NULL) {

  # get names of all packages under library tree(s)
  pkgs <- utils::installed.packages(lib, noCache=TRUE)

  # remove newlines from table elements
  pkgs <- apply(pkgs, 2, function(i) gsub("[\r\n]", "", i))

  # remove duplicate packages
  pkgs <- pkgs[!duplicated(pkgs[, "Package"]), ]

  # subset packages based on specified package(s)
  if (!is.null(pkg)) {
    if(any(is <- !pkg %in% pkgs[, "Package"])) {
      msg <- sprintf("Missing 'pkg' values in library: %s",
                     paste(pkg[is], collapse=", "))
      stop(msg, call.=FALSE)
    }
    FUN <- function(i) {
      x <- utils::packageDescription(i, lib)
      x <- c(x$Depends, x$LinkingTo, x$Imports, x$Suggests)
      if (is.null(x)) return(NULL)
      x <- paste(x, collapse=", ")
      x <- gsub("\\n", " ", x)
      x <- gsub("\\s*\\([^\\)]+\\)", "", x)
      x <- strsplit(x, ", ")[[1]]
      return(x)
    }
    p <- c(unlist(lapply(pkg, FUN)), pkg)
    pkgs <- pkgs[pkgs[, "Package"] %in% p, , drop=FALSE]
  }

  # prompt before overwriting file
  if (file.exists(file)) {
    ans <- readline("File already exists. Do you want to overwrite it (y/n)? ")
    if (tolower(substr(ans, 1, 1)) == "n") return(invisible(NULL))
  }

  # open connection to file
  if (grepl("^.*(.gz)[[:space:]]*$", file))
    con <- gzfile(file, "w", compression=9)
  else
    con <- base::file(file, "w")
  on.exit(close(con))

  # write meta data
  meta <- c(sprintf("# Date modified: %s UTC", format(Sys.time(), tz="GMT")),
            sprintf("# %s", R.version$version.string),
            sprintf("# Running under: %s", utils::sessionInfo()$running),
            sprintf("# Platform: %s", R.version$platform))
  writeLines(meta, con)

  # write package details to file
  pkgs <- as.data.frame(pkgs, stringsAsFactors=FALSE)
  suppressWarnings(utils::write.table(pkgs, con, append=TRUE, quote=FALSE,
                                      sep="\t", row.names=FALSE))

  # write file path and md5 checksum to console
  msg <- sprintf("File path:\n %s", normalizePath(path.expand(file)))
  message(msg)
  md5 <- tools::md5sum(file)
  msg <- sprintf("MD5 checksum:\n %s", md5)
  message(msg)

  # return md5 checksum
  invisible(md5)
}

#' Check whether Package is Installed
#'
#' @param x 'character'.
#'   Vector of package names
#' @param lib 'character'.
#'   Vector of library tree(s)
#'
#' @return A 'logical' vector the length of \code{x}.
#'

.IsPackageInstalled <- function(x, lib) {
  FUN <- function(i) {system.file(package=i, lib.loc=lib) != ""}
  return(vapply(x, FUN, TRUE))
}
