.onAttach <- function(lib, pkg) {
  if (interactive()) {
    ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
    msg <- "USGS Support Package: https://owi.usgs.gov/R/packages.html#support"
    packageStartupMessage(paste(strwrap(msg), collapse="\n"))
  }
  raster::rasterOptions(standardnames=FALSE)
  invisible()
}
