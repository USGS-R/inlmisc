#' Idaho National Laboratory Facilities
#'
#' Federal research facilities at the Idaho National Laboratory (\href{https://www.inl.gov/}{INL}).
#'
#' @format A 'SpatialPolygonsDataFrame' object with 7 features and 1 variable.
#'   See \code{\link{projargs}} dataset for coordinate reference system.
#'
#' @source U.S. Geological Survey INL Project Office: \url{https://id.water.usgs.gov/INL/}
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(facilities, col = "black")
#' raster::text(facilities, facilities@data$NAME, cex = 0.6, pos = 1)
#' str(facilities@data)
#'
#' # To build the dataset, run the following commands:
#'
#' \dontrun{
#' f <- system.file("extdata", "facilities.zip", package = "inlmisc")
#' f <- utils::unzip(f, exdir = tempdir())
#' f <- grep(".shp$", f, value = TRUE)
#' layer <- tools::file_path_sans_ext(basename(f))
#' facilities <- rgdal::readOGR(dirname(f), layer, stringsAsFactors = FALSE)
#' facilities <- sp::spTransform(facilities, projargs)
#' save(facilities, file = "facilities.rda", compress = "xz")
#' }
#'
"facilities"

#' Idaho National Laboratory Boundary
#'
#' The political boundary of the Idaho National Laboratory (\href{https://www.inl.gov/}{INL}).
#'
#' @format A 'SpatialPolygonsDataFrame' object with 1 feature and 4 variables.
#'   See \code{\link{projargs}} dataset for coordinate reference system.
#'
#' @source U.S. Geological Survey INL Project Office: \url{https://id.water.usgs.gov/INL/}
#'
#' @keywords datasets
#'
#' @examples
#' sp::plot(inl)
#' str(inl@data)
#'
#' # To build the dataset, run the following commands:
#'
#' \dontrun{
#' f <- system.file("extdata", "inl.zip", package = "inlmisc")
#' f <- utils::unzip(f, exdir = tempdir())
#' f <- grep(".shp$", f, value = TRUE)
#' inl <- rgdal::readOGR(dirname(f), tools::file_path_sans_ext(basename(f)))
#' inl <- sp::spTransform(inl, projargs)
#' save(inl, file = "inl.rda", compress = "xz")
#' }
#'
"inl"

#' Projection Arguments
#'
#' Projection arguments that define a custom coordinate reference system (CRS) used by the
#' U.S. Geological Survey Idaho National Laboratory Project Office.
#' The CRS consists of all the following components:
#' Albers Equal-Area Conic projection;
#' latitude of first and second standard parallels is 42.83 and 44.16, respectively;
#' latitude of origin is 41.5;
#' central meridian is -113;
#' false easting and northing is 200,000 and 0 meters, respectively;
#' Clarke (1966) reference ellipsoid;
#' North American Datum of 1927; and
#' units of meters.
#'
#' @format A 'character' string with projection arguments in PROJ.4 format.
#'
#' @keywords datasets
#' @examples
#' crs <- sp::CRS(projargs)
#' print(crs)
#'
#' # To build the dataset, run the following commands:
#'
#' \dontrun{
#' projargs <- paste("+proj=aea",
#'                   "+lat_1=42.83333333333333 +lat_2=44.16666666666666",
#'                   "+lat_0=41.5 +lon_0=-113",
#'                   "+x_0=200000 +y_0=0",
#'                   "+ellps=clrk66",
#'                   "+datum=NAD27",
#'                   "+units=m",
#'                   "+no_defs")
#' save(projargs, file = "projargs.rda", compress = "xz")
#' }
#'
"projargs"
