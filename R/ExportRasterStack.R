#' Export a Raster Stack
#'
#' Write a raster-stack, a collection of raster layers,
#' to local directories using multiple file formats.
#'
#' @param rs 'RasterStack' or 'RasterBrick'.
#'   Collection of \code{\linkS4class{RasterLayer}} objects with
#'   the same extent and resolution.
#' @param path 'character' string.
#'   Path name to write raster stack.
#' @param zip 'character' string.
#'   If there is no zip program on your path (on windows),
#'   you can supply the full path to a \file{zip.exe} here, in order to make a KMZ file.
#' @param col 'character' vector.
#'   Color names
#'
#' @details Five local directories are created under \code{path} and
#'   named after their intended file formats:
#'   Comma-Separated Values (\file{csv}),
#'   Portable Network Graphics (\file{png}),
#'   Georeferenced TIFF (\file{tif}),
#'   R Data (\file{rda}), and
#'   Keyhole Markup Language (\file{kml}).
#'   For its reference system, \file{kml} uses geographic coordinates:
#'   longitude and latitude components as defined by the World Geodetic System of 1984.
#'   Therefore, the conversion of gridded data between cartographic projections
#'   may introduce a new source of error.
#'
#' @return Used for the side-effect files written to disk.
#'
#' @note If the zip program is unavailable on windows, install it
#'   by downloading the latest binary version from the
#'   \href{https://www.7-zip.org/download.html}{Info-ZIP} website;
#'   select one of the given FTP locations, enter directory \file{win32},
#'   download \file{zip300xn.zip}, and extract.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link[raster]{writeRaster}}
#'
#' @keywords IO
#'
#' @importClassesFrom raster BasicRaster
#'
#' @export
#'
#' @examples
#' \dontrun{
#' rs <- raster::stack(system.file("external/rlogo.grd", package = "raster"))
#' print(rs)
#' path <- file.path(tempdir(), "rlogo")
#' dir.create(path)
#' ExportRasterStack(rs, path)
#' list.files(normalizePath(path, winslash = "/"), full.name = TRUE,
#'            recursive = TRUE, include.dirs = TRUE)
#'
#' unlink(path, recursive = TRUE)
#' }
#'

ExportRasterStack <- function(rs, path, zip="", col=NULL) {

  # check arguments
  stopifnot(inherits(rs, c("RasterStack", "RasterBrick")))
  checkmate::assertString(path)
  checkmate::assertString(zip)
  if (zip != "") checkmate::assertFileExists(zip)
  checkmate::assertCharacter(col, null.ok=TRUE)

  dir.create(path, showWarnings=FALSE, recursive=TRUE)
  dir.create(path.csv <- file.path(path, "csv"), showWarnings=FALSE)
  dir.create(path.png <- file.path(path, "png"), showWarnings=FALSE)
  dir.create(path.tif <- file.path(path, "tif"), showWarnings=FALSE)
  dir.create(path.rda <- file.path(path, "rda"), showWarnings=FALSE)
  dir.create(path.kml <- file.path(path, "kml"), showWarnings=FALSE)

  if (is.null(col)) col <- GetColors(255, stops=c(0.3, 0.9))

  n <- 0L
  for (i in names(rs)) {
    n <- n + 1L
    fig.num <- formatC(n, width=2, format="d", flag="0")

    f <- file.path(path.csv, paste(fig.num, "_", i, ".csv", sep=""))
    m <- matrix(data=rs[[i]][], nrow=nrow(rs), ncol=ncol(rs), byrow=TRUE)
    utils::write.table(m, file=f, quote=FALSE, sep=",", na="", row.names=FALSE,
                       col.names=FALSE, qmethod="double")

    f <- file.path(path.png, paste(fig.num, "_", i, ".png", sep=""))
    grDevices::png(filename=f, width=7, height=7, units="in", pointsize=12,
                   res=1200, antialias="cleartype")
    raster::plot(rs[[i]], maxpixels=length(rs[[i]]), col=col, main=names(rs[[i]]), asp=1)
    grDevices::dev.off()

    f <- file.path(path.tif, paste(fig.num, "_", i, ".tif", sep=""))
    raster::writeRaster(rs[[i]], filename=f, format="GTiff",
                        overwrite=TRUE, NAflag=-999)
  }

  base.name <- "raster"

  f <- file.path(path.rda, "rasters.rda")
  save(rs, file=f)

  f <- file.path(path.kml, "rasters.kml")
  crs <- "+init=epsg:4326"
  rs <- raster::projectRaster(rs, crs=crs, method="ngb", alignOnly=FALSE)
  raster::KML(rs, f, col=col, maxpixels=raster::ncell(rs) * 2, blur=5,
              zip=zip, overwrite=TRUE)

  invisible()
}
