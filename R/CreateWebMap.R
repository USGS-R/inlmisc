#' Create a Web Map Using The National Map Services
#'
#' This function creates a \href{http://leafletjs.com/}{Leaflet} map widget with base maps offered through
#' The National Map (\href{https://nationalmap.gov/}{TNM}).
#' Information about the content of these base maps can be found within the
#' \href{https://viewer.nationalmap.gov/help/3.0\%20TNM\%20Base\%20Maps.htm}{TNM Base Maps} document.
#'
#' @param maps 'character'.
#'   Vector of TNM base maps to include in the web map.
#'   Possible maps include \code{"Topo"}, \code{"Imagery"},
#'   \code{"Imagery Topo"}, \code{"Hydrography"}, \code{"Hill Shade"}, and \code{"Blank"}.
#'   All base maps are included by default.
#' @param ...
#'   Arguments to be passed to the \code{\link[leaflet]{leaflet}} function.
#' @param collapsed 'logical'.
#'   If true, the layers control will be rendered as an icon that expands when hovered over.
#'
#' @details A number of \href{https://viewer.nationalmap.gov/services/}{map services} are offered through TNM.
#'   There are no use restrictions on these services.
#'   However, map content is limited to the United States and Territories.
#'   This function integrates TNM services within an interactive web map using
#'   \href{https://rstudio.github.io/leaflet/}{Leaflet for R}.
#'
#' @return Returns a 'leaflet' Hypertext Markup Language (HTML) widget object with TNM base maps.
#'   See example for instructions on how to add additional graphic layers
#'   (such as points, lines, and polygons) to the map widget.
#'   Graphic layers added to the web map must be in latitude and longitude using WGS 84
#'   (also known as \href{https://epsg.io/4326}{EPSG:4326}).
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{AddWebMapElements}}
#'
#' @keywords hplot
#'
#' @export
#'
#' @examples
#' map <- CreateWebMap()
#' lng <- c(-112.049705, -122.171257, -77.367458, -149.803565, -80.248344)
#' lat <- c(43.517810, 37.456526, 38.947206, 61.187905, 26.080860)
#' map <- leaflet::addMarkers(map, lng, lat)
#' map
#'

CreateWebMap <- function(maps, ..., collapsed=TRUE) {

  checkmate::assertFlag(collapsed)

  # define base map names
  basemap <- c("Topo"         = "USGSTopo",
               "Imagery"      = "USGSImageryOnly",
               "Imagery Topo" = "USGSImageryTopo",
               "Hydrography"  = "USGSHydroCached",
               "Hill Shade"   = "USGSShadedReliefOnly",
               "Blank"        = "USGSTNMBlank")
  if (!missing(maps)) {
    checkmate::assertSubset(maps, names(basemap), empty.ok=FALSE)
    basemap <- basemap[maps]
  }

  # construct url's for base maps
  url <- sprintf("https://basemap.nationalmap.gov/ArcGIS/rest/services/%s/MapServer/tile/{z}/{y}/{x}", basemap)

  # define attribution for base maps
  att <- sprintf("<a href='%s' title='%s' target='_blank'>%s</a> | <a href='%s' title='%s' target='_blank'>%s</a>",
                 "https://www.usgs.gov/", "United States Geological Survey", "USGS",
                 "https://www.usgs.gov/laws/policies_notices.html", "USGS policies and notices", "Policies")

  # initialize map widget
  map <- leaflet::leaflet(...)

  # add base maps
  opt <- leaflet::WMSTileOptions(format="image/jpeg", version="1.3.0", maxNativeZoom=15)
  for (i in seq_along(basemap)) {
    map <- leaflet::addWMSTiles(map, url[i], group=names(basemap)[i],
                                options=opt, attribution=att, layers="0")
  }

  # add basemap control feature
  if (length(basemap) > 1) {
    opt <- leaflet::layersControlOptions(collapsed=collapsed)
    map <- leaflet::addLayersControl(map, position="topright",
                                     baseGroups=names(basemap), options=opt)
  }

  # add scale bar
  map <- leaflet::addScaleBar(map, position="bottomleft")

  # return map widget
  return(map)
}
