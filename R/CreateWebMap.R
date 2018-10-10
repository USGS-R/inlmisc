#' Create a Web Map Using TNM Services
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
#' @param service 'logical'.
#'   Mapping services for accessing TNM base-map tiles.
#'   Select \code{"rest"} for representational state transfer services (the default) and
#'   \code{"wms"} for web map services.
#'
#' @details Map \href{https://viewer.nationalmap.gov/services/}{service endpoints}
#'   are offered through TNM with no use restrictions.
#'   However, map content is limited to the United States and territories.
#'   This function integrates TNM endpoint services within an interactive web map using
#'   \href{https://rstudio.github.io/leaflet/}{Leaflet for R}.
#'
#' @return Returns a 'leaflet' hypertext markup language (HTML) widget object with TNM base maps.
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

CreateWebMap <- function(maps, ..., collapsed=TRUE, service=c("rest", "wms")) {

  # check arguments
  basemap <- c("Topo"         = "USGSTopo",
               "Imagery"      = "USGSImageryOnly",
               "Imagery Topo" = "USGSImageryTopo",
               "Hydrography"  = "USGSHydroCached",
               "Hill Shade"   = "USGSShadedReliefOnly",
               "Blank"        = "USGSTNMBlank")
  if (!missing(maps))
    basemap <- basemap[match.arg(maps, names(basemap), several.ok=TRUE)]
  checkmate::assertFlag(collapsed)
  service <- match.arg(service)

  # define attribution
  att <- sprintf("<a href='%s' title='%s' target='_blank'>%s</a> | <a href='%s' title='%s' target='_blank'>%s</a>",
                 "https://www.usgs.gov/", "United States Geological Survey", "USGS",
                 "https://www.usgs.gov/laws/policies_notices.html", "USGS policies and notices", "Policies")

  # initialize map widget
  map <- leaflet::leaflet(...)

  # add base maps
  domain <- "https://basemap.nationalmap.gov"
  if (service == "rest") {
    url <- sprintf("%s/ArcGIS/rest/services/%s/MapServer/tile/{z}/{y}/{x}", domain, basemap)
    opt <- leaflet::tileOptions(minZoom=3, maxZoom=16)
    for (i in seq_along(basemap)) {
      map <- leaflet::addTiles(map, urlTemplate=url[i], attribution=att,
                               group=names(basemap)[i], options=opt)
    }
  } else {
    url <- sprintf("%s/arcgis/services/%s/MapServer/WmsServer?", domain, basemap)
    opt <- leaflet::WMSTileOptions(format="image/jpeg", version="1.3.0", minZoom=3, maxZoom=16)
    for (i in seq_along(basemap)) {
      map <- leaflet::addWMSTiles(map, url[i], group=names(basemap)[i],
                                  options=opt, attribution=att, layers="0")
    }
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
  map
}
