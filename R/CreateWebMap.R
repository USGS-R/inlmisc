#' Create a Web Map Using The National Map Services
#'
#' This function creates a \href{http://leafletjs.com/}{Leaflet} map widget with base maps offered through
#' The National Map (\href{https://nationalmap.gov/}{TNM}).
#' Information about the content of these base maps can be found within the
#' \href{https://viewer.nationalmap.gov/help/3.0\%20TNM\%20Base\%20Maps.htm}{TNM Base Maps} document.
#'
#' @param ...
#'   Leaflet options to be passed to the \code{\link[leaflet]{leafletOptions}} function.
#'
#' @details A number of \href{https://viewer.nationalmap.gov/services/}{map services} are offered through TNM.
#'   There are no use restrictions on these services.
#'   However, map content is limited to the United States and Territories.
#'   This function integrates TNM services within an interactive web map using
#'   \href{https://rstudio.github.io/leaflet/}{Leaflet for R}.
#'
#' @return Returns a 'leaflet' Hypertext Markup Language (HTML) widget object with TNM base maps.
#'   See example for instructions on how to add additional graphic layers to the map widget.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link[leaflet]{addWMSTiles}}
#'
#' @keywords hplot
#'
#' @export
#'
#' @examples
#' map <- CreateWebMap()
#'
#' lng <- c(-112.049705, -122.171257, -77.367458, -149.803565, -80.248344)
#' lat <- c(43.517810, 37.456526, 38.947206, 61.187905, 26.080860)
#' pop <- c("ID", "CA", "VA", "AK", "FL")
#' map <- leaflet::addMarkers(map, lng, lat, popup = pop)
#'
#' map
#'

CreateWebMap <- function(...) {

  # initialize map widget
  map <- leaflet::leaflet(options=leaflet::leafletOptions(...))

  # specify attribution
  att <- paste("<a href='https://www.usgs.gov/'>U.S. Geological Survey</a> |",
               "<a href='https://www.usgs.gov/laws/policies_notices.html'>Policies</a>")

  # add tiled basemaps
  basemap <- c("USGS Topo", "USGS Imagery Only", "USGS Imagery Topo",
               "USGS Shaded Relief Only")
  url <- .GetURL(basemap)
  opt <- leaflet::WMSTileOptions(version="1.3.0")
  for (i in seq_along(basemap)) {
    map <- leaflet::addWMSTiles(map, url[i], group=basemap[i], attribution=att,
                                options=opt, layers="0")
  }

  # add tiled overlay
  overlay <- c("USGS Hydro Cached")
  url <- .GetURL(overlay)
  opt <- leaflet::WMSTileOptions(version="1.3.0", format="image/png", transparent=TRUE)
  for (i in seq_along(overlay)) {
    map <- leaflet::addWMSTiles(map, url[i], group=overlay[i], options=opt, layers="0")
    map <- leaflet::hideGroup(map, overlay[i])
  }

  # add control feature
  opt <- leaflet::layersControlOptions(collapsed=FALSE)
  map <- leaflet::addLayersControl(map, baseGroups=basemap,
                                   overlayGroups=overlay, options=opt)

  return(map)
}


.GetURL <- function(x, host="basemap.nationalmap.gov") {
  service <- gsub("[[:space:]]", "", x)
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}
