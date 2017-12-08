#' Add Miscellaneous Web Map Buttons
#'
#' These functions add buttons on a web map that control miscellaneous view options.
#' The \code{AddRefreshButton} function that sets the map view to the original extent.
#' And the \code{AddClusterButton} function toggles marker clusters between frozen and unfrozen states.
#'
#' @param map '\link[leaflet]{leaflet}'.
#'   Map widget object
#' @param extent 'Spatial*', 'Raster*', 'Extent', 'matrix', or 'numeric'.
#'   Extent object (or object from which an \code{\link[raster]{extent}} object can be extracted/created)
#'   representing a rectangular geographical area on the map.
#'   The extent must be specified in the coordinate reference system (CRS) of the web map,
#'   usually in latitude and longitude using WGS 84 (also known as \href{https://epsg.io/4326}{EPSG:4326}).
#'   By default, the extent object is read from the map widget.
#' @param position 'character'.
#'   Position of the button on the web map.
#'   Possible values are \code{"topleft"}, \code{"topright"}, \code{"bottomleft"}, and \code{"bottomright"}.
#' @param clusterId 'character'.
#'   Identification for the marker cluster layer.
#'
#' @return Used for the side-effect of a button placed on a web map.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{CreateWebMap}}
#'
#' @keywords hplot
#'
#' @export
#'
#' @examples
#' map <- CreateWebMap("Topo")
#' d <- maps::us.cities
#' opt <- leaflet::markerClusterOptions(showCoverageOnHover = FALSE)
#' id <- "cities_cluster"
#' map <- leaflet::addMarkers(map, lng = d$long, lat = d$lat, popup = d$name,
#'                            clusterOptions = opt, clusterId = id)
#' map <- AddRefreshButton(map)
#' map <- AddClusterButton(map, id)
#' map
#'

AddRefreshButton <- function(map, extent=NULL, position="topleft") {

  # check arguments
  checkmate::assertClass(map, c("leaflet", "htmlwidget"))
  checkmate::assertChoice(position, c("topleft", "topright", "bottomleft", "bottomright"))

  # extract/create extent object
  if (is.null(extent))
    e <- c(map$x$limits$lng, map$x$limits$lat)
  else
    e <- raster::extent(extent)

  # create button
  js <- sprintf("function(btn, map) {
                   map.fitBounds([[%f, %f],[%f, %f]]);
                 }", e[3], e[1], e[4], e[2])
  button <- leaflet::easyButton(icon="fa-refresh",
                                title="Refresh View",
                                onClick=htmlwidgets::JS(js),
                                position=position)

  # place button on map
  leaflet::addEasyButton(map, button)
}


#' @rdname AddRefreshButton
#' @export

AddClusterButton <- function(map, clusterId, position="topleft") {

  # check arguments
  checkmate::assertClass(map, c("leaflet", "htmlwidget"))
  checkmate::assertString(clusterId, min.chars=1)
  checkmate::assertChoice(position, c("topleft", "topright", "bottomleft", "bottomright"))

  # Javascript derived from https://rstudio.github.io/leaflet/morefeatures.html
  # accessed on 2017-11-06.

  # unfrozen state
  js <- sprintf("function(btn, map) {
                   var clusterManager = map.layerManager.getLayer('cluster', '%s');
                   clusterManager.freezeAtZoom();
                   btn.state('frozen-markers');
                 }", clusterId)
  s0 <- leaflet::easyButtonState(stateName="unfrozen-markers",
                                 icon="ion-ios-color-filter-outline",
                                 title="Freeze Clusters",
                                 onClick=htmlwidgets::JS(js))

  # frozen state
  js <- sprintf("function(btn, map) {
                   var clusterManager = map.layerManager.getLayer('cluster', '%s');
                   clusterManager.unfreeze();
                   btn.state('unfrozen-markers');
                 }", clusterId)
  s1 <- leaflet::easyButtonState(stateName="frozen-markers",
                                 icon="ion-ios-color-filter",
                                 title="Unfreeze Clusters",
                                 onClick=htmlwidgets::JS(js))

  # create button
  button <- leaflet::easyButton(position=position, states=list(s0, s1))

  # place button on map
  leaflet::addEasyButton(map, button)
}
