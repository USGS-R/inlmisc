#' Add Cluster Button
#'
#' This function adds a button on a web map that toggles marker clusters
#' between frozen and unfrozen states.
#'
#' @param map '\link[leaflet]{leaflet}'.
#'   Map widget object
#' @param clusterId 'character'.
#'   Identification for the marker cluster layer.
#'
#' @return Used for the side-effect of a button placed on the web map.
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
#' map <- CreateWebMap()
#' d <- maps::us.cities
#' opt <- leaflet::markerClusterOptions(showCoverageOnHover = FALSE)
#' id <- "cities_cluster"
#' map <- leaflet::addMarkers(map, lng = d$long, lat = d$lat, popup = d$name,
#'                            clusterOptions = opt, clusterId = id)
#' map <- AddClusterButton(map, id)
#' map
#'

AddClusterButton <- function(map, clusterId) {

  # Javascript derived from https://rstudio.github.io/leaflet/morefeatures.html,
  # accessed on 2017-11-06.

  # unfrozen state
  js <- sprintf("function(btn, map) {
                   var clusterManager = map.layerManager.getLayer('cluster', '%s');
                   clusterManager.freezeAtZoom();
                   btn.state('frozen-markers');
                 }", clusterId)
  s0 <- leaflet::easyButtonState(stateName="unfrozen-markers",
                                 icon="ion-toggle",
                                 title="Freeze Clusters",
                                 onClick=htmlwidgets::JS(js))

  # frozen state
  js <- sprintf("function(btn, map) {
                   var clusterManager = map.layerManager.getLayer('cluster', '%s');
                   clusterManager.unfreeze();
                   btn.state('unfrozen-markers');
                 }", clusterId)
  s1 <- leaflet::easyButtonState(stateName="frozen-markers",
                                 icon="ion-toggle-filled",
                                 title="Unfreeze Clusters",
                                 onClick=htmlwidgets::JS(js))

  # create button
  button <- leaflet::easyButton(states=list(s0, s1))

  # place button on map
  leaflet::addEasyButton(map, button)
}
