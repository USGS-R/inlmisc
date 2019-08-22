#' Add Elements to Web Map
#'
#' Augment a \href{http://leafletjs.com/}{Leaflet} web map with additional elements.
#' The \code{AddHomeButton} function adds a button that zooms to the initial map extent.
#' The \code{AddClusterButton} function adds a button that toggles marker clusters on and off.
#' The \code{AddSearchButton} function adds a control that may be used to search markers/features location by property.
#' And the \code{AddCircleLegend} function adds a map legend.
#'
#' @param map '\link[leaflet]{leaflet}'.
#'   Map widget object
#' @param extent 'Spatial*', 'Raster*', 'Extent', 'matrix', or 'numeric' vector.
#'   Extent object (or object from which an \code{\link[raster]{extent}} object can be extracted/created)
#'   representing a rectangular geographical area on the map.
#'   The extent must be specified in the coordinate reference system (CRS) of the web map,
#'   usually in latitude and longitude using WGS 84 (also known as \href{https://epsg.io/4326}{EPSG:4326}).
#'   By default, the extent object is read from the map widget.
#' @param position 'character' string.
#'   Position of the button on the web map.
#'   Possible values are \code{"topleft"}, \code{"topright"}, \code{"bottomleft"}, and \code{"bottomright"}.
#' @param clusterId 'character' string.
#'   Identification for the marker cluster layer.
#' @param group 'character' string.
#'   Name of the group whose features will be searched.
#' @param propertyName 'character' string.
#'   Property name used to describe markers, such as, \code{"label"} and \code{"popup"}.
#' @param zoom 'integer' count.
#'   Zoom level for move to location after marker found in search.
#' @param textPlaceholder 'character' string.
#'   Message to show in search element.
#' @param openPopup 'logical' flag.
#'   Whether to open the marker popup associated with the searched for marker.
#' @param labels 'character' vector.
#'   Labels in the legend.
#' @param colors 'character' vector.
#'   HTML colors corresponding to \code{labels}.
#' @param radius 'numeric' number.
#'   Border radius of symbols in the legend, in pixels.
#' @param opacity 'numeric' number.
#'   Opacity of symbols in the legend, from 0 to 1.
#' @param symbol 'character' string.
#'   Symbol type in the legend, either \code{"square"} or \code{"circle"}.
#' @param title 'character' string.
#'   Legend title
#'
#' @return Returns an object of class 'leaflet'.
#'   A new \code{map} object with added element.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{CreateWebMap}}
#'
#' @keywords hplot
#'
#' @name AddWebMapElements
#'
#' @examples
#' city <- rgdal::readOGR(system.file("extdata/city.geojson",
#'                        package = "inlmisc")[1])
#' opt <- leaflet::markerClusterOptions(showCoverageOnHover = FALSE)
#' map <- CreateWebMap("Topo")
#' map <- leaflet::addMarkers(map, label = ~name, popup = ~name,
#'                            clusterOptions = opt,
#'                            clusterId = "cluster",
#'                            group = "marker", data = city)
#' map <- AddHomeButton(map)
#' map <- AddClusterButton(map, clusterId = "cluster")
#' map <- AddSearchButton(map, group = "marker", zoom = 15,
#'                        textPlaceholder = "Search city names...")
#' map
#'
#' labels <- c("Non-capital", "Capital")
#' colors <- c("green", "red")
#' fillColor <- colors[(city@data$capital > 0) + 1]
#' map <- CreateWebMap("Topo")
#' map <- leaflet::addCircleMarkers(map, radius = 6, color = "white",
#'                                  weight = 1, opacity = 1,
#'                                  fillColor = fillColor,
#'                                  fillOpacity = 1, fill = TRUE,
#'                                  data = city)
#' map <- AddLegend(map, labels = labels, colors = colors, radius = 5,
#'                  opacity = 1, symbol = "circle")
#' map
#'

NULL

#' @rdname AddWebMapElements
#' @export

AddHomeButton <- function(map, extent=NULL, position="topleft") {

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
  button <- leaflet::easyButton(icon="fa-home fa-lg",
                                title="Zoom to initial map extent",
                                onClick=htmlwidgets::JS(js),
                                position=position)

  # place button on map
  leaflet::addEasyButton(map, button)
}


#' @rdname AddWebMapElements
#' @export

AddClusterButton <- function(map, clusterId, position="topleft") {

  # check arguments
  checkmate::assertClass(map, c("leaflet", "htmlwidget"))
  checkmate::assertString(clusterId, min.chars=1)
  checkmate::assertChoice(position, c("topleft", "topright", "bottomleft", "bottomright"))

  # Javascript derived from https://rstudio.github.io/leaflet/morefeatures.html
  # accessed on 2017-11-06.

  # disable clusters
  js <- sprintf("function(btn, map) {
                   var clusterManager = map.layerManager.getLayer('cluster', '%s');
                   clusterManager.disableClustering();
                   btn.state('disable-cluster');
                 }", clusterId)
  s0 <- leaflet::easyButtonState(stateName="enable-cluster",
                                 icon="fa-circle",
                                 title="Disable clustering",
                                 onClick=htmlwidgets::JS(js))

  # enable clusters
  js <- sprintf("function(btn, map) {
                   var clusterManager = map.layerManager.getLayer('cluster', '%s');
                   clusterManager.enableClustering();
                   btn.state('enable-cluster');
                 }", clusterId)
  s1 <- leaflet::easyButtonState(stateName="disable-cluster",
                                 icon="fa-circle-o",
                                 title="Enable clustering",
                                 onClick=htmlwidgets::JS(js))

  # create button
  button <- leaflet::easyButton(position=position, states=list(s0, s1))

  # place button on map
  leaflet::addEasyButton(map, button)
}


#' @rdname AddWebMapElements
#' @export

AddSearchButton <- function(map, group, propertyName="label", zoom=NULL,
                            textPlaceholder="Search...", openPopup=FALSE,
                            position="topleft") {

  # check arguments
  checkmate::assertClass(map, c("leaflet", "htmlwidget"))
  checkmate::assertString(group, min.chars=1)
  checkmate::assertString(propertyName, min.chars=1)
  checkmate::assertInt(zoom, lower=0, null.ok=TRUE)
  checkmate::assertString(textPlaceholder, null.ok=TRUE)
  checkmate::assertFlag(openPopup)
  checkmate::assertChoice(position, c("topleft", "topright", "bottomleft", "bottomright"))

  # check group is in map widget
  grp <- unlist(lapply(lapply(map$x$calls, function(x) x[[2]]), function(x) x[5][[1]]))
  if (!(group %in% grp)) stop("Group with name '", group, "' missing from map widget.")

  # attach html dependencies to map widget
  map$dependencies <- c(map$dependencies, .GetLeafletSearchDependencies())

  # define arguments to be passed to the javascript method
  circle <- list("radius"               = 20,
                 "weight"               = 3,
                 "opacity"              = 0.7,
                 "color"                = "#FF4040",
                 "stroke"               = TRUE,
                 "fill"                 = FALSE)
  marker <- list("icon"                 = FALSE,
                 "animate"              = TRUE,
                 "circle"               = circle)
  option <- list("propertyName"         = propertyName,
                 "zoom"                 = zoom,
                 "textPlaceholder"      = textPlaceholder,
                 "openPopup"            = openPopup,
                 "position"             = position,
                 "initial"              = FALSE,
                 "hideMarkerOnCollapse" = TRUE,
                 "marker"               = marker)

  # add leaflet-search plugin to map
  leaflet::invokeMethod(map, leaflet::getMapData(map), "addSearchControl",
                        group, leaflet::filterNULL(option))
}


.GetLeafletSearchDependencies <- function() {
  list(htmltools::htmlDependency("leaflet-search", "2.9.6", "htmlwidgets/plugins/leaflet-search",
                                 script=c("leaflet-search.min.js", "leaflet-search-binding.js"),
                                 stylesheet="leaflet-search.min.css", package="inlmisc"))
}


#' @rdname AddWebMapElements
#' @export

AddLegend <- function(map, labels, colors, radius, opacity=0.5, symbol=c("square", "circle"),
                      title="EXPLANATION", position="topright") {

  # check arguments
  checkmate::assertClass(map, c("leaflet", "htmlwidget"))
  checkmate::assertCharacter(labels, any.missing=FALSE, min.len=1)
  checkmate::assertCharacter(colors, any.missing=FALSE, len=length(labels))
  checkmate::assertNumeric(radius, lower=0, any.missing=FALSE, min.len=1)
  checkmate::assertNumber(opacity, lower=0, upper=1, finite=TRUE)
  symbol <- match.arg(symbol)
  checkmate::assertString(title, null.ok=TRUE)
  checkmate::assertChoice(position, c("topleft", "topright", "bottomleft", "bottomright"))

  sizes <- rep(radius, length.out=length(colors)) * 2
  if (symbol == "square")
    fmt <- "%s; width:%fpx; height:%fpx; margin-top:4px;"
  else
    fmt <- "%s; border-radius:50%%; width:%fpx; height:%fpx; margin-top:4px;"
  col <- sprintf(fmt, colors, sizes, sizes)
  fmt <- "<div style='display:inline-block; height:%fpx; line-height:%fpx; margin-top:4px;'>%s</div>"
  lab <- sprintf(fmt, sizes, sizes, labels)
  if (is.character(title))
    title <- sprintf("<div style='text-align:center;'>%s</div>", title)
  leaflet::addLegend(map, position=position, colors=col, labels=lab,
                     labFormat=as.character(), opacity=opacity, title=title)
}
