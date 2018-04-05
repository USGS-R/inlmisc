#' Add Miscellaneous Web Map Elements
#'
#' These functions can be used to augment a \href{http://leafletjs.com/}{Leaflet} web map with additional elements.
#' The \code{AddHomeButton} function adds a button that zooms to the initial map extent.
#' The \code{AddClusterButton} function adds a button that toggles marker clusters between frozen and unfrozen states.
#' The \code{AddSearchButton} function adds a search element that may be used to locate, and move to, a marker.
#' And the \code{AddCircleLegend} function adds a map legend.
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
#' @param group 'character'.
#'   Name of the group whose features will be searched.
#' @param propertyName 'character'.
#'   Property name used to describe markers, such as, \code{"label"} and \code{"popup"}.
#' @param zoom 'integer'.
#'   Zoom level for move to location after marker found in search.
#' @param textPlaceholder 'character'.
#'   Text message to show in search element.
#' @param openPopup 'logical'.
#'   Whether to open the marker popup associated with the searched for marker.
#' @param labels 'character'.
#'   Vector of text labels in the legend.
#' @param colors 'character'.
#'   Vector of (HTML) colors corresponding to \code{labels}.
#' @param radius 'numeric'.
#'   Border radius of symbols in the legend, in pixels.
#' @param opacity 'numeric'.
#'   Opacity of symbols in the legend, from 0 to 1.
#' @param symbol 'character'.
#'   Symbol type in the legend, either \code{"square"} or \code{"circle"}.
#' @param title 'character'.
#'   Legend title
#'
#' @return Used for the side-effect of a button placed on a web map.
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
#' df <- maps::us.cities
#' spdf <- sp::SpatialPointsDataFrame(df[, c("long", "lat")], data = df,
#'                                    proj4string = sp::CRS("+init=epsg:4326"))
#' opt <- leaflet::markerClusterOptions(showCoverageOnHover = FALSE)
#' map <- CreateWebMap("Topo")
#' map <- leaflet::addMarkers(map, label = ~name, popup = ~name, clusterOptions = opt,
#'                            clusterId = "cluster", group = "marker", data = spdf)
#' map <- AddHomeButton(map)
#' map <- AddClusterButton(map, clusterId = "cluster")
#' map <- AddSearchButton(map, group = "marker", zoom = 15,
#'                        textPlaceholder = "Search city names...")
#' map
#'
#' labels <- c("Non-capital", "Capital")
#' colors <- c("green", "red")
#' fillColor <- colors[(spdf@data$capital > 0) + 1L]
#' map <- CreateWebMap("Topo")
#' map <- leaflet::addCircleMarkers(map, radius = 6, color = "white", weight = 1,
#'                                  opacity = 1, fillColor = fillColor, fillOpacity = 1,
#'                                  fill = TRUE, data = spdf)
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

  # unfrozen state
  js <- sprintf("function(btn, map) {
                   var clusterManager = map.layerManager.getLayer('cluster', '%s');
                   clusterManager.freezeAtZoom();
                   btn.state('frozen-markers');
                 }", clusterId)
  s0 <- leaflet::easyButtonState(stateName="unfrozen-markers",
                                 icon="fa-circle-o",
                                 title="Freeze clusters",
                                 onClick=htmlwidgets::JS(js))

  # frozen state
  js <- sprintf("function(btn, map) {
                   var clusterManager = map.layerManager.getLayer('cluster', '%s');
                   clusterManager.unfreeze();
                   btn.state('unfrozen-markers');
                 }", clusterId)
  s1 <- leaflet::easyButtonState(stateName="frozen-markers",
                                 icon="fa-circle",
                                 title="Unfreeze clusters",
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
  checkmate::assertCharacter(group, min.chars=1, any.missing=FALSE, min.len=1)
  checkmate::assertString(propertyName, min.chars=1)
  checkmate::assertInt(zoom, lower=0, null.ok=TRUE)
  checkmate::assertString(textPlaceholder, null.ok=TRUE)
  checkmate::assertFlag(openPopup)
  checkmate::assertChoice(position, c("topleft", "topright", "bottomleft", "bottomright"))

  map$dependencies <- c(map$dependencies, .SearchDependencies())
  leaflet::invokeMethod(map,
                        data=leaflet::getMapData(map),
                        method="addSearchMarker",
                        group,
                        position,
                        propertyName,
                        zoom,
                        textPlaceholder,
                        openPopup)
}

.SearchDependencies <- function() {
  src <- system.file("htmlwidgets/plugins/leaflet-search", package="inlmisc")
  css <- if (utils::packageVersion("leaflet") < 2) "leaflet-search-old.css" else "leaflet-search.css"
  list(htmltools::htmlDependency(name="leaflet-search",
                                 version="2.4.0",
                                 src=src,
                                 script=c("leaflet-search.min.js", "leaflet-search-binding.js"),
                                 stylesheet=css))
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
  checkmate::assertString(title, null.ok=TRUE)
  checkmate::assertChoice(position, c("topleft", "topright", "bottomleft", "bottomright"))

  symbol <- match.arg(symbol)

  sizes <- rep(radius, length.out=length(colors)) * 2
  if (symbol == "square")
    fmt <- "%s; width:%fpx; height:%fpx; margin-top:4px;"
  else
    fmt <- "%s; border-radius:50%%; width:%fpx; height:%fpx; margin-top:4px;"
  col <- sprintf(fmt, colors, sizes, sizes)
  fmt <- "<div style='display:inline-block; height:%fpx; line-height:%fpx; margin-top:4px;'>%s</div>"
  lab <- sprintf(fmt, sizes, sizes, labels)
  if (!is.null(title))
    title <- sprintf("<div style='text-align:center;'>%s</div>", title)
  return(leaflet::addLegend(map, position=position, colors=col, labels=lab,
                            labFormat=as.character(), opacity=opacity, title=title))
}
