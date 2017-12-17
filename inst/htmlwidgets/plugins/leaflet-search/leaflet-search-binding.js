LeafletWidget.methods.addSearchMarker = function(group, position, propertyName, zoom, textPlaceholder, openPopup) {

  if(this.search) {
    this.search.removeFrom(this);
  }

  var layer_group = this.layerManager.getLayerGroup(group, true);

  var search = new L.Control.Search({
    layer: layer_group,
    propertyName: propertyName,
    zoom: zoom,
    initial: false,
    textPlaceholder: textPlaceholder,
    position: position,
    marker: false
  });
  this.addControl(search);
  this.search = search;

  this.search.on('search:locationfound', function(e) {
    if(openPopup && e.layer._popup) {
      e.layer.openPopup();
    }
  });

};
