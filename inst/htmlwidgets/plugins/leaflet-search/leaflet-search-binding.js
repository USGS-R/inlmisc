LeafletWidget.methods.addSearchMarker = function(group, position, propertyName, zoom) {

  if(this.search) {
    this.search.removeFrom(this);
  }

  var layer_group = this.layerManager.getLayerGroup(group, true);

  var search = new L.Control.Search({
    layer: layer_group,
    position: position,
    propertyName: propertyName,
    initial: false,
    zoom: zoom,
    marker: false
  });
  this.addControl(search);
  this.search = search;

  this.search.on('search:locationfound', function(e) {
    if(e.layer._popup) {
      e.layer.openPopup();
    }
  });
};
