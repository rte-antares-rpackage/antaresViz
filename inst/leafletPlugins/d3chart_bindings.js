window.LeafletWidget.methods.addD3charts = function(options, data, maxValues, colorPalette) {
  for (var i = 0; i < options.lng.length; i++) {
    var opt = {};
    for (var k in options) {
      if (options.hasOwnProperty(k)) opt[k] = options[k][i];
    }
    
    if (data) opt.data = data[i];
    if (maxValues) opt.maxValues = maxValues;
    if (colorPalette) opt.colorPalette = colorPalette;
    
    var l = L.d3chart([options.lat[i], options.lng[i]], opt);
    
    if (options.popup) l.bindPopup(options.popup[i]);
    
    var id = options.layerId ? options.layerId[i] : undefined;
    this.layerManager.addLayer(l, "d3chart", id);
  }
};

window.LeafletWidget.methods.updateD3charts = function(options, data, maxValues, colors) {
  for (var i = 0; i < options.layerId.length; i++) {
    var l = this.layerManager.getLayer("d3chart", options.layerId[i]);
    
    var opt = {};
    for (var k in options) {
      if (options.hasOwnProperty(k)) opt[k] = options[k][i];
    }
    if (data) opt.data = data[i];
    if (maxValues) opt.maxValues = maxValues;
    if (colors) opt.colors = colors;
    
    if (options.popup) l.bindPopup(options.popup[i]);
    
    l.setOptions(opt);
  }
};
