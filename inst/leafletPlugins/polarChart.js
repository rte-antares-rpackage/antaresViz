/*
PolarChart

Leaflet class that adds a polar bar chart.

Creation:
  L.polarChart(<LatLng> center, <Number> radius, <Number[]> data,
               <PolarChart options> options)

Options:
- opacity: opacity of the chart
- colors: array of colors

Methods:
  setStyle(<DirectedSegment options> options): update the style of the directed 
    segment
*/

L.PolarChart = L.CircleMarker.extend({
  options: {
    opacity: 1,
    colors: d3.schemeCategory10,
    maxValue: 0
  },
  
  initialize: function(center, radius, data, options) {
    this._center = center;
    this._radius = radius;
    this._data = data;
    L.Util.setOptions(this, options);
    L.CircleMarker.prototype.initialize.call(
      this, 
      center, 
      {radius: radius, stroke: false, fill: false}
    );
  },
  
  onAdd: function (map) {
    L.CircleMarker.prototype.onAdd.call(this, map);
    
    // create a DOM element and put it into one of the map panes
    var radius = this._radius;
    
    this._container.setAttribute("class", "leaflet-zoom-hide");
    
    this._g = d3.select(this._container).append("g");
      //.attr("transform", "translate(" + radius + "," + radius + ")");

    // add a viewreset event listener for updating layer's position, do the latter
    map.on('viewreset', this._reset, this);
    this._reset();
  },

  onRemove: function (map) {
    // remove layer's DOM elements and listeners
    L.CircleMarker.prototype.onRemove.call(this, map);
    map.off('viewreset', this._reset, this);
  },
  
  setStyle: function(options) {
    L.Util.setOptions(this, options);
    console.log(this.options.opacity);
    this._reset();
  },
  
  setData: function(data) {
    this._data = data;
    this._reset();
  },
  
  _reset: function () {
    // Set Position of the container
    var p = this._map.latLngToLayerPoint(this._center);
    this._g.attr("transform", "translate(" + p.x + "," + p.y + ")");
    
    // Draw polar area chart
    var radius = this._radius;
    var dmax = this.options.maxValue || d3.max(this._data);
    
    var pie = d3.pie().value(function(d) {return 1;});
    var arc = d3.arc().innerRadius(0).outerRadius(function(d) {return d.data * radius / dmax});
    var color = d3.scaleOrdinal(this.options.colors);
    
    // remove old polar chart if necessary
    this._g.selectAll("path").remove();
    
    // redraw the polar chart
    this._g.selectAll("path")
  		.data(pie(this._data))
      .enter()
      .append("path")
      .attr("class", "leaflet-clickable")
      .attr("d", arc)
      .attr("fill", function(d, i) {return color(i)})
      .attr("fill-opacity", this.options.opacity);
  }
});

L.polarChart = function(center, radius, data, options) {
  return new L.PolarChart(center, radius, data, options);
};

// Methods that enhance R htmlwidget leaflet

/*
Add a segment on the map with a triangle in the middle representing its direction.

@param options:
  data.frame with columns lng, lat, radius and optionally opacity, maxValue
  popup and layerId
  
@param data:
  matrix containing the data to contruct the polar area charts
    
*/
window.LeafletWidget.methods.addPolarChart = function(options, data, colors) {
  for (var i = 0; i < options.lng.length; i++) {
    
    style = {};
    if (options.opacity) style.opacity = options.opacity[i];
    if (options.maxValue) style.maxValue = options.maxValue[i];
    if (colors) style.colors = colors;
    
    var l = L.polarChart(
      [options.lat[i], options.lng[i]],
      options.radius[i],
      data[i],
      style
    );
    
    if (options.popup) l.bindPopup(options.popup[i]);
    
    var id = options.layerId ? options.layerId[i] : undefined;
    this.layerManager.addLayer(l, "polarChart", id);
  }
};

window.LeafletWidget.methods.updatePolarCharts = function(options, data, colors) {
  for (var i = 0; i < options.layerId.length; i++) {
    var l = this.layerManager.getLayer("polarChart", options.layerId[i]);
    
    var style = {};
    if (options.opacity) style.opacity = options.opacity[i];
    if (options.maxValue) style.maxValue = options.maxValue[i];
    if (colors) style.colors = colors;
    
    if (options.popup) l.bindPopup(options.popup[i]);
    
    l.setStyle(style);
    
    if (data) {
      l.setData(data[i]);
    }
  }
};