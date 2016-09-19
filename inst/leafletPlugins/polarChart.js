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

L.PolarChart = L.Class.extend({
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
  },
  
  onAdd: function (map) {
    this._map = map;
    
    // create a DOM element and put it into one of the map panes
    var container = map.getPanes().overlayPane;
    var radius = this._radius;
    
    this._svg = d3.select(container).append("svg")
      .attr("width", radius*2)
      .attr("height", radius * 2)
      .attr("class", "leaflet-zoom-hide");
    
    this._g = this._svg.append("g").attr("transform", "translate(" + radius + "," + radius + ")");

    // add a viewreset event listener for updating layer's position, do the latter
    map.on('viewreset', this._reset, this);
    this._reset();
  },

  onRemove: function (map) {
    // remove layer's DOM elements and listeners
    this._svg.remove();
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
      .attr("d", arc)
      .attr("fill", function(d, i) {return color(i)})
      .attr("fill-opacity", this.options.opacity);
      
    // update layer's position
    var pos = this._map.latLngToLayerPoint(this._center);
    L.DomUtil.setPosition(this._svg.node(), {x: pos.x - radius, y: pos.y - radius});
  }
});

L.polarChart = function(center, radius, data, options) {
  return new L.PolarChart(center, radius, data, options);
};

// Methods that enhance R htmlwidget leaflet

/*
Add a segment on the map with a triangle in the middle representing its direction.

@param options:
  data.frame with columns lng, lat, radius and optionally opacity, maxValue and
  layerId
  
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
    
    var id = options.layerId ? options.layerId[i] : undefined;
    this.layerManager.addLayer(l, "polarChart", id);
  }
};

window.LeafletWidget.methods.updatePolarCharts = function(options, data, colors) {
  for (var i = 0; i < options.layerId.length; i++) {
    var l = this.layerManager.getLayer("polarChart", options.layerId[i]);
    
    var style = {};
    style = {};
    if (options.opacity) style.opacity = options.opacity[i];
    if (options.maxValue) style.maxValue = options.maxValue[i];
    if (colors) style.colors = colors;
    
    l.setStyle(style);
    
    if (data) {
      l.setData(data[i]);
    }
  }
};