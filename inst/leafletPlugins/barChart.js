L.BarChart = L.CircleMarker.extend({
  options: {
    data: null,
    minValue: 0,
    maxValue: 0,
    width:50,
    height:50,
    colors: d3.schemeCategory10,
    opacity: 1
  },
  
  initialize: function(center, size, data, options) {
    this.options.width = size;
    this.options.height = size;
    this.options.data = data;
    this._center = center;
    L.Util.setOptions(this, options);
    L.CircleMarker.prototype.initialize.call(
      this, 
      center, 
      {radius: size / 2, stroke: false, fill: false}
    );
  },
  
  onAdd: function(map) {
    L.CircleMarker.prototype.onAdd.call(this, map);
    this._container.setAttribute("class", "leaflet-zoom-hide");
    this._g = d3.select(this._container).append("g");
    map.on('viewreset', this._reset, this);
    this._reset();
  },
  
  onRemove: function() {
    // remove layer's DOM elements and listeners
    L.CircleMarker.prototype.onRemove.call(this, map);
    map.off('viewreset', this._reset, this);
  },
  
  setOptions: function(options) {
    if (options.size) {
      console.log("glop");
      this.options.width = options.size;
      this.options.height = options.size;
    }
    
    L.Util.setOptions(this, options);
    this._reset();
  },
  
  _reset: function() {
    var x = d3.scaleLinear()
      .domain([this.options.minValue, this.options.maxValue])
      .range([this.options.height, 0]);
    
    var color = d3.scaleOrdinal(this.options.colors);
    
    var barWidth = this.options.width / this.options.data.length;
      
    // Set Position of the container
    var p = this._map.latLngToLayerPoint(this._center);
    this._g.attr("transform", "translate(" + (p.x - this.options.width / 2) + "," + (p.y - x(0)) + ")");
    
    // Remove old chart
    this._g.selectAll("rect").remove();
    
    var bar = this._g.selectAll("rect")
      .data(this.options.data)
      .enter()
      .append("rect")
      .attr("x", function(d, i) {return i * barWidth})
      .attr("y", function(d) {return d > 0? x(d): x(0);})
      .attr("height", function(d) {return Math.abs(x(d) - x(0))})
      .attr("width", barWidth)
      .attr("fill", function(d, i) {return color(i)})
      .attr("fill-opacity", this.options.opacity);
        
  }
});

L.barChart = function(center, size, data, options) {
  return new L.BarChart(center, size, data, options);
};

window.LeafletWidget.methods.addBarCharts = function(options, data, colors) {
  for (var i = 0; i < options.lng.length; i++) {
    
    var style = {};
    if (options.minValue) style.minValue = options.minValue[i];
    if (options.maxValue) style.maxValue = options.maxValue[i];
    if (options.opacity) style.opacity = options.opacity[i];
    if (colors) style.colors = colors;
    
    style.size = options.size ? options.size[i]: 30;
    
    var l = L.barChart(
      [options.lat[i], options.lng[i]],
      style.size,
      data[i],
      style
    );
    
    if (options.popup) l.bindPopup(options.popup[i]);
    
    var id = options.layerId ? options.layerId[i] : undefined;
    this.layerManager.addLayer(l, "barChart", id);
  }
};

window.LeafletWidget.methods.updateBarCharts = function(options, data, colors) {
  for (var i = 0; i < options.layerId.length; i++) {
    var l = this.layerManager.getLayer("barChart", options.layerId[i]);
    
    var opts = {};
    if (options.minValue) opts.minValue = options.minValue[i];
    if (options.maxValue) opts.maxValue = options.maxValue[i];
    if (options.opacity) opts.opacity = options.opacity[i];
    if (options.size) opts.size = options.size[i];
    if (colors) opts.colors = colors;
    if (data) opts.data = data[i];
    
    if (options.popup) l.bindPopup(options.popup[i]);
    
    l.setOptions(opts);
  }
};
