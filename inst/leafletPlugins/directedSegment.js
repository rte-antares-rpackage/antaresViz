// Copyright © 2016 RTE Réseau de transport d’électricité
/*
DirectedSegment

Leaflet class for drawing a simple segment with an arrow on its middle that 
represents its direction.

Creation:
  L.directedSegment(<LatLng> start, <LatLng> end, <DirectedSegment options> options?)

Options:
  color: color of the segment
  weight: width of the segment
  opacity: opacity of the segment
  dir: 1 if direction is from start to end, -1 if it is from end to start, 0
    if it has no direction

Methods:
  setStyle(<DirectedSegment options> options): update the style of the directed 
    segment

*/

L.DirectedSegment = L.Polyline.extend({
  options: {
    color: "blue",
    weight: 3,
    opacity: 1,
    dir: 1
  },
  
  initialize: function(start, end, options) {
    this._start = start;
    this._end = end;
    L.Polyline.prototype.initialize.call(this, [start, end], options);
  },

  onAdd: function(map) {
    L.Polyline.prototype.onAdd.call(this, map);
    this._container.setAttribute("class", "leaflet-zoom-hide");
    
    function createSvgElement(el, parent) {
      el = document.createElementNS("http://www.w3.org/2000/svg", el);
      parent.appendChild(el);
      return el;
    }
    
    this._arrow = createSvgElement("path", this._container);
    this._arrow.setAttribute("d", "M -10,-10 -10,10 10,0 Z");
    this._arrow.setAttribute("class", "leaflet-clickable");

    // add a viewreset event listener for updating layer's position, do the latter
    map.on('viewreset', this._reset, this);
    this._reset();
  },
  
  onRemove: function(map) {
    L.Polyline.prototype.onRemove.call(this, map);
    map.off('viewreset', this._reset, this);
  },
  
  _reset: function() {
    var p1 = this._map.latLngToLayerPoint(this._start);
    var p2 = this._map.latLngToLayerPoint(this._end);

    // Middle point
    var middle = {x: (p1.x + p2.x) / 2, y: (p1.y + p2.y) / 2};
    // Angle of the line
    var angle = Math.atan((p2.y - p1.y) / (p2.x - p1.x));
    angle = angle / Math.PI * 180;
    if (p2.x - p1.x < 0) {
      angle = 180 + angle;
    }
    if (this.options.dir == -1) {
      angle = 180 + angle;
    }
    
    // Place and rotate
    var transform = L.Util.template(
      "translate({x}, {y}) rotate({a}) scale({s})",
      {
        x: middle.x, 
        y: middle.y, 
        a: angle, 
        s: 0.35 * Math.sqrt(this.options.weight)}
    );
    this._arrow.setAttribute("transform", transform);
    
    // style arrow
    this._arrow.style.fill = this.options.color; 
    this._arrow.style.fillOpacity = this.options.dir === 0 ? 0 : this.options.opacity;
  },
  
  setStyle: function(options) {
    L.Polyline.prototype.setStyle.call(this, options);
    L.Util.setOptions(this, options);
    this._reset();
  }
});

L.directedSegment = function(start, end, options) {
  return new L.DirectedSegment(start, end, options);
};


// Methods that enhance R htmlwidget leaflet

/*
Add a segment on the map with a triangle in the middle representing its direction.

@param data:
  data.frame with columns x0, y0, x1, y1 and optionnaly dir, color, opacity, weight
  popup and layerId
    
*/
window.LeafletWidget.methods.addDirectedSegments = function(data) {
  for (var i = 0; i < data.x0.length; i++) {
    var style = {};
    if (data.color) style.color = data.color[i];
    if (data.opacity) style.opacity = data.opacity[i];
    if (data.weight) style.weight = data.weight[i];
    if (data.dir) style.dir = data.dir[i];
    var l = L.directedSegment([data.y0[i], data.x0[i]], [data.y1[i], data.x1[i]], style);
    
    if (data.popup) l.bindPopup(data.popup[i]);
    
    var id = data.layerId ? data.layerId[i] : undefined;
    this.layerManager.addLayer(l, "directedSegment", id);
  }
};

/*
Update the style of directed segments

@param data
  data.frame with columns layerId and optionnaly dir, color, opacity popup 
  and weight
  
*/
window.LeafletWidget.methods.updateDirectedSegments = function(data) {
  for (var i = 0; i < data.layerId.length; i++) {
    var style = {};
    if (data.color) style.color = data.color[i];
    if (data.opacity) style.opacity = data.opacity[i];
    if (data.weight) style.weight = data.weight[i];
    if (data.dir) style.dir = data.dir[i];
    
    var l = this.layerManager.getLayer("directedSegment", data.layerId[i]);
    l.setStyle(style);
    
    if (data.popup) l.bindPopup(data.popup[i]);
  }
};
