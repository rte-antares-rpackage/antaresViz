L.directedSegment = function(pt1, pt2, style) {
  var arrow = '\
  <svg id="glop" height="100" width="100">\
    <g>\
      <path d="M 0,0 0,20 20,10 Z" transform="translate(-10, -10)" />\
    </g>\
  </svg>\
  ';
  
  var iconArrow = L.divIcon({
    html: arrow, 
    className: "leaflet-label", 
    iconSize: [100, 100], iconAnchor:
    [50, 50]
  });
  
  var ptHalf = [(pt1[0] + pt2[0]) / 2, (pt1[1] + pt2[1]) / 2];
  var line = L.polyline(
    [pt1, ptHalf, pt2], 
    {opacity: style.opacity || 1, color: style.color, weight: style.weight || 3}
  );
  
  var marker = L.marker(ptHalf, {icon: iconArrow});
  
  // Default style options
  if (!style) style = {};
  style.color = style.color || "blue";
  style.weight = style.weight || 3;
  style.opacity = style.opacity || 1;
  
  return {
    line: line, 
    marker: marker,
    pt1: pt1,
    pt2: pt2,
    style: style,
    angle: 0,
    setStyle: function(style) {
      if (style) {
        for (var p in style) {
          this.style[p] = style[p];
        }
      }
      
      var path = marker._icon.getElementsByTagName("path")[0];
      path.style.fill = this.style.color;
      //path.style.stroke = this.style.color;
      path.style.fillOpacity = this.style.opacity;
      //path.style.strokeWidth = this.style.weight;
      this.line.setStyle({opacity: this.style.opacity, color: this.style.color, weight: this.style.weight});
      
      // rotate marker
      var g = marker._icon.getElementsByTagName("g")[0];
      g.setAttribute('transform', "translate(50, 50) rotate(" + this.angle + ") scale(" + 0.35 * Math.sqrt(this.style.weight) +")");
      
      return this;
    },
    addTo: function(map) {
      // Compute the needed rotation of the arrow
      var pt1cart = map.latLngToLayerPoint(this.pt1);
      var pt2cart = map.latLngToLayerPoint(this.pt2);
      
      var angle = Math.atan((pt2cart.y - pt1cart.y) / (pt2cart.x - pt1cart.x));
      angle = Math.round(angle / Math.PI * 180);
      if (pt2cart.x - pt1cart.x < 0) {
        angle = 180 + angle;
      }
      
      this.angle = angle;
      
      // Add line and marker to map
      this.line.addTo(map);
      this.marker.addTo(map);
      this.setStyle();
      
      return this;
    }
  };

};

window.LeafletWidget.methods.addDirectedSegment = function(data) {
  var style;
  for (var i = 0; i < data.x0.length; i++) {
    style = {};
    if (data.color) style.color = data.color[i];
    if (data.opacity) style.opacity = data.opacity[i];
    if (data.weight) style.weight = data.weight[i];
    L.directedSegment([data.y0[i], data.x0[i]], [data.y1[i], data.x1[i]], style).addTo(this);
  }
};
