/*
Update the style of circle markers

@param data
  data.frame with columns layerId and optionnaly radius, stroke, color, weight,
  opacity, fill, fillColor, fillOpacity, popup
  
*/
window.LeafletWidget.methods.updateCircleMarkers = function(data) {
  console.log(data);
  
  for (var i = 0; i < data.layerId.length; i++) {
    var l = this.layerManager.getLayer("marker", data.layerId[i]);
    
    var style = {};
    if (data.stroke) style.stroke = data.stroke[i];
    if (data.color) style.color = data.color[i];
    if (data.weight) style.weight = data.weight[i];
    if (data.opacity) style.opacity = data.opacity[i];
    if (data.fill) style.fill = data.fill[i];
    if (data.fillColor) style.fillColor = data.fillColor[i];
    if (data.fillOpacity) style.fillOpacity = data.fillOpacity[i];
    
    l.setStyle(style);
    
    if(data.radius) l.setRadius(data.radius[i]);
    if (data.popup) {
      l.bindPopup(data.popup[i]);
    }
  }
};