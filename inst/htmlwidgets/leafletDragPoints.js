HTMLWidgets.widget({

  name: 'leafletDragPoints',

  type: 'output',

  factory: function(el, width, height) {
    var map = L.map(el).setView([0, 0], 2);
    L.tileLayer(
      'http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}',
      {
        attribution: 'Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ'
      }
    ).addTo(map);

    var points = [];
    var mapLayer;
    var markersLayer = [];
    
    function clear_polyline() {
      map.removeLayer( linesFeatureLayer );
    }

    // Function that updates shiny input
    function updateShinyInput() {
      var coords = points.map(function(p) {
        return p.marker.getLatLng();
      });
      if(HTMLWidgets.shinyMode){
        Shiny.onInputChange(el.id + "_coords",  coords);
        Shiny.onInputChange(el.id + "_mapcenter",  map.getCenter());
      }
    }

    map.on("moveend", updateShinyInput);

    // Initialize shiny input.
    updateShinyInput();

    return {

      renderValue: function(x) {
        if(x.init){
          for(var i=0;i<markersLayer.length;i++) {
            map.removeLayer(markersLayer[i]);
          }  
          map.setView([0, 0], 2);
          markersLayer = [];
          points = [];
          if(mapLayer !== undefined){
            map.removeLayer(mapLayer);
            mapLayer = undefined;
          }
        }
        
        // If x contains an element "map", add it to the map
        if (x.map) {
          if(mapLayer !== undefined){
            map.removeLayer(mapLayer);
          }
          mapLayer = L.geoJson(x.map, {color:"#66f", weight: 1});
          mapLayer.addTo(map);
        } else {
          if(x.reset_map){
            if(mapLayer !== undefined){
              map.removeLayer(mapLayer);
            }
          }
        }
        
        // For each new point create a marker that will be placed in the map
        if(x.geopoints){
          x.geopoints.forEach(function(p) {
            var markerIcon = L.VectorMarkers.icon({
              icon: 'circle',
              markerColor: p.color,
              iconColor: p.color == '#FFFFFF' ? '#CCCCCC' : '#FFFFFF'
            });

            p.marker =  L.marker(
              [p.lat, p.lon],
              {draggable: x.draggable, icon: markerIcon}
            );

            p.marker.bindPopup(p.info);
            p.marker.on('dragend', updateShinyInput);


            p.marker.addTo(map);

            markersLayer.push(p.marker);
            points.push(p);
          });
          updateShinyInput();
        }
        
        if(HTMLWidgets.shinyMode){
          Shiny.onInputChange(el.id + "_init",  true);
        }
      },

      resize: function(width, height) {
      }

    };
  }
});
