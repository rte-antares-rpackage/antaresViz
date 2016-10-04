L.MyLegend = L.Control.extend({
  options: {
    position: "topright",
    areaColor: null,
    areaSize: null,
    areaSizesColors: null,
    linkColor: null,
    linkSize: null,
    collapsed: true
  },
  
  onAdd: function() {
    var container = L.DomUtil.create('div', 'leaflet-bar leaflet-control leaflet-control-custom');
    
    container.style.backgroundColor = 'white';
    container.style.padding = "5px";

    
    var content = L.DomUtil.create("div");
    content.style.width = "200px";
    content.style.height = "100px";
    content.style.backgroundColor = "red";
    content.style.display = "none";
    
    var btn = L.DomUtil.create("button", "btn btn-link btn-xs pull-right");
    btn.textContent = "Show legend";
    
    var self = this;
    btn.onclick = function() {
      self.options.collapsed = !self.options.collapsed;
      self.showHide();
    };
    
    container.appendChild(content);
    container.appendChild(btn);
    
    this._content = content;
    this._btn = btn;
    this._container = container;
    
    return container;
  },
  
  onRemove: function() {},
  
  showHide: function() {
    if (this.options.collapsed) {
      this._content.style.display = "none";
      this._btn.textContent = "Show legend";
    } else {
      this._content.style.display = "block";
      this._btn.textContent = "Hide legend";
    }
  }
});

L.control.myLegend = function (options) {
    return new L.MyLegend(options);
};
