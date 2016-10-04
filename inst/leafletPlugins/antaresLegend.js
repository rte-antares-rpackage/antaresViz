L.AntaresLegend = L.Control.extend({
  options: {
    position: "topright",
    html: null,
    collapsed: true
  },
  
  initialize: function(options) {
    var container = L.DomUtil.create('div', 'leaflet-bar leaflet-control leaflet-control-custom');
    container.style.backgroundColor = 'white';
    container.style.padding = "5px";
    
    var content = L.DomUtil.create("div");
    
    var btn = L.DomUtil.create("button", "btn btn-link btn-xs pull-right");
    
    container.appendChild(content);
    container.appendChild(btn);
    
    this._content = content;
    this._btn = btn;
    this._container = container;
    
    L.Control.prototype.initialize.call(this, options);
  },
  
  onAdd: function() {
    var self = this;
    
    this._btn.onclick = function() {
      self.options.collapsed = !self.options.collapsed;
      self.showHide();
    };
    
    this._reset();
    
    return this._container;
  },
  
  onRemove: function() {
    this._container.parentNode.removeChild(this._container);
  },
  
  _reset: function() {
    if (!this.options.html) {
      this._container.style.display = "none";
    } else {
      this._container.style.display = "block";
      this._content.innerHTML = this.options.html;
      this.showHide();
    }
  },
  
  showHide: function() {
    if (this.options.collapsed) {
      this._content.style.display = "none";
      this._btn.textContent = "Show legend";
    } else {
      this._content.style.display = "block";
      this._btn.textContent = "Hide legend";
    }
  },
  
  setContent: function(html) {
    this.options.html = html;
    this._reset();
  }
});

L.antaresLegend = function (options) {
    return new L.AntaresLegend(options);
};

window.LeafletWidget.methods.addAntaresLegend = function(html) {
  var l = L.antaresLegend({html:html});
  console.log(this.controls);
  this.controls.add(l, "antaresLegend");
};

window.LeafletWidget.methods.updateAntaresLegend = function(html) {
  var l = this.controls._controlsById.antaresLegend;
  l.setContent(html);
};