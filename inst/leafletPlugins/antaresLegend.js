/* Copyright © 2016 RTE Réseau de transport d’électricité */
L.AntaresLegend = L.Control.extend({
  options: {
    position: "topright",
    htmlAreaColor: null,
    htmlAreaSize: null,
    htmlLinkColor: null,
    htmlLinkSize: null,
    onComplete:null,
    display: "choose",
    collapsed: true,
    areas_name: "Areas",
    links_names: "Links",
    show_legend: "Show legend",
    hide_legend: "Hide legend"
  },
  
  initialize: function(options) {
    var createEl = L.DomUtil.create;
    var container = createEl('div');
    container.style.padding = "5px";
    
    var areas_name, links_names;
    areas_name = options.areas_name;
    links_names = options.links_names;

    var btn = createEl("button", "btn btn-link btn-xs pull-left", container);
        
    var content = createEl("div", "", container);
    content.innerHTML = '\
      <br><div id = "legend-area" class="legend">\
        <h2>' + areas_name + '</h2>\
        <div id="area-color" class="legend-section"></div>\
        <div id="area-size" class="legend-section"></div>\
        <div style="clear:both;"></div>\
      </div>\
      <div id = "legend-link" class="legend">\
        <h2>' + links_names + '</h2>\
        <div id="link-color" class="legend-section"></div>\
        <div id="link-size" class="legend-section"></div>\
        <div style="clear:both;"></div>\
      </div>\
    ';
    
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
    if (this.options.display == "hidden") {
      this._container.className = "";
      this._container.style.display = "none";
      this._content.style.display = "none";
      this._btn.style.display = "none";
    } else {
      this._container.className = 'leaflet-bar leaflet-control leaflet-control-custom';
      this._container.style.display = "block";
      this._container.style.backgroundColor = 'white';
      this._content.style.display = "block";
      this._btn.style.display = "block";
    }
    
    if (this.options.display == "visible") {
      this.options.collapsed = false;
      this._btn.style.display = "none";
    }
    
    var legAreas = this._content.querySelector("#legend-area");
    var legLinks = this._content.querySelector("#legend-link");
    var o = this.options;
    
    // If the legend is empty, do not display it
    if (o.htmlAreaSize || o.htmlAreaColor || o.htmlLinkSize || o.htmlLinkColor) {
      this._container.style.display = "block";
      this.showHide();
    } else {
      this._container.style.display = "none";
      return;
    }
    
    // If one section is empty do not show this section
    legAreas.style.display = (!o.htmlAreaColor && !o.htmlAreaSize)?"none":"block";
    legLinks.style.display = (!o.htmlLinkColor && !o.htmlLinkSize)?"none":"block";
    
    // Update html of each section
    this._content.querySelector("#area-size").innerHTML = o.htmlAreaSize;
    this._content.querySelector("#area-color").innerHTML = o.htmlAreaColor;
    this._content.querySelector("#link-size").innerHTML = o.htmlLinkSize;
    this._content.querySelector("#link-color").innerHTML = o.htmlLinkColor;
    eval(this.options.onComplete);
  },
  
  showHide: function() {
    var show_legend, hide_legend;
    show_legend = this.options.show_legend;
    hide_legend = this.options.hide_legend;
    
    if (this.options.collapsed) {
      this._content.style.display = "none";
      this._btn.textContent = show_legend;
    } else {
      this._content.style.display = "block";
      this._btn.textContent = hide_legend;
    }
  },
  
  setOptions: function(options) {
    L.Util.setOptions(this, options);
    this._reset();
  }
});

L.antaresLegend = function (options) {
    return new L.AntaresLegend(options);
};

window.LeafletWidget.methods.addAntaresLegend = function(options) {
  var l = L.antaresLegend(options);
  this.controls.add(l, "antaresLegend");
};

window.LeafletWidget.methods.updateAntaresLegend = function(options) {
  var l = this.controls._controlsById.antaresLegend;
  l.setOptions(options);
};