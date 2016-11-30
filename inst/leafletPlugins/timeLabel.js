// Copyright © 2016 RTE Réseau de transport d’électricité
L.TimeLabel = L.Control.extend({
  options: {
    position: "bottomleft",
    timeStep: "hourly",
    time: null
  },
  
  initialize: function(options) {
    this._label = L.DomUtil.create('div');//, 'leaflet-bar leaflet-control leaflet-control-custom');
    this._label.style.fontSize = "16px";
    this._label.style.fontFamily = "sans-serif";
    this._label.style.fontWeight= "bold";
    this._label.style.textAlign = "center";
    this._label.style.color = "#555";
    
    L.Control.prototype.initialize.call(this, options);
  },
  
  onAdd: function() {
    this._reset();
    return this._label;
  },
  
  onRemove: function() {
    console.log(this._label.parentNode);
    this._label.parentNode.removeChild(this._label);
  },
  
  setOptions: function(options) {
    L.Util.setOptions(this, options);
    this._reset();
  },
  
  _reset: function() {
    var date = new Date(this.options.time * 1000);
    
    switch(this.options.timeStep) {
      case "hourly":
        var day = date.toUTCString().slice(0, 11);
        var h = date.toUTCString().slice(17, 22);
        this._label.innerHTML = day + '<br/>' + h;
        break;
      case "daily":
        this._label.innerHTML = date.toUTCString().slice(0, 11);
        break;
      case "weekly":
        this._label.innerHTML = date.toUTCString().slice(0, 11);
        break;
      case "monthly":
        this._label.innerHTML = date.toUTCString().slice(7, 11);
        break;
      default:
        this._label.innerHTML = this.options.time;
        break;
    }
  }
});

L.timeLabel = function (options) {
    return new L.TimeLabel(options);
};

window.LeafletWidget.methods.addTimeLabel = function(options) {
  var l = L.timeLabel(options);
  this.controls.add(l, "timeLabel");
};

window.LeafletWidget.methods.updateTimeLabel = function(options) {
  var l = this.controls._controlsById.timeLabel;
  l.setOptions(options);
};
