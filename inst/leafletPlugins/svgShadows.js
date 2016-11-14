// Copyright © 2016 RTE Réseau de transport d’électricité
window.LeafletWidget.methods.addShadows = function() {
  var svg = this._panes.overlayPane.children[0];
  var els = svg.childNodes;
  
  var filter = new DOMParser().parseFromString('\
  <filter xmlns="http://www.w3.org/2000/svg" id="dropShadow">\
    <feGaussianBlur in="SourceAlpha" stdDeviation="1" />\
    <feOffset dx="1" dy="1" result="offsetblur"/>\
    <feComponentTransfer>\
    <feFuncA type="linear" slope="0.4"/>\
  </feComponentTransfer>\
    <feMerge>\
      <feMergeNode />\
      <feMergeNode in="SourceGraphic" />\
    </feMerge>\
  </filter>\
  ', 'image/svg+xml');
  
  svg.appendChild(svg.ownerDocument.importNode(filter.documentElement, true));
  
  for (var i = 0; i < els.length - 1; i++) {
    var cl = els[i].getAttribute("class");
    if (!cl || cl.indexOf("no-shadow") == -1) {
      els[i].setAttribute("filter", "url(#dropShadow)");
    }
  }
};