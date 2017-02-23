# Copyright © 2016 RTE Réseau de transport d’électricité

requireDep <- function(map, deps) {
   mapDeps <- list(
     
     directedSegment = htmlDependency(
       "directedSegment", 
       "1.0",
       src = system.file("leafletPlugins", package = "antaresViz"), 
       script = "directedSegment.js"
     ),
     
     shadows = htmlDependency(
       "shadows",
       "1.0",
       src = system.file("leafletPlugins", package = "antaresViz"),
       script = "svgShadows.js"
     ),
     
     antaresLegend = htmlDependency(
       "antaresLegend",
       "1.0",
       src = system.file("leafletPlugins", package = "antaresViz"),
       script = "antaresLegend.js",
       stylesheet = "antaresLegend.css"
     ),
     
     timeLabel = htmlDependency(
       "timeLabel",
       "1.0",
       src = system.file("leafletPlugins", package = "antaresViz"),
       script = "timeLabel.js"
     ),
     
     minichart = htmlDependency(
       "minichart",
       "0.2.2",
       src = system.file("leafletPlugins", package = "antaresViz"),
       script = "leaflet.minichart.min.js"
     ),
     
     minichart_bindings = htmlDependency(
       "minichart_bindings",
       "1.0",
       src = system.file("leafletPlugins", package = "antaresViz"),
       script = "minichart_bindings.js"
     )
     
   )
   
   depsInMap <- sapply(map$dependencies, function(x) x$name)
   
   for (dep in deps) {
     if (!any(depsInMap == dep)) {
       map$dependencies <- c(map$dependencies, list(mapDeps[[dep]]))
     }
   }
   
   map
 }
