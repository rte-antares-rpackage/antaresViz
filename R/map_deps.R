# Copyright © 2016 RTE Réseau de transport d’électricité

requireDep <- function(map, deps) {
   mapDeps <- list(
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
