 requireDep <- function(map, deps) {
   mapDeps <- list(
     
     updateCircleMarkers = htmlDependency(
       "updateCircleMarkers",
       "1.0",
       src = system.file("leafletPlugins", package = "antaresViz"), 
       script = "updateCircleMarkers.js"
     ),
     
     directedSegment = htmlDependency(
       "directedSegment", 
       "1.0",
       src = system.file("leafletPlugins", package = "antaresViz"), 
       script = "directedSegment.js"
     ),
     
     polarChart = htmlDependency(
       "polarChart",
       "1.0",
       src = system.file("leafletPlugins", package = "antaresViz"), 
       script = "polarChart.js"
     ),
     
     d3 = htmlDependency(
       "d3",
       "4.2.3",
       src = system.file("leafletPlugins", package = "antaresViz"), 
       script = "d3.min.js"
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
       script = "antaresLegend.js"
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
