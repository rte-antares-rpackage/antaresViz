 requireDep <- function(map, deps) {
   depsInMap <- sapply(map$dependencies, function(x) x$name)
   
   for (dep in deps) {
     if (!any(depsInMap == dep)) {
       map$dependencies <- c(map$dependencies, list(mapDeps[[dep]]))
     }
   }
   
   map
 }

mapDeps <- list(
  
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
  )
  
)