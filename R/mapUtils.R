addDirectedSegments <- function(map, x0, y0, x1, y1, color = "blue", weight = 3, opacity = 1) {
  # Check if dependency is present
  deps <- sapply(map$dependencies, function(x) x$name)
  if (! "directedSegment" %in% deps) {
    dep <- htmlDependency(
      "directedSegment", 
      "1.0",
      src = system.file("leafletPlugins", package = "antaresViz"), 
      script = "directedSegment.js"
    )
    map$dependencies <- c(map$dependencies, list(dep))
  }
  
  data <- data.frame(x0 = x0, y0 = y0, x1 = x1, y1 = y1, 
                     color = color, weight = weight, opacity = opacity)
  
  invokeMethod(map, data = leaflet:::getMapData(map), "addDirectedSegment", data)
}