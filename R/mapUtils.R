#' @export
addDirectedSegments <- function(map, x0, y0, x1, y1, color = "blue", weight = 3, opacity = 1, dir = 1, layerId = NULL) {
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
  
  data <- data.frame(x0 = x0, y0 = y0, x1 = x1, y1 = y1, dir = dir, 
                     color = color, weight = weight, opacity = opacity)
  
  if(!is.null(layerId)) data$layerId <- layerId
  
  invokeMethod(map, data = leaflet:::getMapData(map), "addDirectedSegments", data)
}

#' @export
updateDirectedSegments <- function(map, layerId, color = "blue", weight = 3, opacity = 1, dir = 1) {
  data <- data.frame(layerId = layerId, color = color, weight = weight, opacity = opacity, dir = dir)
  
  invokeMethod(map, data = leaflet:::getMapData(map), "updateDirectedSegments", data)
}