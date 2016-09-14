

#' @export
addDirectedSegments <- function(map, x0, y0, x1, y1, color = "blue", weight = 3, 
                                opacity = 1, dir = 1, popup = NULL, layerId = NULL) {
  
  data <- data.frame(x0 = x0, y0 = y0, x1 = x1, y1 = y1, dir = dir, 
                     color = color, weight = weight, opacity = opacity)
  
  if(!is.null(layerId)) data$layerId <- layerId
  if(!is.null(popup)) data$popup <- popup
  
  map %>% 
    requireDep("directedSegment") %>% 
    invokeMethod(data = leaflet:::getMapData(map), "addDirectedSegments", data)
}

#' @export
updateDirectedSegments <- function(map, layerId, color = "blue", weight = 3, 
                                   opacity = 1, dir = 1, popup = NULL) {
  data <- data.frame(layerId = layerId, color = color, weight = weight, opacity = opacity, dir = dir)
  if(!is.null(popup)) data$popup <- popup
  
  invokeMethod(map, data = leaflet:::getMapData(map), "updateDirectedSegments", data)
}

#' @export
addPolarChart <- function(map, lng, lat, data, radius = 20, opacity = 1,
                          scale = c("radius", "area"), maxValue = NULL) {
 
  scale <- match.arg(scale)
  
  options <- data.frame(lng = lng, lat = lat, radius = radius, 
                        opacity = opacity, maxValue = 1)
  
  # Data preparation
  if (nrow(options) == 1) data <- matrix(data, nrow = 1)
  data <- as.matrix(data)
  
  if (scale == "area") {
    data <- sqrt(data)
    if (!is.null(maxValue)) maxValue <- sqrt(maxValue)
  } 
  
  if (is.null(maxValue)) {
    maxValue <- apply(data, 2, max)
  } else {
    if (length(maxValue) == 1 && maxValue == 0) maxValue <- max(data)
    maxValue <- rep_len(maxValue, ncol(data))
  }
  
  for (i in 1:ncol(data)) {
    data[, i] <- data[, i] / maxValue[i]
  }
  
  map %>% requireDep(c("d3", "polarChart")) %>% 
    invokeMethod(leaflet:::getMapData(map), "addPolarChart", options, data)
}

