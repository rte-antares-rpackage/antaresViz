.prepareOptions <- function(required, optional) {
  options <- do.call(data.frame, required)
  for (o in names(optional)) {
    if (!is.null(optional[[o]])) options[[o]] <- optional[[o]]
  }
  options
}

#' @export
updateCircleMarkers <- function(map, layerId, radius=NULL, stroke=NULL, 
                                color=NULL, weight=NULL, 
                                opacity=NULL, fill=NULL, fillColor=NULL, 
                                fillOpacity= NULL) {
  options <- .prepareOptions(
    required = list(layerId = layerId),
    optional = list(radius = radius, stroke = stroke, color = color, 
                    weight = weight, opacity = opacity, fill = fill, 
                    fillColor = fillColor, fillOpacity = fillOpacity)
  )
  
  map %>% requireDep("updateCircleMarkers") %>% 
    invokeMethod(data=NULL, "updateCircleMarkers", options)
}

#' @export
addDirectedSegments <- function(map, x0, y0, x1, y1, color = "blue", weight = 3, 
                                opacity = 1, dir = 1, popup = NULL, layerId = NULL) {
  
  options <- .prepareOptions(
    required = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1),
    optional = list(dir = dir, color = color, weight = weight, opacity = opacity, 
                    layerId = layerId, popup = popup)
  )
  
  map %>% 
    requireDep("directedSegment") %>% 
    invokeMethod(data = leaflet:::getMapData(map), "addDirectedSegments", options)
}

#' @export
updateDirectedSegments <- function(map, layerId, color = NULL, weight = NULL, 
                                   opacity = NULL, dir = NULL, popup = NULL) {
  options <- .prepareOptions(
    required = list(layerId = layerId),
    optional = list(color = color, weight = weight, opacity = opacity,dir = dir,
                    popup = popup)
  )
  
  invokeMethod(map, data = leaflet:::getMapData(map), "updateDirectedSegments", options)
}

#' @export
addPolarChart <- function(map, lng, lat, data, radius = 20, opacity = 1,
                          scale = c("radius", "area"), maxValue = NULL, 
                          colors = NULL, layerId = NULL) {
 
  scale <- match.arg(scale)
  
  options <- .prepareOptions(
    required = list(lng = lng, lat = lat), 
    optional = list(radius = radius, opacity = opacity, maxValue = 1, layerId = layerId)
  )
  
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
    invokeMethod(leaflet:::getMapData(map), "addPolarChart", options, data, colors)
}

updatePolarChart <- function(map, layerId, data = NULL, radius = 20, opacity = 1,
                             scale = c("radius", "area"), maxValue = NULL) {
  scale <- match.arg(scale)
  
  options <- .prepareOptions(
    required = list(layerId = layerId),
    optional = list(radius = radius, opacity = opacity, maxValue = 1)
  )
  
  if (!is.null(data)) {
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
  }
  
  invokeMethod(map, data = NULL, "updatePolarCharts", options, data)
}