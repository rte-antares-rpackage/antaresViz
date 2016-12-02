addD3charts <- function(map, lng, lat, data = 1, maxValues = NULL, type = "auto", 
                        fillColor = NULL, colorPalette = NULL,
                        width = 60, height = 60, opacity = 1, showLabels = FALSE,
                        labelStyle = "fill:white;font-size:8px;", 
                        labelPrecision = 0, labelText = NULL,
                        transitionTime = 750, popup = NULL, layerId = NULL) {
  
  type <- match.arg(type, c("auto", "bar", "pie", "polar-area", "polar-radius"))
  
  # Data preparation
  
  # When adding only one d3chart, data can be a vector or a data frame, so it 
  # needs to be converted to a matrix with correct lines and columns
  if (max(length(lng), length(lat)) == 1) {
    data <- matrix(data, nrow = 1)
  } else {
    if (is.vector(data)) {
      data <- matrix(data, ncol = 1, nrow = max(length(lng), length(lat)))
    }
  }
  
  data <- as.matrix(data)
  
  # If maxValues is not set, explicitely, we use the maximal observed value
  if (is.null(maxValues)) maxValues <- max(abs(data))
  
  # If there is only one variable in data, we draw circles with different radius
  # else we draw bar charts by default.
  if (type == "auto") {
    type <- ifelse (ncol(data) == 1, "polar-area", "bar")
  }
  
  options <- .prepareOptions(
    required = list(lng = lng, lat = lat), 
    optional = list(type = type, width = width, height = height, 
                    opacity = opacity, showLabels = showLabels, 
                    labelStyle = labelStyle, labelPrecision = labelPrecision,
                    labelText = labelText, transitionTime = transitionTime,
                    popup = popup, layerId = layerId, fillColor = fillColor)
  )
  
  map %>% requireDep(c("d3", "d3chart", "d3chart_bindings")) %>% 
    invokeMethod(leaflet:::getMapData(map), "addD3charts", 
                 options, data, maxValues, colorPalette)
}

updateD3charts <- function(map, layerId, data = NULL, maxValues = NULL, type = NULL, 
                           fillColor = NULL, colorPalette = NULL, 
                           width = NULL, height = NULL, opacity = NULL, showLabels = NULL,
                           labelStyle = NULL, 
                           labelPrecision = NULL, labelText = NULL,
                           transitionTime = NULL, popup = NULL) {
  
  type <- match.arg(type, c("auto", "bar", "pie", "polar-area", "polar-radius"))
  
  # Data preparation
  if (!is.null(data)) {
    if (length(layerId) == 1) {
      data <- matrix(data, nrow = 1)
    } else {
      if (is.vector(data)) {
        data <- matrix(data, ncol = 1, nrow = length(layerId))
      }
    }
    
    data <- as.matrix(data)
    
    if (type == "auto") {
      type <- ifelse (ncol(data) == 1, "polar-area", "bar")
    }
  } else {
    type <- NULL
  }
  
  
  options <- .prepareOptions(
    required = list(layerId = layerId), 
    optional = list(type = type, width = width, height = height, 
                    opacity = opacity, showLabels = showLabels, 
                    labelStyle = labelStyle, labelPrecision = labelPrecision,
                    labelText = labelText, transitionTime = transitionTime,
                    popup = popup, fillColor = fillColor)
  )
  
  map %>% requireDep(c("d3", "d3chart", "d3chart_bindings")) %>% 
    invokeMethod(leaflet:::getMapData(map), "updateD3charts", 
                 options, data, maxValues, colorPalette)
  
}