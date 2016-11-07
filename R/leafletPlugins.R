# Copyright © 2016 RTE Réseau de transport d’électricité

# This script contains functions that enhance the leaflet package. They can add
# new types of elements to a leaflet map and update elements already drawn in a
# map.
# These functions are quite hacky because for now, leaflet does not provide any
# mechanism to add custom functions. An issue has been created on github:
# https://github.com/rstudio/leaflet/issues/290

#' Private function that prepare R arguments to be sent to javascript functions.
#'
#' @param required
#'   Named list of required parameters
#' @param optional
#'   Named list of optional parameters
#' 
#' @return 
#'   A data.frame where each column represent one parameter
#' 
#' @noRd
#' 
.prepareOptions <- function(required, optional) {
  options <- do.call(data.frame, required)
  for (o in names(optional)) {
    if (!is.null(optional[[o]])) options[[o]] <- optional[[o]]
  }
  options
}

#' Update the style of circle markers
#' 
#' Function to update the visual properties of circle markers already added to a 
#' map. The main utility of this function appears when used with function 
#' \code{\link[leaflet]{leafletProxy}}.
#' 
#' @param map
#'   A map object created with \code{\link[leaflet]{leaflet()}}
#' @param layerId
#'   The layer Id
#' @param radius
#'   Numeric vector of radii for the circles.
#' @param stroke
#'   Wheter to draw the border of the circles
#' @param color
#'   Color of the border of the circles
#' @param weight
#'   Line width of the border of the circles
#' @param opacity
#'   Opacity of the border of the circles
#' @param fill
#'   Wheter to fill the circles with colors
#' @param fillColor
#'   Fill color
#' @param fillOpacity
#'   Fill opacity
#'   
#' @return 
#' the new map object
#' 
#' @examples 
#' require(leaflet)
#' 
#' leaflet() %>% 
#'   addCircleMarkers(c(0, 0), c(0, 10), layerId = c("a", "b")) %>% 
#'   updateCircleMarkers(c("a"), color = "red")
#' 
#' \dontrun{  
#' # updateCircleMarkers becomes really usefull inside the server function of
#' # a shiny application.
#' 
#' server <- function(input, output, session) {
#'   output$map <- renderLeaflet({
#'     leaflet() %>% addCircleMarkers(c(0, 0), c(0, 10), layerId = c("a", "b"))
#'   })
#'   
#'   map <- leafletProxy("map", session)
#'   
#'   observe({
#'     updateCircleMarkers(map, layerId = c("a", "b"), color = input$color)
#'   })
#'   
#'   ...
#' }
#' 
#' }
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

#' Add or update directed segments
#' 
#' These functions add or update directed segments on a leaflet map: they are 
#' simply lines with an arrow in their middle that represent their direction.
#' 
#' @param map
#'   A map object created with \code{\link[leaflet]{leaflet()}}
#' @param x0
#'   longitude of the origin of the segments
#' @param y0
#'   lattitude of the origin of the segments
#' @param x1
#'   longitude of the destination of the segments
#' @param y1
#'   lattitude of the destination of the segments
#' @param color
#'   color of the segments
#' @param weight
#'   line width of the segments
#' @param opacity
#'   opacity of the segments
#' @param dir
#'   direction of the segments. Possible values are -1, 0 and 1. If it equals
#'   to 0, then no arrow is drawn. If it equals 1 an arrow is drawn and 
#'   points to the destination of the segment. If it equals to -1, then the arrow 
#'   is reversed and points to the origin of the segment. 
#' @param popup
#'   a character vector of the HTML content for the popups. They are displayed 
#'   when user clicks on a segment.
#' @param layerId
#'   Layer id.
#'   
#' @return 
#'   The modified map object.
#'   
#' @examples 
#' require(leaflet)
#' 
#' leaflet() %>% 
#'   addDirectedSegments(
#'     x0 = c(0, 0, 0),
#'     y0 = c(0, 1, 2),
#'     x1 = c(3, 3, 3),
#'     y1 = c(0, 1, 2),
#'     weight = c(1, 2, 3),
#'     dir = c(0, 1, -1)
#'   )
#' 
#' @export
addDirectedSegments <- function(map, x0, y0, x1, y1, color = "blue", weight = 3, 
                                opacity = 1, dir = 1, popup = NULL, layerId = NULL) {
  
  weight <- abs(weight)
  
  options <- .prepareOptions(
    required = list(x0 = x0, y0 = y0, x1 = x1, y1 = y1),
    optional = list(dir = dir, color = color, weight = weight, opacity = opacity, 
                    layerId = layerId, popup = popup)
  )
  
  map %>% 
    requireDep("directedSegment") %>% 
    invokeMethod(data = leaflet:::getMapData(map), "addDirectedSegments", options)
}

#' @rdname addDirectedSegments
#' @export
updateDirectedSegments <- function(map, layerId, color = NULL, weight = NULL, 
                                   opacity = NULL, dir = NULL, popup = NULL) {
  
  weight <- abs(weight)
  
  options <- .prepareOptions(
    required = list(layerId = layerId),
    optional = list(color = color, weight = weight, opacity = opacity,dir = dir,
                    popup = popup)
  )
  
  invokeMethod(map, data = leaflet:::getMapData(map), "updateDirectedSegments", options)
}

#' Add or update polar charts
#' 
#' These functions add and update polar area charts on a leaflet map. 
#' 
#' @param lng
#'   longitude of the polar charts
#' @param lat
#'   lattitude of the polar charts
#' @param data
#'   numeric matrix. Number of rows must equal the number of polar charts. The
#'   number of columns is equal to the number of sectors in each polar chart.
#' @param size
#'   maximal radius of a polar chart
#' @param opacity
#'   Opacity of a polar chart
#' @param scale
#'   Should the values be represented by the radius or the area ? Radius by 
#'   default. This creates a high distorsion of the values, but permits to 
#'   clearly see differences between different zones.
#' @param maxValue
#'   Either a single numeric value, or a vector with length equal to the number
#'   of columns of data. In the first case (one value), all variable will share
#'   the same scale while in the second one each variable will have its own scale.
#'   If it is \code{NULL}, the maximal value of each column is used (so scales
#'   are different for each column). If it equals to 0, the global maximum of
#'   the matrix is used and all variables share the same scale.
#' @param colors
#'   Vector of colors for the sectors of the polar chart
#' @inheritParams addDirectedSegments
#' 
#' @return 
#' The modified map object.
#' 
#' @examples 
#' 
#' require(leaflet)
#' mydata <- rbind(1:3, 4:6)
#' 
#' # Polar chart with distinct scales for each column of mydata. For the second
#' # polar chart the radius will be equal to 20 (maximal radius) for each sector.
#' # For the first one radius will be 1/4 * 20, 2/5 * 20 and 3/6 * 20
#' 
#' leaflet() %>% addPolarCharts(c(0,30), c(0, 0), mydata) 
#' 
#' # Common scale for the three columns
#' 
#' leaflet() %>% addPolarCharts(c(0,30), c(0, 0), mydata, maxValue = 0)
#' 
#' # Explicitely provide the maximal value. This is useful for comparison to have
#' # the same scale on different maps.
#' 
#' leaflet() %>% addPolarCharts(c(0,30), c(0, 0), mydata, maxValue = 9)
#' 
#' @export
addPolarCharts <- function(map, lng, lat, data, size = 20, opacity = 1,
                          scale = c("radius", "area"), maxValue = NULL, 
                          colors = NULL, popup = NULL, layerId = NULL) {
 
  scale <- match.arg(scale)
  data <- abs(data)
  
  options <- .prepareOptions(
    required = list(lng = lng, lat = lat), 
    optional = list(radius = size, opacity = opacity, maxValue = 1, 
                    layerId = layerId, popup = popup)
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

#' @rdname addPolarCharts
#' @export
updatePolarCharts <- function(map, layerId, data = NULL, size = 20, opacity = 1,
                             scale = c("radius", "area"), maxValue = NULL, popup = NULL) {
  scale <- match.arg(scale)
  if (!is.null(data)) data <- abs(data)
  
  options <- .prepareOptions(
    required = list(layerId = layerId),
    optional = list(radius = size, opacity = opacity, maxValue = 1, popup = popup)
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

#' Add or update bar charts
#' 
#' These functions add and update bar charts on a leaflet map. 
#' 
#' @param lng
#'   longitude of the bar charts
#' @param lat
#'   lattitude of the bar charts
#' @param data
#'   numeric matrix. Number of rows must equal the number of bar charts. The
#'   number of columns is equal to the number of bars in each bar chart.
#' @param size
#'   maximal width and height of the bar charts
#' @param opacity
#'   Opacity of a bar chart
#' @param colors
#'   Vector of colors for each bar of the bar charts
#' @inheritParams addDirectedSegments
#' @inheritParams addPolarCharts
#' 
#' @return 
#' The modified map object.
#' 
#' @examples 
#' 
#' require(leaflet)
#' mydata <- rbind(1:3, 4:6)
#' 
#' # Bar chart with distinct scales for each column of mydata. For the second
#' # bar chart the height will be equal to 20 (maximal size) for each bar.
#' # For the first one height will be 1/4 * 20, 2/5 * 20 and 3/6 * 20
#' 
#' leaflet() %>% addBarChart(c(0,30), c(0, 0), mydata) 
#' 
#' # Common scale for the three columns
#' 
#' leaflet() %>% addBarCharts(c(0,30), c(0, 0), mydata, maxValue = 0)
#' 
#' # Explicitely provide the maximal value. This is useful for comparison to have
#' # the same scale on different maps.
#' 
#' leaflet() %>% addBarCharts(c(0,30), c(0, 0), mydata, maxValue = 50)
#' 
#' @export
addBarCharts <- function(map, lng, lat, data, size = 30, opacity = 1,
                        minValue = NULL, maxValue = NULL, 
                        colors = NULL, popup = NULL, layerId = NULL) {
  
  # Data preparation
  if (max(length(lng), length(lat)) == 1) data <- matrix(data, nrow = 1)
  data <- as.matrix(data)
  
  if (is.null(maxValue)) {
    maxValue <- apply(data, 2, max)
  } else {
    if (length(maxValue) == 1 && maxValue == 0) maxValue <- max(data)
    maxValue <- rep_len(maxValue, ncol(data))
  }
  maxValue <- pmax(maxValue, 0)
  
  if (is.null(minValue)) {
    minValue <- apply(data, 2, min)
  } else {
    if (length(minValue) == 1 && minValue == -Inf) minValue <- min(data)
    minValue <- rep_len(minValue, ncol(data))
  }
  minValue <- pmin(minValue, 0)
  
  scaleCoef <- pmax(maxValue, abs(minValue))
  
  for (i in 1:ncol(data)) {
    data[, i] <- data[, i] / scaleCoef[i]
  }
  
  rangeValues <- range(c(maxValue / scaleCoef, minValue / scaleCoef))
  
  options <- .prepareOptions(
    required = list(lng = lng, lat = lat), 
    optional = list(size = size, opacity = opacity, 
                    minValue = rangeValues[1], maxValue = rangeValues[2], 
                    layerId = layerId, popup = popup)
  )
  
  map %>% requireDep(c("d3", "barChart")) %>% 
    invokeMethod(leaflet:::getMapData(map), "addBarCharts", options, data, colors)
}


#' @rdname addBarCharts
#' @export
updateBarCharts <- function(map, layerId, data = NULL, size = NULL, opacity = NULL,
                           minValue = NULL, maxValue = NULL, 
                           colors = NULL, popup = NULL) {
  
  if (!is.null(data)) {
    # Data preparation
    if (length(layerId) == 1) data <- matrix(data, nrow = 1)
    data <- as.matrix(data)
    
    if (is.null(maxValue)) {
      maxValue <- apply(data, 2, max)
    } else {
      if (length(maxValue) == 1 && maxValue == 0) maxValue <- max(data)
      maxValue <- rep_len(maxValue, ncol(data))
    }
    maxValue <- pmax(maxValue, 0)
    
    if (is.null(minValue)) {
      minValue <- apply(data, 2, min)
    } else {
      if (length(minValue) == 1 && minValue == -Inf) minValue <- min(data)
      minValue <- rep_len(minValue, ncol(data))
    }
    minValue <- pmin(minValue, 0)
    
    scaleCoef <- pmax(maxValue, abs(minValue))
    
    for (i in 1:ncol(data)) {
      data[, i] <- data[, i] / scaleCoef[i]
    }
    
    rangeValues <- range(c(maxValue / scaleCoef, minValue / scaleCoef))
    minValue <- rangeValues[1]
    maxValue <- rangeValues[2]
  } else {
    minValue <- NULL
    maxValue <- NULL
  }
  
  options <- .prepareOptions(
    required = list(layerId = layerId),
    optional = list(size = size, opacity = opacity, minValue = minValue, 
                    maxValue = maxValue, popup = popup)
  )
  
  invokeMethod(map, data = NULL, "updateBarCharts", options, data)
}

#' Add a shadow to map layers
#' 
#' This function adds a shadow to every svg element added to a leaflet map. It
#' can greatly improve the lisibility of the map.
#' 
#' @inheritParams addDirectedSegments
#' 
#' @return 
#' The modified map object
#' 
#' @examples 
#' 
#' leaflet() %>%
#'   addTiles() %>% 
#'   addDirectedSegments(0, 0, 1, 0, col= gray(0.9)) %>%
#'   addCircleMarkers(c(0, 1), c(0, 0), color = "white", fillOpacity = 1, stroke = FALSE) %>%
#'   addShadows()
#' 
#' @export
addShadows <- function(map) {
  map %>% requireDep("shadows") %>% invokeMethod(data = NULL, "addShadows")
}

#' Add legend to a map created with plotMap
#' 
#' @param map
#'   leaflet map
#' @param htmlAreaColor
#'   HTML of legend for area colors (character string)
#' @param htmlAreaSize
#'   HTML of the legend for area size
#' @param htmlLinkColor
#'   HTML of the legend for link colors
#' @param htmlLinkSize
#'   HTML of the legend for link width
#' @param onComplete
#'   Character vector containing Javascript code that must be executed once the
#'   html of the legend has been set.
#' 
#' @return Leaflet map
#' 
#' @noRd
addAntaresLegend <- function(map, htmlAreaColor = NULL, htmlAreaSize = NULL, 
                             htmlLinkColor = NULL, htmlLinkSize = NULL,
                             onComplete = "") {
  options <- list(
    htmlAreaColor = htmlAreaColor,
    htmlAreaSize = htmlAreaSize,
    htmlLinkColor = htmlLinkColor,
    htmlLinkSize = htmlLinkSize,
    onComplete = onComplete
  )
  
  map %>% requireDep("antaresLegend") %>% 
    invokeMethod(data=NULL, "addAntaresLegend", options)
}

#' Update legend of a map created with plotMap
#' 
#' @noRd
updateAntaresLegend <- function(map, htmlAreaColor = NULL, htmlAreaSize = NULL, 
                                htmlLinkColor = NULL, htmlLinkSize = NULL,
                                onComplete = NULL) {
  options <- list(
    htmlAreaColor = htmlAreaColor,
    htmlAreaSize = htmlAreaSize,
    htmlLinkColor = htmlLinkColor,
    htmlLinkSize = htmlLinkSize,
    onComplete = onComplete
  )
  
  # Remove null elements
  nullOpts <- sapply(options, is.null)
  options <- options[!nullOpts]
  
  map %>% requireDep("antaresLegend") %>% 
    invokeMethod(data=NULL, "updateAntaresLegend", options)
}