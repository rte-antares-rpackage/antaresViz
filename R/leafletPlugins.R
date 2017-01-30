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

#' Add or update directed segments
#' 
#' These functions add or update directed segments on a leaflet map: they are 
#' simply lines with an arrow in their middle that represent their direction.
#' 
#' @param map
#'   A map object created with \code{\link[leaflet]{leaflet}()}
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
    invokeMethod(data = NULL, "addDirectedSegments", options)
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
  
  invokeMethod(map, data = NULL, "updateDirectedSegments", options)
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
#' if (require(leaflet)) {
#'   leaflet() %>%
#'     addTiles() %>% 
#'     addDirectedSegments(0, 0, 1, 0, col= gray(0.9)) %>%
#'     addCircleMarkers(c(0, 1), c(0, 0), color = "white", fillOpacity = 1, stroke = FALSE) %>%
#'     addShadows()
#' }
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
                             onComplete = "", display = "choose") {
  options <- list(
    htmlAreaColor = htmlAreaColor,
    htmlAreaSize = htmlAreaSize,
    htmlLinkColor = htmlLinkColor,
    htmlLinkSize = htmlLinkSize,
    onComplete = onComplete,
    display = display
  )
  
  map %>% requireDep("antaresLegend") %>% 
    invokeMethod(data=NULL, "addAntaresLegend", options)
}

#' Update legend of a map created with plotMap
#' 
#' @noRd
updateAntaresLegend <- function(map, htmlAreaColor = NULL, htmlAreaSize = NULL, 
                                htmlLinkColor = NULL, htmlLinkSize = NULL,
                                onComplete = NULL, display = NULL) {
  options <- list(
    htmlAreaColor = htmlAreaColor,
    htmlAreaSize = htmlAreaSize,
    htmlLinkColor = htmlLinkColor,
    htmlLinkSize = htmlLinkSize,
    onComplete = onComplete,
    display = display
  )
  
  # Remove null elements
  nullOpts <- sapply(options, is.null)
  options <- options[!nullOpts]
  
  map %>% requireDep("antaresLegend") %>% 
    invokeMethod(data=NULL, "updateAntaresLegend", options)
}

addTimeLabel <- function(map, timeId = NULL, timeStep, opts) {
  time <- .timeIdToDate(timeId, timeStep, opts) %>% 
    as.POSIXct(tz = "UTC") %>% 
    as.numeric()
  
  options <- list(
    time = time,
    timeStep = timeStep
  )
  
  map %>% requireDep("timeLabel") %>% 
    invokeMethod(data=NULL, "addTimeLabel", options)
}

#' Update legend of a map created with plotMap
#' 
#' @noRd
updateTimeLabel <- function(map, timeId = NULL, timeStep, opts) {
  if (is.null(timeId)) return(map)
  if (timeStep == "none") {
    time <- timeId
  } else {
    time <- .timeIdToDate(timeId, timeStep, opts) %>% 
      as.POSIXct() %>% 
      as.numeric() 
  }
  
  options <- list(
    time = time,
    timeStep = timeStep
  )
  
  # Remove null elements
  nullOpts <- sapply(options, is.null)
  options <- options[!nullOpts]
  
  map %>% requireDep("timeLabel") %>% 
    invokeMethod(data=NULL, "updateTimeLabel", options)
}