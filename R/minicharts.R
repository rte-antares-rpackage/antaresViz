#' Add or update charts on a leaflet map
#' 
#' these functions add or update charts in a leaflet map at given coordinates:
#' they can be bar charts, pie charts or polar charts where data is encoded
#' either by area or by radius.
#' 
#' @param map A leaflet map object created with \code{\link[leaflet]{leaflet}}.
#' @param lng Longitude where to place the charts.
#' @param lat Lattitude where to place the charts.
#' @param data A numeric matrix with number of rows equal to the number of 
#'   elements in \code{lon} or \code{lat} and numbe rof column equal to the 
#'   number of variables to represent.
#' @param maxValues maximal absolute values of the variables to the represent. 
#'   It can be a vector with one value per column of \code{data} or a single 
#'   value. Using a single value enforces charts to use a unique scale for all 
#'   variables. If it is \code{NULL}, the maximum value of \code{data} is used.
#' @param type Type of chart. Possible values are \code{"bar"} for bar charts, 
#'   \code{"pie"} for pie charts, \code{"polar-area"} and \code{"polar-radius"} 
#'   for polar area charts where the values are represented respectively by the 
#'   area or the radius of the slices. Finally it can be equal to \code{"auto"},
#'   the default. In this case, if there is only one variable to represent, the 
#'   chart will be a single circle, else it is a barchart.
#' @param fillColor Used only if data contains only one column. It is the color 
#'   used to fill the circles.
#' @param colorPalette Color palette to use when \code{data} contains more than 
#'   one column.
#' @param width maximal width of the created elements.
#' @param height maximal height of the created elements.
#' @param opacity Opacity of the chart.
#' @param showLabels Should values be displayed above chart elements.
#' @param labelStyle Character string containing CSS properties to apply to the
#'   labels.
#' @param labelPrecision Integer indicating the number of signficant digits to
#'   keep when displaying numerci values
#' @param labelText character vector containing the text content of the charts. 
#'   Used only if \code{data} contains only one column.
#' @param transitionTime duration of the transitions when a property of a chart 
#'   is updated
#' @param popup Content of the popup bind to a given chart. This can be html
#'   text.
#' @param layerId An ID variable. It is mandatoy when one wants to update some
#'   chart.
#' 
#' @return 
#' The modified leaflet map object.
#' 
#' @examples 
#' require(leaflet)
#' mymap <- leaflet() %>% addTiles() %>% addMinicharts(0, 0, data = 1:3, layerId = "c1") 
#' 
#' mymap
#' mymap %>% updateMinicharts("c1", maxValues = 6)
#' mymap %>% updateMinicharts("c1", type="pie")
#' 
#' @export
#' 
addMinicharts <- function(map, lng, lat, data = 1, maxValues = NULL, type = "auto", 
                        fillColor = NULL, colorPalette = NULL,
                        width = 30, height = 30, opacity = 1, showLabels = FALSE,
                        labelStyle = NULL, 
                        labelPrecision = 0, labelText = NULL,
                        transitionTime = 750, popup = NULL, layerId = NULL) {
  
  type <- match.arg(type, c("auto", "bar", "pie", "polar-area", "polar-radius"))
  
  # Data preparation
  
  # When adding only one minichart, data can be a vector or a data frame, so it 
  # needs to be converted to a matrix with correct lines and columns
  if (max(length(lng), length(lat)) == 1) {
    data <- matrix(data, nrow = 1)
  } else {
    if (is.vector(data)) {
      data <- matrix(data, ncol = 1, nrow = max(length(lng), length(lat)))
    }
  }
  
  data <- unname(as.matrix(data))
  
  # If maxValues is not set explicitely, we use the maximal observed value
  if (is.null(maxValues)) maxValues <- max(abs(data))
  
  # If there is only one variable in data, we draw circles with different radius
  # else we draw bar charts by default.
  if (type == "auto") {
    type <- ifelse (ncol(data) == 1, "polar-area", "bar")
  }
  
  options <- .prepareOptions(
    required = list(lng = lng, lat = lat), 
    optional = list(type = type, width = width, height = height, 
                    opacity = opacity, labels = ifelse(showLabels, "auto", "none"), 
                    labelStyle = labelStyle, labelPrecision = labelPrecision,
                    labelText = labelText, transitionTime = transitionTime,
                    popup = popup, layerId = layerId, fillColor = fillColor)
  )
  
  map %>% requireDep(c("minichart", "minichart_bindings")) %>% 
    invokeMethod(NULL, "addMinicharts", 
                 options, data, unname(maxValues), colorPalette)
}

#' @export
#' @rdname addMinicharts
updateMinicharts <- function(map, layerId, data = NULL, maxValues = NULL, type = NULL, 
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
    
    data <- unname(as.matrix(data))
    
    if (type == "auto") {
      type <- ifelse (ncol(data) == 1, "polar-area", "bar")
    }
  } else {
    type <- NULL
  }
  
  options <- .prepareOptions(
    required = list(layerId = layerId), 
    optional = list(type = type, width = width, height = height, 
                    opacity = opacity, labels = ifelse(showLabels, "auto", "none"), 
                    labelStyle = labelStyle, labelPrecision = labelPrecision,
                    labelText = labelText, transitionTime = transitionTime,
                    popup = popup, fillColor = fillColor)
  )
  
  map %>% requireDep(c("minichart", "minichart_bindings")) %>% 
    invokeMethod(NULL, "updateMinicharts", 
                 options, data, unname(maxValues), colorPalette)
  
}