# Copyright © 2016 RTE Réseau de transport d’électricité

#' Graphical options for plotMap
#' 
#' These functions get and set options that control some graphical aspects 
#' of maps created with \code{\link{plotMap}}.
#' 
#' @param areaDefaultCol
#'   default color of areas.
#' @param areaDefaultSize
#'   default size of areas.
#' @param areaMaxSize
#'   maximal size of an area when it represents the value of some variable.
#' @param areaMaxHeight
#'   Maximal height of bars. Used only if a barchart representation is used.
#' @param areaChartColors
#'   Vector of colors to use in polar area charts and bar charts
#' @param areaColorScaleOpts
#'   List of options used to construct a continuous color scale. This list should
#'   be generated with function \code{colorScaleOptions}.
#' @param linkDefaultCol
#'   Default color of links.
#' @param linkDefaultSize
#'   Default line width of links.
#' @param linkMaxSize
#'   Maximal line width of a link when it represents the value of some variable.
#' @param linkColorScaleOpts
#'   List of options used to construct a continuous color scale. This list should
#'   be generated with function \code{colorScaleOptions}.
#' @param legend
#'   Should the legend be displayed or not ? Default is to mask the legend but
#'   add a button to display it. Other values are "visible" to make the legend
#'   always visible and "hidden" to mask it.
#' @param addTiles
#'   Should a base map be downloaded on the internet and displayed ?
#' @param polygons
#'   An object of class \code{SpatialPolygonsDataFrame} created by package 
#'   \code{sp} or related packages. These polygons will be displayed above the 
#'   base map (if any) and under the layer containing areas and links. It can
#'   be useful to create maps that do not require an internet connection to be
#'   correctly displayed or to display custom geographic area boundaries.
#' @param polygonOptions
#'   A list of options that are accepted by function 
#'   \code{\link[leaflet]{addPolygons}}. When parameter \code{polygons} is 
#'   provided, these options control how they are drawn on the map (color, 
#'   opacity, etc.).
#'   
#' @return 
#'   A list with the values of the different graphical parameters.
#'   
#' @examples 
#' \dontrun{
#' params <- plotMapOptions(areaDefaultCol = "red", linkDefaultCol = "orange")
#' plotMap(mydata, mylayout, options = params)
#' }
#' 
#' @export
plotMapOptions <- function(areaDefaultCol = "#CCCCCC", areaDefaultSize = 30, 
                           areaMaxSize = 50, areaMaxHeight = 50, areaChartColors = NULL,
                           areaColorScaleOpts = colorScaleOptions(),
                           linkDefaultCol = "#CCCCCC", linkDefaultSize = 3, 
                           linkMaxSize = 10, linkColorScaleOpts = colorScaleOptions(),
                           legend = c("choose", "visible", "hidden"),
                           addTiles = TRUE, background = "white", polygons = NULL,
                           polygonOptions = list(stroke = TRUE,
                                                 color = "#999",
                                                 weight = 0.5,
                                                 opacity = 1,
                                                 fillOpacity = 0.2,
                                                 options = list(clickable = FALSE))) {
  legend <- match.arg(legend)
  areaColorScaleOpts <- do.call(colorScaleOptions, areaColorScaleOpts)
  linkColorScaleOpts <- do.call(colorScaleOptions, linkColorScaleOpts)
  
  as.list(environment())
}

#' @param breaks
#'   Either a single number indicating the approximate number of colors to use, or
#'   a vector of values at which values to change color. 
#'   In the first case, the function tries to cut the data nicely, so the real 
#'   number of colors used may vary.
#' @param domain
#'   Range of the data, ie. the range of possible values. If \code{NULL}, the
#'   the range of the data is used
#' @param negCol
#'   color of the extreme negative value.
#' @param zeroCol
#'   color of the 0 value.
#' @param posCol
#'   Color of the extreme positive value.
#' @param naCol
#'   Color for missing values
#' @param zeroTol
#'   All values in the interval \code{\[-zeroTol, +zeroTol\]} are mapped to the
#'   \code{zeroCol} color. If \code{NULL}, the function tries to pick a nice
#'   value that is approximately equal to 1\% of the maximal value.
#'
#' @rdname plotMapOptions
#' @export
colorScaleOptions <- function(breaks = 5, domain = NULL,
                              negCol = "#FF0000", zeroCol = "#FFFFFF", posCol = "#0000FF",
                              naCol = "#EEEEEE", zeroTol = NULL) {
  as.list(environment())
}