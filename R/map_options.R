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
#' @param labelMinSize minimal height of labels.
#' @param labelMaxSize maximal height of labels.
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
#' @param tilesURL URL template used to get map tiles. The followign site 
#'   provides some URLs;
#'   \url{https://leaflet-extras.github.io/leaflet-providers/preview/}
#' @param preprocess A function that takes as argument a map and that returns a
#'   modified version of this map. This parameter can be used to add extra
#'   information on a map.
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
plotMapOptions <- function(areaDefaultCol = "#DDDDE5", areaDefaultSize = 30, 
                           areaMaxSize = 50, areaMaxHeight = 50, areaChartColors = NULL,
                           areaColorScaleOpts = colorScaleOptions(),
                           labelMinSize = 8, labelMaxSize = 24,
                           linkDefaultCol = "#BEBECE", linkDefaultSize = 3, 
                           linkMaxSize = 15, linkColorScaleOpts = colorScaleOptions(),
                           legend = c("choose", "visible", "hidden"),
                           tilesURL = defaultTilesURL(),
                           preprocess = function(map) {map}) {
  legend <- match.arg(legend)
  areaColorScaleOpts <- do.call(colorScaleOptions, areaColorScaleOpts)
  linkColorScaleOpts <- do.call(colorScaleOptions, linkColorScaleOpts)
  
  as.list(environment())
}

#' @export
#' @rdname plotMapOptions
defaultTilesURL <- function() {
  "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
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
#' @param colors
#'   Vector of colors. If it is set and if user manually sets break points, then
#'   these colors are used instead of the colors defined by parameters negCol,
#'   zeroCol and posCol.
#' @param levels
#'   Vector of the distinct values a variable can take. Only used when the 
#'   variable to represent is a categorical variable. 
#'  
#' @rdname plotMapOptions
#' @export
colorScaleOptions <- function(breaks = 5, domain = NULL,
                              negCol = "#FF0000", zeroCol = "#FAFAFA", posCol = "#0000FF",
                              naCol = "#EEEEEE", zeroTol = NULL, colors = NULL, levels = NULL) {
  as.list(environment())
}