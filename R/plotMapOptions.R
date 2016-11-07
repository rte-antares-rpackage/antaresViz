#Copyright © 2016 RTE Réseau de transport d’électricité

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
#' @param areaChartColors
#'   Vector of colors to use in polar area charts and bar charts
#' @param areaChartUniqueScale
#'   If the map contains polar or bar charts, should the different variables 
#'   represented use the same scale or should each variable have its own scale ?
#'   This parameter should be TRUE only if the variables have the same unit and 
#'   are comparable : for instance production variables.  
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
plotMapOptions <- function(areaDefaultCol = "#CCCCCC", areaDefaultSize = 15, 
                           areaMaxSize = 15, areaChartColors = NULL,
                           areaChartUniqueScale = FALSE,
                           areaColorScaleOpts = colorScaleOptions(),
                           linkDefaultCol = "#CCCCCC", linkDefaultSize = 3, 
                           linkMaxSize = 10, linkColorScaleOpts = colorScaleOptions()) {
  as.list(environment())
}

#' @param n
#'   Approximate number of colors to use. The function tries to cut the data 
#'   nicely, so the real number of colors used may vary.
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
colorScaleOptions <- function(n = 5, domain = NULL,
                              negCol = "#FF0000", zeroCol = "#FFFFFF", posCol = "#0000FF",
                              naCol = "#EEEEEE", zeroTol = NULL) {
  as.list(environment())
}