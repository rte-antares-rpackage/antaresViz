#Copyright © 2016 RTE Réseau de transport d’électricité

#' Graphical options for plotMap
#' 
#' This function gets and sets options that control some graphical aspects 
#' of maps created with \code{\link{plotMap}}
#' 
#' @param colArea
#'   default color of areas.
#' @param sizeArea
#'   default size of areas.
#' @param maxSizeArea
#'   maximal size of an area when it represents the value of some variable.
#' @param colLink
#'   Default color of links.
#' @param sizeLink
#'   Default line width of links.
#' @param maxSizeLink
#'   Maximal line width of a link when it represents the value of some variable.
#'   
#' @return 
#'   A list with the values of the different graphical parameters.
#'   
#' @examples 
#' \dontrun{
#' params <- plotMapOptions(colArea = "red", colLink = "orange")
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

#' @export
colorScaleOptions <- function(n = 5, domain = NULL,
                              negCol = "#FF0000", zeroCol = "#FFFFFF", posCol = "#0000FF",
                              naCol = "#EEEEEE", zeroTol = NULL) {
  as.list(environment())
}