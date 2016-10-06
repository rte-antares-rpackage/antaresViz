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
plotMapOptions <- function(colArea = "#CCCCCC", sizeArea = 15, maxSizeArea = 15,
                           colLink = "#CCCCCC", sizeLink = 3, maxSizeLink = 10) {
  list(
    colArea = colArea,
    sizeArea = sizeArea, 
    maxSizeArea = maxSizeArea,
    colLink = colLink,
    sizeLink = sizeLink,
    maxSizeLink = maxSizeLink
  )
}