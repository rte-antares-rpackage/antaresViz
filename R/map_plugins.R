# Copyright © 2016 RTE Réseau de transport d’électricité

# This script contains functions that enhance the leaflet package. They can add
# new types of elements to a leaflet map and update elements already drawn in a
# map.
# These functions are quite hacky because for now, leaflet does not provide any
# mechanism to add custom functions. An issue has been created on github:
# https://github.com/rstudio/leaflet/issues/290

#' Add a shadow to map layers
#' 
#' This function adds a shadow to every svg element added to a leaflet map. It
#' can greatly improve the lisibility of the map.
#' 
#' @param map A leaflet map object.
#' 
#' @return 
#' The modified map object
#' 
#' @examples 
#' require(leaflet)
#' require(leaflet.minicharts)
#' 
#' leaflet() %>%
#'   addTiles() %>% 
#'   addFlows(0, 0, 1, 0, col= gray(0.9)) %>%
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
                             onComplete = "", display = "choose", language = "en") {
  options <- list(
    htmlAreaColor = htmlAreaColor,
    htmlAreaSize = htmlAreaSize,
    htmlLinkColor = htmlLinkColor,
    htmlLinkSize = htmlLinkSize,
    onComplete = onComplete,
    display = display,
    areas_name = .getLabelLanguage("Areas", language),
    links_names = .getLabelLanguage("Links", language),
    show_legend = .getLabelLanguage("Show legend", language),
    hide_legend = .getLabelLanguage("Hide legend", language)
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
