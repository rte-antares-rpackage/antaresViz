#Copyright © 2016 RTE Réseau de transport d’électricité

#' Private function used by plotMap as the server function of the shiny gadget
#' it launches.
#' It takes as parameter a map and it updates the visual attributes of its
#' elements (color and size of areas and links) when the user changes the value
#' of an input.
#' 
#' @param x
#'   Object of class antaresDataTable containing areas and links
#' @param mapLayout
#'   Object created with function mapLayout
#' @param initialMap
#'   Initial map
#' @param areaChartType
#'   "bar" or "polar"
#' @param options
#' 
#' @return 
#' A shiny server function.
#'
#' @noRd
.plotMapServer <- function(x, mapLayout, initialMap, areaChartType, options) {
  
  function(input, output, session) {
    # Initialization of the map
    output$map <- renderLeaflet(initialMap)
    
    map <- leafletProxy("map", session)

    observe({
      .redrawLinks(map, x, mapLayout, input$timeId, input$colLinkVar, 
                   input$sizeLinkVar, options)
      .redrawCircles(map, x, mapLayout, input$timeId, input$colAreaVar, 
                     input$sizeAreaVars, areaChartType, options)
    })
    
    # Return a list with the last value of inputs
    observeEvent(input$done, {
      stopApp(list(
        t = input$timeId, 
        colAreaVar = input$colAreaVar, 
        sizeAreaVars = input$sizeAreaVars,
        colLinkVar = input$colLinkVar,
        sizeLinkVar = input$sizeLinkVar
      ))
    })
  }
}
