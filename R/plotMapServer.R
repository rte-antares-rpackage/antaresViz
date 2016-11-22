# Copyright © 2016 RTE Réseau de transport d’électricité

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
.plotMapServer <- function(x, mapLayout, initialMap, options, sizeAreaVars) {
  
  function(input, output, session) {
    # Initialization of the map
    output$map <- renderLeaflet(initialMap)
    
    # selectInput changes the order of the selected choices. This line prevents this
    updateSelectInput(session, "sizeAreaVars", selected = sizeAreaVars)
    
    map <- leafletProxy("map", session)

    observe({
      .redrawLinks(map, x, mapLayout, input$timeId, input$colLinkVar, 
                   input$sizeLinkVar, input$popupLinkVars, options)
      .redrawCircles(map, x, mapLayout, input$timeId, input$colAreaVar, 
                     input$sizeAreaVars, input$popupAreaVars, input$uniqueScale, options)
      updateTimeLabel(map, input$timeId, attr(x, "timeStep"), simOptions(x))
    })
    
    # Return a list with the last value of inputs
    observeEvent(input$done, {
      stopApp(list(
        t = input$timeId, 
        colAreaVar = input$colAreaVar, 
        sizeAreaVars = input$sizeAreaVars,
        popupAreaVars = input$popupAreaVars,
        colLinkVar = input$colLinkVar,
        sizeLinkVar = input$sizeLinkVar,
        popupLinkVars = input$popupLinkVars
      ))
    })
  }
}
