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
#' @param options
#' 
#' @return 
#' A shiny server function.
#'
#' @noRd
.plotMapServer <- function(x, mapLayout, initialMap, options) {
  
  function(input, output, session) {
    # Initialization of the map
    output$map <- renderLeaflet(initialMap)
    
    map <- leafletProxy("map", session)

    observe({
      # These functions are defined below.
      .redrawLinks(map, x, mapLayout, input$timeId, input$colLinkVar, input$sizeLinkVar, options)
      .redrawCircles(map, x, mapLayout, input$timeId, input$colAreaVar, input$sizeAreaVars, options)
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

# Update the circles or polar charts representing areas
.redrawCircles <- function(map, x, mapLayout, t, colAreaVar, sizeAreaVars, options) {
  ml <- copy(mapLayout)
  
  optsArea <- .getColAndSize(x$areas, mapLayout$coords, "area", t,
                              colAreaVar, sizeAreaVars)
  ml$coords <- optsArea$coords
  
  if (is.null(optsArea$color)) optsArea$color <- options$colArea
  
  if (is.null(optsArea$size)) optsArea$size <- options$sizeArea
  else if (ncol(optsArea$size) == 1) {
    optsArea$size <- sqrt(optsArea$size) * options$maxSizeArea
  }
  
  if (is.matrix(optsArea$size) && ncol(optsArea$size) > 1) {
    map <- map %>% 
      updateCircleMarkers(optsArea$coords$area, opacity = 0, fillOpacity = 0) %>% 
      updatePolarChart(optsArea$coords$area, opacity = 1, data = optsArea$size)
  } else {
    map <- map %>% 
      updateCircleMarkers(optsArea$coords$area, fillColor = optsArea$color, 
                          radius = optsArea$size, 
                          opacity = 1, fillOpacity = 1) %>% 
      updatePolarChart(optsArea$coords$area, opacity = 0)
  }
  
  if (!is.null(optsArea$pal)) {
    map <- addLegend(map, "topright", optsArea$pal, optsArea$coords[[colAreaVar]], 
                     title = colAreaVar,
                     opacity = 1, layerId = "legAreas")
  } else {
    map <- removeControl(map, "legAreas")
  }
  
  map
}

# Update the links in the map
.redrawLinks <- function(map, x, mapLayout, t, colLinkVar, sizeLinkVar, options) {
  ml <- copy(mapLayout)
  
  optsLink <- .getColAndSize(x$links, mapLayout$links, "link", t,
                             colLinkVar, sizeLinkVar)
  
  if (is.null(optsLink$color)) optsLink$color <- options$colLink
  if (is.null(optsLink$size)) optsLink$size <- options$sizeLink
  else optsLink$size <- optsLink$size * options$maxSizeLink
  
  map <- map %>% updateDirectedSegments(layerId = ml$links$link, 
                                        color = optsLink$color,
                                        weight = optsLink$size,
                                        dir = optsLink$dir)
  
  if (!is.null(optsLink$pal)) {
    map <- addLegend(map, "topright", optsLink$pal, optsLink$coords[[colLinkVar]], 
                     title = colLinkVar,
                     opacity = 1, layerId = "legLinks")
  } else {
    map <- removeControl(map, "legLinks")
  }
  
  map
}

#' Private function that binds data with map layout and returns colors and sizes
#' for each element
#' 
#' @param data
#'   antaresDataTable containing data for areas or links
#' @param coords
#'   element of a map layout corresponding to data (coordinates of areas or links)
#' @param mergeBy
#'   name of the variable to merge data and coords by ("area" or "link")
#' @param t
#'   timeStep
#' @param colVar
#'   variable to map with colors. "none" for no mapping
#' @param sizeVar
#'   variables to map with sizes. "none", NULL or c() for no mapping
#' 
#' @return 
#' A list with the following elements:
#' * coords : coords augmented with data
#' * dir    : direction of the links (if relevant)
#' * color  : color of each element
#' * pal    : color palette used
#' * size   : size of elements.
#' 
#' @noRd
.getColAndSize <- function(data, coords, mergeBy, t, colVar, sizeVar) {

  coords <- merge(coords, data[timeId == t], by = mergeBy)
  
  # Initialize the object returned by the function
  res <- list(coords = coords, dir = 0)
  
  # color
  if (colVar != "none" & length(sizeVar) <= 1) {
    rangevar <- range(data[[colVar]])
    if (rangevar[1] >= 0) {
      domain <- rangevar
      res$pal <- colorBin("Blues", domain, bins = 5)
    } else {
      domain <- c(-max(abs(rangevar)), max(abs(rangevar)))
      res$pal <- colorBin("RdBu", domain, bins = 7)
    }
    
    res$color <- res$pal(coords[[colVar]])
  }
  
  # size
  if (length(sizeVar) > 0 && !("none" %in% sizeVar)) {
    res$size <- abs(as.matrix(coords[, sizeVar, with = FALSE]))
    if (length(sizeVar) == 1) res$size <- res$size / max(res$size)
  }
  
  # Direction
  if ("FLOW LIN." %in% names(coords)) {
    res$dir <- sign(coords$`FLOW LIN.`)
  } else {
    res$dir <- 0
  }
  
  res
}