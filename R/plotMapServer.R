.plotMapServer <- function(x, mapLayout, initialMap, defaults) {
  
  function(input, output, session) {
    # Initialization of the map
    output$map <- renderLeaflet(initialMap)
    
    map <- leafletProxy("map", session)

    observe({
      .redrawLinks(map, x, mapLayout, input$timeId, input$colLinkVar, input$sizeLinkVar, defaults)
      .redrawCircles(map, x, mapLayout, input$timeId, input$colAreaVar, input$sizeAreaVars, defaults)
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

.redrawCircles <- function(map, x, mapLayout, t, colAreaVar, sizeAreaVars, defaults) {
  ml <- copy(mapLayout)
  
  optsArea <- .getColAndSize(x$areas, mapLayout$coords, "area", t,
                              colAreaVar, sizeAreaVars)
  ml$coords <- optsArea$coords
  
  if (is.null(optsArea$color)) optsArea$color <- defaults$colArea
  
  if (is.null(optsArea$size)) optsArea$size <- defaults$sizeArea
  else if (ncol(optsArea$size) == 1) {
    optsArea$size <- sqrt(optsArea$size) * defaults$maxSizeArea
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

.redrawLinks <- function(map, x, mapLayout, t, colLinkVar, sizeLinkVar, defaults) {
  ml <- copy(mapLayout)
  
  optsLink <- .getColAndSize(x$links, mapLayout$links, "link", t,
                             colLinkVar, sizeLinkVar)
  
  if (is.null(optsLink$color)) optsLink$color <- defaults$colLink
  if (is.null(optsLink$size)) optsLink$size <- defaults$sizeLink
  else optsLink$size <- optsLink$size * defaults$maxSizeLink
  
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