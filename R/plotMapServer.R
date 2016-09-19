.plotMapServer <- function(x, mapLayout, initialMap) {
  
  function(input, output, session) {
    # Initialization of the map
    output$map <- renderLeaflet(initialMap)
    
    map <- leafletProxy("map", session)

    observe({
      .redrawLinks(map, x, mapLayout, input$timeId, input$colLinkVar, input$sizeLinkVar)
      .redrawCircles(map, x, mapLayout, input$timeId, input$colAreaVar, input$sizeAreaVars)
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

.redrawCircles <- function(map, x, mapLayout, t, colAreaVar, sizeAreaVars) {
  ml <- copy(mapLayout)
  
  opts <- .getColAndSize(x$areas, mapLayout$coords, "area", t,
                              colAreaVar, sizeAreaVars)
  ml$coords <- opts$coords
  
  if (is.matrix(opts$size) && ncol(opts$size) > 1) {
    map <- map %>% 
      updateCircleMarkers(opts$coords$area, opacity = 0, fillOpacity = 0) %>% 
      updatePolarChart(opts$coords$area, opacity = 1, data = opts$size)
  } else {
    map <- map %>% 
      updateCircleMarkers(opts$coords$area, fillColor = opts$color, 
                          radius = sqrt(opts$size) * 15, 
                          opacity = 1, fillOpacity = 1) %>% 
      updatePolarChart(opts$coords$area, opacity = 0)
  }
  
  if (!is.null(opts$pal)) {
    map <- addLegend(map, "topright", opts$pal, opts$coords[[colAreaVar]], 
                     title = colAreaVar,
                     opacity = 1, layerId = "legAreas")
  } else {
    map <- removeControl(map, "legAreas")
  }
  
  map
}

.redrawLinks <- function(map, x, mapLayout, t, colLinkVar, sizeLinkVar) {
  ml <- copy(mapLayout)
  
  opts <- .getColAndSize(x$links, mapLayout$links, "link", t,
                         colLinkVar, sizeLinkVar)
  
  map <- map %>% updateDirectedSegments(layerId = ml$links$link, 
                                        color = opts$color,
                                        weight = opts$size * 10,
                                        dir = opts$dir)
  
  if (!is.null(opts$pal)) {
    map <- addLegend(map, "topright", opts$pal, opts$coords[[colLinkVar]], 
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
  res <- list(coords = coords, color = "#CCCCCC", size = 1, dir = 0)
  
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