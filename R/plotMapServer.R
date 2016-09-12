.plotMapServer <- function(x, mapLayout) {
  
  function(input, output, session) {
    # Initialization of the map
    output$map <- renderLeaflet({
      plot(mapLayout)
    })
    
    map <- leafletProxy("map", session)
    
    # Update circles and links when timeId is changed. We update both
    # so that areas stay above links
    observeEvent(input$linkVar, {
      .redrawLinks(x, mapLayout, input$linkVar, input$timeId, map)
      .redrawCircles(x, mapLayout, input$areaVar, input$timeId, map)
    })
    
    observeEvent(input$areaVar, {
      .redrawCircles(x, mapLayout, input$areaVar, input$timeId, map)
    })
    
    # Update circle colors when areavar is modified
    observeEvent(input$timeId, {
      if (input$linkVar != "none") {
        .redrawLinks(x, mapLayout, input$linkVar, input$timeId, map)
        .redrawCircles(x, mapLayout, input$areaVar, input$timeId, map)
      }
      if (input$areaVar != "none" & input$linkVar == "none") {
        .redrawCircles(x, mapLayout, input$areaVar, input$timeId, map)
      }
    })
    
    # Return a list with the last value of inputs
    observeEvent(input$done, {
      stopApp(list(
        t = input$timeId, 
        areaVar = input$areaVar, 
        linkVar = input$linkVar
      ))
    })
  }
}

.redrawCircles <- function(x, mapLayout, areaVar, t, map) {
  ml <- copy(mapLayout)
  
  if (areaVar != "none") {
    colAndPal <- .getColAndPal(x$areas, mapLayout$coords, areaVar, t, "area")
    ml$coords <- colAndPal$coords
    colAreas <- colAndPal$col
  } else {
    colAreas <- "#CCCCCC"
  }
  
  map <- map %>% 
    clearMarkers() %>% 
    addCircleMarkers(
      lng = ml$coords$x, lat = ml$coords$y, 
      color = gray(0.3), weight = 1, 
      fillColor = colAreas, fillOpacity = 1, 
      popup = ml$coords$area,
      options = list(zIndexOffset=1000)
    )
  
  if (areaVar != "none") {
    map <- addLegend(map, "topright", colAndPal$pal, ml$coords[[areaVar]], 
                     title = areaVar,
                     opacity = 1, layerId = "legAreas")
  }
  
  map
}

.redrawLinks <- function(x, mapLayout, linkVar, t, map) {
  ml <- copy(mapLayout)
  
  if (linkVar != "none") {
    colAndPal <- .getColAndPal(x$links, mapLayout$links, linkVar, t, "link")
    ml$links <- colAndPal$coords
    colLinks <- colAndPal$col
    dir <- colAndPal$dir
  } else {
    colLinks <- "#CCCCCC"
    dir <- 0
  }
  
  map <- map %>% updateDirectedSegments(layerId = ml$links$link, color = colLinks,
                                        dir = dir)
  
  if (linkVar != "none") {
    map <- addLegend(map, "topright", colAndPal$pal, ml$links[[linkVar]], 
                     title = linkVar,
                     opacity = 1, layerId = "legLinks")
  }
  
  map
}

.getColAndPal <- function(data, coords, var, t, mergeBy) {
  if ("FLOW LIN." %in% names(data)) {
    var <- union(var, "FLOW LIN.")
  }
  coords <- merge(
    copy(coords),
    data[timeId == t, c(mergeBy, var), with = FALSE],
    by = mergeBy
  )
  
  #setnames(coords, var[1], "var")
  
  rangevar <- range(data[[var[1]]])
  if (rangevar[1] >= 0) {
    domain <- rangevar
    pal <- colorBin("Blues", domain, bins = 5)
  } else {
    domain <- c(-max(abs(rangevar)), max(abs(rangevar)))
    pal <- colorBin("RdBu", domain, bins = 7)
  }
  
  col <- pal(coords[[var[1]]])
  
  res <- list(coords = coords, col = col, pal = pal)
  
  if ("FLOW LIN." %in% names(coords)) {
    res$dir <- sign(coords$`FLOW LIN.`)
  } else {
    res$dir <- 0
  }
  
  res
}