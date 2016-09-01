.plotMapServer <- function(x, mapLayout) {
  # Keep only links and areas present in the data
  areaList <- unique(x$areas$area)
  linkList <- unique(x$links$link)
  mapLayout$coords <- mapLayout$coords[area %in% areaList]
  mapLayout$links <- mapLayout$links[link %in% linkList]
  
  
  function(input, output, session) {
    # Initialization of the map
    output$map <- renderLeaflet({
      leaflet() %>% 
        addTiles(urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}")
    })
    
    map <- leafletProxy("map", session)
    
    # Update circle colors when areavar is modified
    observeEvent(input$linkVar, {
      .redrawLinks(x, mapLayout, input$linkVar, input$timeId, map)
      .redrawCircles(x, mapLayout, input$areaVar, input$timeId, map)
    })
    
    observeEvent(input$areaVar, {
      .redrawCircles(x, mapLayout, input$areaVar, input$timeId, map)
    })
    
    # Update circles and links when timeId is changed
    observeEvent(input$timeId, {
      if (input$linkVar != "none") {
        .redrawLinks(x, mapLayout, input$linkVar, input$timeId, map)
        .redrawCircles(x, mapLayout, input$areaVar, input$timeId, map)
      }
      if (input$areaVar != "none" & input$linkVar == "none") {
        .redrawCircles(x, mapLayout, input$areaVar, input$timeId, map)
      }
    })
    
    observeEvent(input$done, {
      stopApp("glop glop")
    })
  }
}

.redrawCircles <- function(x, mapLayout, areaVar, t, map) {
  ml <- copy(mapLayout)
  
  if (areaVar != "none") {
    ml$coords <- merge(
      ml$coords, 
      x$areas[timeId == t, c("area", areaVar), with = FALSE], 
      by = "area"
    )
    
    setnames(ml$coords, areaVar, "var")
    
    rangevar <- range(x$areas[[areaVar]])
    if (rangevar[1] >= 0) {
      domain <- rangevar
      areaPal <- colorBin("Blues", domain, bins = 5)
    } else {
      domain <- c(-max(abs(rangevar)), max(abs(rangevar)))
      areaPal <- colorBin("RdBu", domain, bins = 7)
    }
    
    colAreas <- areaPal(ml$coords$var)
    
  } else {
    colAreas <- "#CCCCCC"
  }
  
  map %>% 
    clearMarkers() %>% 
    addCircleMarkers(
      lng = ml$coords$x, lat = ml$coords$y, 
      color = gray(0.3), weight = 1, 
      fillColor = colAreas, fillOpacity = 1, 
      popup = ml$coords$area,
      options = list(zIndexOffset=1000)
    )
}

.redrawLinks <- function(x, mapLayout, linkVar, t, map) {
  ml <- copy(mapLayout)
  
  if (linkVar != "none") {
    ml$links <- merge(
      ml$links, 
      x$links[timeId == t, c("link", linkVar), with = FALSE], 
      by = "link"
    )
    
    setnames(ml$links, linkVar, "var")
    
    rangevar <- range(x$links[[linkVar]])
    if (rangevar[1] >= 0) {
      domain <- rangevar
      linkPal <- colorBin("Blues", domain, bins = 5)
    } else {
      domain <- c(-max(abs(rangevar)), max(abs(rangevar)))
      linkPal <- colorBin("RdBu", domain, bins = 7)
    }
    
    colLinks <- linkPal(ml$links$var)
    
  } else {
    colLinks <- "#CCCCCC"
  }
  
  links <- lapply(1:nrow(ml$links), function(i) {
    l <- ml$links[i, ]
    sp::Lines(list(sp::Line(matrix(c(l$x0, l$x1, l$y0, l$y1), ncol = 2))), ID = i)
  })
  
  links <- sp::SpatialLines(links)
  
  map %>% 
    clearShapes() %>% 
    addPolylines(
      data = links, 
      popup = x$links$link, 
      color = colLinks, 
      opacity = 1, 
      group = "links"
    )
}