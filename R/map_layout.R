# Copyright © 2016 RTE Réseau de transport d’électricité

#' Place areas of a study on a map
#' 
#' This function launches an interactive application that let the user place 
#' areas of a study on a map. the GPS coordinates of the areas are then returned
#' and can be used in functions. This function should be used only once per 
#' study. The result should then be saved in an external file and be reused.
#' 
#' @param layout
#'   object returned by function \code{\link[antaresRead]{readLayout}}
#' @param what
#'   Either "areas" or "districts". Indicates what type of object to place
#'   on the map.
#' @param map
#'   An optional \code{\link[sp]{SpatialPolygons}} or 
#'   \code{\link[sp]{SpatialPolygonsDataFrame}} object.
#'   
#' @return 
#' An object of class \code{mapLayout}.
#' 
#' @examples 
#' \dontrun{
#' # Read the coordinates of the areas in the Antares interface, then convert it
#' # in a map layout.
#' layout <- readLayout()
#' ml <- mapLayout(layout)
#' 
#' # Save the result for future use
#' save(ml, file = "ml.rda")
#' }
#' 
#' @export
#' 
mapLayout <- function(layout, what = c("areas", "districts"), map = NULL) {
  what <- match.arg(what)
  
  if (what == "areas") {
    coords <- copy(layout$areas)
    info <- coords$area
    links <- copy(layout$links)
  } else {
    coords <- copy(layout$districts)
    info <- coords$district
    links <- copy(layout$districtLinks)
  }
  
  links$x0 <- as.numeric(links$x0)
  links$x1 <- as.numeric(links$x1)
  links$y0 <- as.numeric(links$y0)
  links$y1 <- as.numeric(links$y1)
  
  mapCoords <- changeCoords(coords$x, coords$y, coords$color, info, map)
  coords$x <- sp::coordinates(mapCoords)[, 1]
  coords$y <- sp::coordinates(mapCoords)[, 2]
  
  if (what == "areas") {
    links[coords, `:=`(x0 = x, y0 = y),on=c(from = "area")]
    links[coords, `:=`(x1 = x, y1 = y),on=c(to = "area")]
  } else {
    links[coords, `:=`(x0 = x, y0 = y),on=c(fromDistrict = "district")]
    links[coords, `:=`(x1 = x, y1 = y),on=c(toDistrict = "district")]
  }
  
  if (!is.null(map)) {
    coords$geoAreaId <- mapCoords$geoAreaId
    coords <- coords[!is.na(coords$geoAreaId),]
    
    map <- map[coords$geoAreaId,]
  }
  
  res <- list(coords = coords, links = links, map = map)
  class(res) <- "mapLayout"
  attr(res, "type") <- what
  res
}

#' Modify coordinates interactively
#'
#' This function helps to correct the coordinates of a set of spatial  points by
#' creating an interactive map. Moreover, the function can be used to visually
#' associate points with polygons from a \code{\link[sp]{SpatialPolygons}} or
#' \code{\link[sp]{SpatialPolygonsDataFrame}} object.
#'
#' @param lon
#'   Longitude of the points (x-axis)
#' @param lat
#'   Latitude of the points (y-axis)
#' @param col
#'   Vector of colors
#' @param info
#'   A character vector that is displayed when one clicks on a marker. This helps
#'   identify the points on the map.
#' @param map
#'   A \code{\link[sp]{SpatialPolygons}} or
#'   \code{\link[sp]{SpatialPolygonsDataFrame}} object
#'
#' @return
#' An object of class \code{\link[sp]{SpatialPoints}}. If parameter \code{map}
#' has been provided then the function returns a
#' \code{\link[sp]{SpatialPointsDataFrame}} with a column geoAreaId containing
#' the number of the polygon a point belongs to.
#'
#' @noRd
#'
changeCoords <- function(lon, lat, col = "blue", info = paste(lon, ",", lat), map = NULL) {
  
  points <- data.frame(lon = lon, lat = lat, oldLon = lon, oldLat = lat,
                       color = col, info = as.character(info),
                       stringsAsFactors = FALSE)
  
  # Find the bottom-left most and top right-most points
  avgCoord <- rowMeans(points[, c("lon", "lat")])
  pt1 <- which.min(avgCoord)
  pt2 <- which.max(avgCoord)
  
  # Keep a copy of the initial coordinates
  points$oldLon <- points$lon
  points$oldLat <- points$lat
  
  ui <- miniPage(
    gadgetTitleBar("My Gadget"),
    miniContentPanel(
      fillRow(
        flex = c(NA, 1),
        tags$div(
          style = "width:200px;",
          tags$p(textOutput("order")),
          htmlOutput("info"),
          conditionalPanel(
            condition = "input.state < 2",
            imageOutput("preview", height="150px"),
            tags$p(),
            actionButton("state", "Next")
          )
        ),
        leafletDragPointsOutput("map", height = "100%")
      )
    )
  )
  
  renderPreview <- function(pt) {
    renderPlot({
      col <- rep("#cccccc", nrow(points))
      col[pt] <- "red"
      cex <- rep(1, nrow(points))
      cex[pt] <- 2
      par (mar = rep(0.1, 4))
      plot(points$oldLon, points$oldLat, bty = "n", xaxt = "n", yaxt = "n",
           xlab = "", ylab = "", main = "", col = col, asp = 1, pch = 19, cex = cex)
    })
  }
  
  server <- function(input, output, session) {
    # Initialize outputs
    points$lon[pt1] <- points$lat[pt1] <- 0
    output$map <- renderLeafletDragPoints({leafletDragPoints(points[pt1, ], map)})
    output$order <- renderText("Please place the following point on the map.")
    output$info <- renderUI(HTML(points$info[pt1]))
    output$preview <- renderPreview(pt1)
    
    coords <- reactive({
      coords <- matrix(input$map_coords, ncol = 2, byrow = TRUE)
      colnames(coords) <- c("lat", "lon")
      as.data.frame(coords)
    })
    
    observeEvent(input$state, {
      if (input$state == 1) {
        points$lat[pt2] <- input$map_mapcenter$lat
        points$lon[pt2] <- input$map_mapcenter$lng
        output$map <- renderLeafletDragPoints({leafletDragPoints(points[pt2, ])})
        output$info <- renderUI(HTML(points$info[pt2]))
        output$preview <- renderPreview(pt2)
      } else if (input$state == 2) {
        points <- .changeCoordinates(points, coords(), c(pt1, pt2))
        output$map <- renderLeafletDragPoints({leafletDragPoints(points[-c(pt1, pt2), ])})
        output$order <- renderText("Drag the markers on the map to adjust coordinates then click the 'Done' button")
        output$info <- renderUI(HTML("<p>You can click on a marker to display information about the corresponding point.</p>"))
      }
    })
    
    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      coords <- sp::SpatialPoints(coords()[, c("lon", "lat")],
                                  proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
      if (!is.null(map)) {
        map <- sp::spTransform(map, sp::CRS("+proj=longlat +datum=WGS84"))
        map$geoAreaId <- 1:length(map)
        coords$geoAreaId <- sp::over(coords, map)$geoAreaId
      }
      
      # Put coords in right order
      ord <- order(c(pt1, pt2, (1:length(coords))[-c(pt1, pt2)]))
      coords <- coords[ord,]
      
      stopApp(coords)
    })
  }
  
  runGadget(ui, server, viewer = browserViewer())
}

.changeCoordinates <- function(points, coords, pts = 1:nrow(points)) {
  coords$oldLon <- points$oldLon[pts]
  regLon <- lm(lon ~ oldLon, data = coords)
  points$lon <- predict(regLon, newdata = points)
  
  coords$oldLat <- points$oldLat[pts]
  regLat <- lm(lat ~ oldLat, data = coords)
  points$lat <- predict(regLat, newdata = points)
  
  points$oldLon <- points$oldLat <- NULL
  
  points
}

#' Plot method for map layout
#' 
#' This method can be used to visualize the network of an antares study.
#' It generates an interactive map with a visual representaiton of a
#' map layout created with function \code{\link{mapLayout}}. 
#' 
#' @param x
#'   Object created with function \code{\link{mapLayout}}
#' @param colAreas
#'   Vector of colors for areas. By default, the colors used in the Antares
#'   software are used.
#' @param dataAreas
#'   A numeric vector or a numeric matrix that is passed to function
#'   \code{link[addMinicharts]}. A single vector will produce circles with
#'   different radius. A matrix will produce bar charts or pie charts or 
#'   polar charts, depending on the value of \code{areaChartType}
#' @param opacityArea Opacity of areas. It has to be a numeric vector with values
#'   between 0 and 1.
#' @param areaMaxSize Maximal width in pixels of the symbols that represent 
#'   areas on the map.
#' @param areaChartType Type of chart to use to represent areas.
#' @param labelArea Character vector containing labels to display inside areas.
#' @param colLinks
#'   Vector of colors for links.
#' @param sizeLinks
#'   Line width of the links, in pixels.
#' @param opacityLinks Opacity of the links. It has to be a numeric vector with values
#'   between 0 and 1. 
#' @param dirLinks
#'   Single value or vector indicating the direction of the link. Possible values
#'   are 0, -1 and 1. If it equals 0, then links are repsented by a simple line. 
#'   If it is equal to 1 or -1 it is represented by a line with an arrow pointing
#'   respectively the destination and the origin of the link. 
#' @param areas
#'   Should areas be drawn on the map ?
#' @param links
#'   Should links be drawn on the map ?
#' @param ...
#'   Currently unused.
#' @inheritParams prodStack
#' @inheritParams plotMapOptions
#'   
#' @return 
#'   The function generates an \code{htmlwidget} of class \code{leaflet}. It can
#'   be stored in a variable and modified with package 
#'   \code{\link[leaflet]{leaflet}}
#'   
#' @examples 
#' \dontrun{
#' # Read the coordinates of the areas in the Antares interface, then convert it
#' # in a map layout.
#' layout <- readLayout()
#' ml <- mapLayout(layout)
#' 
#' # Save the result for future use
#' save(ml, file = "ml.rda")
#' 
#' # Plot the network on an interactive map
#' plot(ml)
#' 
#' # change style
#' plot(ml, colAreas = gray(0.5), colLinks = "orange")
#' 
#' # Use polar area charts to represent multiple values for each area.
#' nareas <- nrow(ml$coords)
#' fakeData <- matrix(runif(nareas * 3), ncol = 3)
#' plot(ml, sizeAreas = fakeData)
#' 
#' # Store the result in a variable to change it with functions from leaflet 
#' # package
#' library(leaflet)
#' 
#' center <- c(mean(ml$coords$x), mean(ml$coords$y))
#' 
#' p <- plot(ml)
#' p %>% 
#'   addCircleMarker(center[1], center[2], color = "red", 
#'                   popup = "I'm the center !")
#' }
#' 
#' @export
plot.mapLayout <- function(x, colAreas =  x$coords$color, dataAreas = 1,
                           opacityArea = 1, areaMaxSize = 30, areaMaxHeight = 50,
                           areaChartType = c("auto", "bar", "pie", "polar-area", "polar-radius"), 
                           labelArea = NULL, labelMinSize = 8, labelMaxSize = 8,
                           colLinks = "#CCCCCC", sizeLinks = 3, 
                           opacityLinks = 1, dirLinks = 0, 
                           links = TRUE, areas = TRUE, tilesURL = defaultTilesURL(),
                           preprocess = function(map) {map},
                           width = NULL, height = NULL, ...) {
  
  areaChartType <- match.arg(areaChartType)
  
  map <- leaflet(width = width, height = height, padding = 10) %>% addTiles(tilesURL) 
  
  # Add Polygons
  if (areas & !is.null(x$map)) {
    map <- addPolygons(map, data = x$map, layerId = x$coords$area, fillColor = colAreas, weight = 1,
                       fillOpacity = 1, color = "#333")
  }
  
  # Add custom elements
  map <- preprocess(map)
  
  # Add links
  if (links) {
    map <- addFlows(map, x$links$x0, x$links$y0, x$links$x1, x$links$y1, dir = dirLinks,
                    flow = abs(sizeLinks), opacity = opacityLinks, maxFlow = 1, maxThickness = 1,
                    color = colLinks, layerId = x$links$link)
  }
  
  # Add areas
  if (areas) {
    
    areaChartType <- match.arg(areaChartType)
    
    map <- addMinicharts(map, lng = x$coords$x, lat = x$coords$y, 
                         chartdata = dataAreas, fillColor = colAreas,
                         showLabels = !is.null(labelArea),
                         labelText = labelArea,
                         width = areaMaxSize,
                         height = areaMaxHeight,
                         layerId = x$coords$area, 
                         opacity = opacityArea,
                         labelMinSize = labelMinSize,
                         labelMaxSize = labelMaxSize)
    
  }
  
  # Add shadows to elements
  map %>% addShadows()
}
