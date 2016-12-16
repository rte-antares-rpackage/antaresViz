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
  if (!is.null(map)) coords$geoAreaId <- mapCoords$geoAreaId
  
  if (what == "areas") {
    links[coords, `:=`(x0 = x, y0 = y),on=c(from = "area")]
    links[coords, `:=`(x1 = x, y1 = y),on=c(to = "area")]
  } else {
    links[coords, `:=`(x0 = x, y0 = y),on=c(fromDistrict = "district")]
    links[coords, `:=`(x1 = x, y1 = y),on=c(toDistrict = "district")]
  }
  
  res <- list(coords = coords, links = links)
  class(res) <- "mapLayout"
  attr(res, "type") <- what
  res
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
#'   \code{link[addD3charts]}. A single vector will produce circles with
#'   different radius. A matrix will produce bar charts or pie charts or 
#'   polar charts, depending on the value of \code{areaChartType}
#' @param popupArea
#'   Character vector containing the html to display when the user clicks on an
#'   area.
#' @param colLinks
#'   Vector of colors for links.
#' @param sizeLinks
#'   Line width of the links, in pixels.
#' @param dirLinks
#'   Single value or vector indicating the direction of the link. Possible values
#'   are 0, -1 and 1. If it equals 0, then links are repsented by a simple line. 
#'   If it is equal to 1 or -1 it is represented by a line with an arrow pointing
#'   respectively the destination and the origin of the link. 
#' @param popupLink
#'   Character vector containing the html to display when the user clicks on a 
#'   link.
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
                           opacityArea = 1, areaMaxWidth = 20,
                           areaChartType = c("auto", "bar", "pie", "polar-area", "polar-radius"), 
                           popupArea = x$coords$area, 
                           labelArea = NULL,
                           colLinks = "#CCCCCC", sizeLinks = 3, 
                           opacityLinks = 1, dirLinks = 0, 
                           popupLink = x$links$link,
                           links = TRUE, areas = TRUE,
                           addTiles = TRUE, background = "white", polygons = NULL,
                           polygonOptions = list(stroke = TRUE,
                                                 color = "#bbb",
                                                 weight = 0.5,
                                                 opacity = 1,
                                                 fillOpacity = 0.2,
                                                 options = list(clickable = FALSE)),
                           width = NULL, height = NULL, ...) {
  
  areaChartType <- match.arg(areaChartType)
  
  map <- leaflet(width = width, height = height, padding = 10)
  
  # Add a base map
  if (addTiles) {
    map <- addTiles(map, "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}")
  }
  
  # Add custom polygons
  if (!is.null(polygons)) {
    if (!is(polygons, "SpatialPolygonsDataFrame") || !is(polygons, "SpatialPolygons")) {
      stop("Parameter 'polygons' should be an object of class 'SpatialPolygonsDataFrame'.")
    }
    polygonOptions$map <- map
    polygonOptions$data <- polygons
    map <- do.call(addPolygons, polygonOptions)
  }
  
  # Add links
  if (links) {
    map <- addDirectedSegments(map, x$links$x0, x$links$y0, x$links$x1, x$links$y1, dir = dirLinks,
                               weight = sizeLinks, opacity = opacityLinks,
                               color = colLinks, layerId = x$links$link, popup = popupLink)
  }
  
  # Add areas
  if (areas) {
    areaChartType <- match.arg(areaChartType)
    
    map <- addD3charts(map, lng = x$coords$x, lat = x$coords$y, 
                       data = dataAreas, fillColor = colAreas,
                       showLabels = !is.null(labelArea),
                       labelText = labelArea,
                       width = areaMaxWidth,
                       popup = popupArea, layerId = x$coords$area, opacity = opacityArea)
  }
  
  # Set the view of the map to include all data
  xcoords <- c()
  ycoords <- c()
  
  if (links) {
    xcoords <- c(x$links$x0, x$links$x1)
    ycoords <- c(x$links$y0, x$links$y1)
  }
  
  if (areas) {
    xcoords <- c(xcoords, x$coords$x)
    ycoords <- c(ycoords, x$coords$y)
  }

  rangeX <- range(xcoords)
  rangeY <- range(ycoords)
  
  map <- fitBounds(map, rangeX[1], rangeY[1], rangeX[2], rangeY[2])
  
  # Add shadows to elements
  map %>% addShadows()
}
