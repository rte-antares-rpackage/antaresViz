#' Place areas of a study on a map
#' 
#' This function launches an interactive application that let the user place 
#' areas of a study on a map. the GPS coordinates of the areas are then returned
#' and can be used in functions.
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

#' @export
plot.mapLayout <- function(x, colAreas =  x$coords$color, colLinks = "blue", 
                           dirLinks = 0, ...) {
  map <- leaflet() %>% 
    addTiles(urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}") %>%
    addDirectedSegments(x$links$x0, x$links$y0, x$links$x1, x$links$y1, dir = dirLinks,
                        color = colLinks, layerId = x$links$link, popup = x$links$link) %>% 
    addCircleMarkers(lng = x$coords$x, lat = x$coords$y, 
                     color = gray(0.5), weight = 1, 
                     fillColor = colAreas, fillOpacity = 1, 
                     popup = x$coords$area)
  
  map
}
