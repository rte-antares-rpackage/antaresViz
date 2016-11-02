#Copyright © 2016 RTE Réseau de transport d’électricité

#' Display results of a simulation on a map
#' 
#' This function generates an interactive map that let the user visually explore
#' the results of an Antares simulation. By default the function starts a Shiny 
#' gadget that let the user which variables to represent.
#' 
#' @param x
#'   Object of class \code{antaresDataList} created with 
#'   \code{\link[antaresRead]{readAntares}} and containing areas and links data
#' @param mapLayout
#'   Object created with function \code{\link{mapLayout}}
#' @param colAreaVar
#'   Name of a variable present in \code{x$areas}. The values of this variable
#'   are represented by the color of the areas on the map. If \code{"none"}, then
#'   the default color is used for all areas. 
#' @param sizeAreaVars
#'   vector of variables present in \code{x$areas} to associate with the size of 
#'   areas on the map. If this parameter has length equal to 0, all areas have the
#'   same size. If it has length equal to one, then the radius of the areas change
#'   depending on the values of the variable choosen. If it has length greater than
#'   1 then areas are represented by a polar area chart where the size of each section
#'   depends on the values of each variable.
#' @param colLinkVar
#'   Name of a variable present in \code{x$links}. The values of this variable
#'   are represented by the color of the links on the map. If \code{"none"}, then
#'   the default color is used for all links  
#' @param sizeLinkVar
#'   Name of a variable present in \code{x$links}. Its values are represented by
#'   the line width of the links on the map.
#' @param timeId
#'   A single time id present in the data.
#' @param options
#'   List of parameters that override some default visual settings. See the
#'   help of \code{\link{plotMapOptions}}.
#' @inheritParams productionStack
#'   
#' @return 
#' An htmlwidget of class "leaflet". It can be modified with package 
#' \code{leaflet}. By default the function starts a shiny gadget that lets the
#' user play with most of the parameters of the function. The function returns
#' a leaflet map when the user clicks on the button \code{"done"}.
#' 
#' @examples 
#' \dontrun{
#' mydata <- readAntares(areas = "all", links = "all", timeStep = "daily",
#'                       select = "nostat")
#' 
#' # Place areas on a map. Ths has to be done once for a given study. Then the
#' # object returned by "mapLayout" may be saved and reloaded with
#' # functions save and load
#' 
#' layout <- readLayout()
#' ml <- mapLayout(layout)
#' save("ml", file = "ml.rda")
#' 
#' plotMap(mydata, ml)
#' 
#' # Specify the variables to use to control the color or size of elements.
#' plotMap(mydata, ml, 
#'         sizeAreaVars = c("WIND", "SOLAR", "H. ROR"),
#'         sizeLinkVar = "FLOW LIN.")
#' 
#' # Change default graphical properties
#' plotMap(mydata, ml, options = list(colArea="red", colLink = "orange"))
#' 
#' }
#' 
#' @export
plotMap <- function(x, mapLayout, colAreaVar = "none", sizeAreaVars = c(),
                    areaChartType = c("polar", "bar"),
                    colLinkVar = "none", sizeLinkVar = "none", 
                    timeId = NULL,
                    interactive = base::interactive(),
                    options = plotMapOptions(),
                    width = NULL, height = NULL) {
  
  areaChartType <- match.arg(areaChartType)
  options <- do.call(plotMapOptions, options)
  
  # Check that parameters have the good class
  if (!is(mapLayout, "mapLayout")) stop("Argument 'mapLayout' must be an object of class 'mapLayout' created with function 'mapLayout'.")
  if (!is(x, "antaresData")) {
    stop("Argument 'x' must be an object of class 'antaresData' created with function 'readAntares'.")
  } else {
    x <- as.antaresDataList(x)
    if (is.null(x$areas) && is.null(x$links)) stop("Argument 'x' should contain at least area or link data.")
  }
  
  # Should links and/or areas be displayed ?
  areas <- !is.null(x$areas)
  links <- !is.null(x$links)
  
  # Select first timeId if necessary
  if (is.null(timeId)) {
    if (areas) timeId <- min(x$areas$timeId)
    else timeId <- min(x$links$timeId)
  } 
  
  # Keep only links and areas present in the data
  if (areas) {
    areaList <- unique(x$areas$area)
    mapLayout$coords <- mapLayout$coords[area %in% areaList]
  }
  if (links) {
    linkList <- unique(x$links$link)
    mapLayout$links <- mapLayout$links[link %in% linkList]
  }
  
  # Function that draws the final map when leaving the shiny gadget.
  plotFun <- function(t, colAreaVar, sizeAreaVars, colLinkVar, sizeLinkVar) {
    
    ml <- copy(mapLayout)
    
    if (areas) {
      optsArea <- .getColAndSize(x$areas, mapLayout$coords, "area", t,
                                 colAreaVar, sizeAreaVars)
      
      ml$coords <- optsArea$coords
      
      if (is.null(optsArea$color)) optsArea$color <- options$colArea
      
      if (is.null(optsArea$size)) optsArea$size <- options$sizeArea
      else if (ncol(optsArea$size) == 1) {
        optsArea$size <- sqrt(optsArea$size) * options$maxSizeArea
      }
    } else {
      optsArea <- list()
    }
    
    if (links) {
      optsLink <- .getColAndSize(x$links, mapLayout$links, "link", t,
                                 colLinkVar, sizeLinkVar)
      
      ml$links <- optsLink$coords
      
      if (is.null(optsLink$color)) optsLink$color <- options$colLink
      if (is.null(optsLink$size)) optsLink$size <- options$sizeLink
      else optsLink$size <- optsLink$size * options$maxSizeLink
      
    } else {
      optsLink <- list()
    }
    
    
    
    map <- plot(ml, optsArea$color, optsArea$size, optsArea$maxSize, areaChartType,
                optsLink$color, optsLink$size, dir = optsLink$dir,
                areas = areas, links = links,
                width = width, height = height) %>% addAntaresLegend()
    
    # Add legends
    if (!is.null(optsArea$pal))
      map <- updateAntaresLegend(map, htmlAreaColor = colorLegend(colAreaVar, optsArea$pal, optsArea$colorBreaks))
    if (!is.null(optsArea$maxSize)) {
      if (length(sizeAreaVars) == 1) {
        map <- updateAntaresLegend(map, htmlAreaSize = radiusLegend(sizeAreaVars, options$maxSizeArea, optsArea$maxSize))
      } else {
        map <- updateAntaresLegend(
          map, 
          htmlAreaSize = polarChartLegend(),
          onComplete = polarChartLegendJS(sizeAreaVars)
        )
      }
    }
    if (!is.null(optsLink$pal))
      map <- updateAntaresLegend(map, htmlLinkColor = colorLegend(colLinkVar, optsLink$pal, optsLink$colorBreaks))
    
    if (!is.null(optsLink$maxSize)) {
      map <- updateAntaresLegend(map, htmlLinkSize = lineWidthLegend(sizeLinkVar, options$maxSizeLink, optsLink$maxSize))
    }
    
    if (areas) {
      # Add an invisible layer containing either circleMarkers or polarCharts
      if (length(sizeAreaVars) <= 1) {
        addChart <- switch(areaChartType, polar = addPolarChart, bar = addBarChart)
        
        map <- addChart(map, ml$coords$x, ml$coords$y, 
                        data = matrix(1, nrow = nrow(ml$coords)),
                        opacity = 0, layerId = ml$coords$area,
                        popup = ml$coords$area)
      } else {
        map <- addCircleMarkers(map, ml$coords$x, ml$coords$y, opacity = 0, 
                                fillOpacity = 0,
                                layerId = ml$coords$area,
                                popup = ml$coords$area, stroke = FALSE)
      }
    }
    
    map %>% addShadows()
  }
  
  initialMap <- plotFun(timeId, colAreaVar, sizeAreaVars, colLinkVar, sizeLinkVar)
  
  if (!interactive) {
    return(initialMap)
  }
  
  areaValColums <- setdiff(names(x$areas), .idCols(x$areas))
  linkValColums <- setdiff(names(x$links), .idCols(x$links))
  
  ui <- mwUI(
    timeId = mwSlider(min(x$areas$timeId), max(x$areas$timeId), timeId, step = 1, animate = TRUE),
    Areas = list(
      colAreaVar = mwSelect(c("none", areaValColums), colAreaVar, label = "Color"),
      sizeAreaVars = mwSelect(areaValColums, sizeAreaVars, label = "Radius", multiple = TRUE)
    ),
    Links = list(
      colLinkVar = mwSelect(c("none", setdiff(names(x$links), .idCols(x$links))), colLinkVar, label = "Color"),
      sizeLinkVar = mwSelect(c("none", setdiff(names(x$links), .idCols(x$links))), sizeLinkVar, label = "Width")
    ),
    .content = leafletOutput("map", height = "100%")
  )
  
  args <- runGadget(ui, 
                    .plotMapServer(x, mapLayout, initialMap, areaChartType, options), 
                    viewer = browserViewer())
  do.call(plotFun, args)
}

