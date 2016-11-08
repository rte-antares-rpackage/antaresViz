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
#' * maxSize: vector containing maximal absolute value observed in each column of
#'            the data
#' 
#' @noRd
.getColAndSize <- function(data, coords, mergeBy, t, colVar, sizeVar, colorScaleOpts) {
  
  coords <- merge(coords, data[timeId == t], by = mergeBy)
  
  # Initialize the object returned by the function
  res <- list(coords = coords, dir = 0)
  
  # color
  if (colVar != "none" & length(sizeVar) <= 1) {
    rangevar <- range(data[[colVar]])
    if (rangevar[1] >= 0) {
      domain <- rangevar
    } else {
      domain <- c(-max(abs(rangevar)), max(abs(rangevar)))
    }
    
    colorScaleOpts$x <- coords[[colVar]]
    colorScaleOpts$domain = domain
    res$color <- do.call(continuousColorPal, colorScaleOpts)
    
    res$pal <- attr(res$color, "pal")
    res$colorBreaks <- attr(res$color, "breaks")
  }
  
  # size
  if (length(sizeVar) > 0 && !("none" %in% sizeVar)) {
    res$size <- as.matrix(coords[, sizeVar, with = FALSE])
    res$maxSize <- apply(abs(as.matrix(data[, sizeVar, with = FALSE])), 2, max)
    if (length(sizeVar) == 1) res$size <- res$size / res$maxSize
  }
  
  # Direction
  if ("FLOW LIN." %in% names(coords)) {
    res$dir <- sign(coords$`FLOW LIN.`)
  } else {
    res$dir <- 0
  }
  
  res
}

# Initialize a map with all elements invisible: links, circles and bar or polar 
# charts 
.initMap <- function(x, ml, areaChartType, options) {
  
  map <- plot(ml, areas = !is.null(x$areas), links = !is.null(x$links), 
              opacityArea = 0, opacityLinks = 0, addTiles = options$addTiles,
              polygons = options$polygons, 
              polygonOptions = options$polygonOptions) %>% 
    addAntaresLegend()
  
  # Add a layer with bar or polar charts
  if (!is.null(x$areas)) {
    addChart <- switch(areaChartType, bar = addBarCharts, polar = addPolarCharts)
    map <- addChart(map, ml$coords$x, ml$coords$y,
                    data = matrix(1, nrow = nrow(ml$coords)),
                    opacity = 0, layerId = ml$coords$area,
                    colors = options$areaChartColors,
                    popup = ml$coords$area, size = options$areaMaxSize)
  }
  
  # Reset the bounds of the map if links are drawn
  if (!is.null(x$links)) {
    rangeX <- range(c(ml$links$x0, ml$links$x1))
    rangeY <- range(c(ml$links$y0, ml$links$y1))
    map <- fitBounds(map, rangeX[1], rangeY[1], rangeX[2], rangeY[2])
  }
  
  addShadows(map)
}

# Update the circles or polar charts representing areas in an existing map
.redrawCircles <- function(map, x, mapLayout, t, colAreaVar, sizeAreaVars,
                           areaChartType, options) {
  if (is.null(x$areas)) return(map)
  
  # Just in case, we do not want to accidentally modify the original map layout.
  ml <- copy(mapLayout)
  
  # How to represent multiple size variables ?
  updateChart <- switch(areaChartType, 
                        bar = updateBarCharts, 
                        polar = updatePolarCharts)
  
  # Compute color and size of areas for the given time step.
  optsArea <- .getColAndSize(x$areas, ml$coords, "area", t,
                             colAreaVar, sizeAreaVars, options$areaColorScaleOpts)
  ml$coords <- optsArea$coords
  
  # Use default values if needed.
  if (is.null(optsArea$color)) optsArea$color <- options$areaDefaultCol
  
  if (is.null(optsArea$size)) optsArea$size <- options$areaDefaultSize
  else if (ncol(optsArea$size) == 1) {
    optsArea$size <- sqrt(abs(optsArea$size)) * options$areaMaxSize
  }
  
  # Update circle markers and/or polar/bar charts.
  if (is.matrix(optsArea$size) && ncol(optsArea$size) > 1) {
    if (options$areaChartUniqueScale) optsArea$maxSize <- max(optsArea$maxSize)
      
    map <- map %>% 
      updateCircleMarkers(optsArea$coords$area, opacity = 0, fillOpacity = 0) %>% 
      updateChart(optsArea$coords$area, opacity = 1, data = optsArea$size, 
                  maxValue = optsArea$maxSize)
  } else {
    map <- map %>% 
      updateCircleMarkers(optsArea$coords$area, fillColor = optsArea$color, 
                          radius = optsArea$size, 
                          opacity = 1, fillOpacity = 1) %>% 
      updateChart(optsArea$coords$area, opacity = 0)
  }
  
  # Update the legend
  #
  # Color scale legend
  if (!is.null(optsArea$pal)) {
    map <- updateAntaresLegend(map, htmlAreaColor = colorLegend(colAreaVar, optsArea$pal, optsArea$colorBreaks))
  } else {
    map <- updateAntaresLegend(map, htmlAreaColor = "")
  }
  
  # Size legend (radius, polar or bar chart legend)
  if (!is.null(optsArea$maxSize)) {
    if (length(sizeAreaVars) == 1) {
      map <- updateAntaresLegend(map, htmlAreaSize = radiusLegend(sizeAreaVars, options$areaMaxSize, optsArea$maxSize))
    } else {
      if (areaChartType == "bar") {
        map <- updateAntaresLegend(
          map, 
          htmlAreaSize = barChartLegend(sizeAreaVars, colors = options$areaChartColors)
        )
      } else {
        map <- updateAntaresLegend(
          map, 
          htmlAreaSize = polarChartLegend(),
          onComplete = polarChartLegendJS(sizeAreaVars, colors = options$areaChartColors)
        )
      }
    }
  } else {
    map <- updateAntaresLegend(map, htmlAreaSize = "")
  }
  
  map
}

# Update the links in an existing map
.redrawLinks <- function(map, x, mapLayout, t, colLinkVar, sizeLinkVar, options) {
  if (is.null(x$links)) return(map)
  
  ml <- copy(mapLayout)
  
  # Get color and size of links
  optsLink <- .getColAndSize(x$links, mapLayout$links, "link", t,
                             colLinkVar, sizeLinkVar, options$linkColorScaleOpts)
  
  # Use default values if needed
  if (is.null(optsLink$color)) optsLink$color <- options$linkDefaultCol
  if (is.null(optsLink$size)) optsLink$size <- options$linkDefaultSize
  else optsLink$size <- optsLink$size * options$linkMaxSize
  
  map <- map %>% updateDirectedSegments(layerId = ml$links$link, 
                                        color = optsLink$color,
                                        weight = optsLink$size,
                                        dir = optsLink$dir,
                                        opacity = 1)
  
  # Update the legend
  
  # Color scale legend
  if (!is.null(optsLink$pal)) {
    map <- updateAntaresLegend(map, htmlLinkColor = colorLegend(colLinkVar, optsLink$pal, optsLink$colorBreaks))
  } else {
    map <- updateAntaresLegend(map, htmlLinkColor = "")
  }
  
  # Line width legend
  if (!is.null(optsLink$maxSize)) {
    map <- updateAntaresLegend(map, htmlLinkSize = lineWidthLegend(sizeLinkVar, options$linkMaxSize, optsLink$maxSize))
  } else {
    map <- updateAntaresLegend(map, htmlLinkSize = "")
  }
  
  map
}
