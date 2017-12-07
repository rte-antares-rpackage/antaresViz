# Copyright © 2016 RTE Réseau de transport d’électricité

#' Private function that binds data with map layout and returns colors and sizes
#' for each element
#' 
#' @param data
#'   antaresDataTable containing data for areas or links
#' @param coords
#'   element of a map layout corresponding to data (coordinates of areas or links)
#' @param mergeBy
#'   name of the variable to merge data and coords by ("area" or "link").
#' @param t
#'   timeStep
#' @param colVar
#'   variable to map with colors. "none" for no mapping.
#' @param sizeVar
#'   variables to map with sizes. "none", NULL or c() for no mapping.
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
.getColAndSize <- function(data, coords, mergeBy, mcy, t, colVar, sizeVar, 
                           popupVars, colorScaleOpts, labelVar = NULL) {

  
  
  if (mcy != "average") data <- data[J(as.numeric(mcy))]
  
  neededVars <- setdiff(unique(c(colVar, sizeVar, popupVars, labelVar)), "none")
  if (any(! neededVars %in% names(data))) {
    missingVars <- setdiff(neededVars, names(data))
    for (v in missingVars) {
      warning("Column '", v, "' does not exist.", call. = FALSE)
    }
    
    neededVars <- intersect(neededVars, names(data))
  }
  
  if (is.null(t)) {
    if (length(neededVars) > 0) {
      data <- data[, lapply(.SD, mean), 
                   keyby = mergeBy, 
                   .SDcols = neededVars]
    } else {
      data <- unique(data[, mergeBy, with = FALSE])
    }
  }
  
  # Initialize the object returned by the function
  res <- list(coords = data, dir = 0)
  
  # color
  if (!colVar %in% names(data)) colVar <- "none"
  if (colVar != "none") {
    if (is.numeric(data[[colVar]])) {
      rangevar <- range(data[[colVar]])
      if(length(colorScaleOpts$breaks) > 1 ){
        if(min(rangevar) < min(colorScaleOpts$breaks)){
          colorScaleOpts$breaks <- c(min(rangevar), colorScaleOpts$breaks)
          colorScaleOpts$colors <- c("noColor", colorScaleOpts$colors)
        }
        
        if(max(rangevar) > max(colorScaleOpts$breaks)){
          colorScaleOpts$breaks <- c( colorScaleOpts$breaks, max(rangevar))
          colorScaleOpts$colors <- c( colorScaleOpts$colors, "noColor")
        }
      }
      
      # Special case of FLOW LIN
      if (colVar == "FLOW LIN.") rangevar <- c(0, max(abs(rangevar)))
      
      if (rangevar[1] >= 0) {
        domain <- rangevar
      } else {
        domain <- c(-min(rangevar), max(rangevar))
      }
      
      if (colVar == "FLOW LIN.") colorScaleOpts$x <- abs(data[[colVar]])
      else colorScaleOpts$x <- data[[colVar]]
      
      colorScaleOpts$domain <- domain
      res$color <- do.call(continuousColorPal, colorScaleOpts)
      
      res$pal <- attr(res$color, "pal")
      res$colorBreaks <- attr(res$color, "breaks")
    } else {
      if (is.null(colorScaleOpts$levels)) {
        if (is.factor(data[[colVar]])) colorScaleOpts$levels <- levels(data[[colVar]])
        else colorScaleOpts$levels <- unique(data[[colVar]])
      }
      
      colorScaleOpts$x <- data[[colVar]]
      
      res$color <- do.call(catColorPal, colorScaleOpts)
      res$pal <- attr(res$color, "pal")
      res$levels <- attr(res$color, "levels")
    }
    
  }
  
  # size
  sizeVar <- intersect(sizeVar, names(data))
  if (length(sizeVar) > 0 && !("none" %in% sizeVar)) {
    res$size <- as.matrix(data[, sizeVar, with = FALSE])
    res$maxSize <- apply(abs(as.matrix(data[, sizeVar, with = FALSE])), 2, max)
  }
  
  # Direction
  if ("FLOW LIN." %in% names(data)) {
    res$dir <- sign(data$`FLOW LIN.`)
    #coords[, `FLOW LIN.` := abs(`FLOW LIN.`)]
  } else {
    res$dir <- 0
  }
  
  # Pop-up
  # return names of columns that need to be added in popups
  res$popupVarsSup <- setdiff(neededVars, sizeVar)
  
  res
}

.getTimeFormat <- function(timeStep) {
  switch(
    timeStep,
    hourly = "%a %d %b %Y<br/>%H:%M",
    daily = "%a %d %b %Y",
    weekly = "W%V %Y",
    monthly = "%b %Y",
    yearly = "%Y"
  )
}

# Initialize a map with all elements invisible: links, circles and bar or polar 
# charts 
.initMap <- function(x, ml, options) {
  
  map <- plot(ml, areas = !is.null(x$areas), links = !is.null(x$links), 
              colAreas = options$areaDefaultCol,
              opacityArea = 1, opacityLinks = 1, 
              labelMinSize = options$labelMinSize,
              labelMaxSize = options$labelMaxSize,
              tilesURL = options$tilesURL, 
              preprocess = options$preprocess) %>% 
    addAntaresLegend(display = options$legend)
  
  addShadows(map)
}

# Update the circles and polar charts representing areas in an existing map
.redrawCircles <- function(map, x, mapLayout, mcy, t, colAreaVar, sizeAreaVars,
                           popupAreaVars, uniqueScale, showLabels, labelAreaVar,
                           areaChartType,
                           options, sizeMiniPlot = FALSE) {
  
  if (is.null(x$areas)) return(map)
  if (nrow(x$areas) == 0) return(map)
  
  
  timeStep <- attr(x, "timeStep")
  
  # Just in case, we do not want to accidentally modify the original map layout.
  ml <- copy(mapLayout)
  
  # Compute color and size of areas for the given time step.
  optsArea <- .getColAndSize(x$areas, ml$coords, "area", mcy, t,
                             colAreaVar, sizeAreaVars, popupAreaVars,
                             options$areaColorScaleOpts, labelVar = labelAreaVar)
  ml$coords <- optsArea$coords
  
  
  # Use default values if needed.
  if (is.null(optsArea$color)) optsArea$color <- options$areaDefaultCol
  
  if (is.null(optsArea$size)) {
    optsArea$size <- 1
    optsArea$maxSize <- 1
    areaWidth <- options$areaDefaultSize
  } else {
    areaWidth <- options$areaMaxSize
  }
  
  # Chart options
  if (length(sizeAreaVars) < 2) areaChartType <- "polar-area"
  if (uniqueScale) optsArea$maxSize <- max(optsArea$maxSize)
  
  # Labels
  labels <- NULL
  if (length(sizeAreaVars) < 2) {
    if (labelAreaVar == "none") {
      showLabels <- FALSE
    } else {
      showLabels <- TRUE
      labels <- optsArea$coords[[labelAreaVar]]
      # Create nice labels
      labels <- prettyNumbers(labels)
    }
    
  }
  
  showValuesInPopup <- length(sizeAreaVars) > 0
  
  # Update corresponding polygons if necessary
  if (!is.null(ml$map)) {
    onChange <- JS('
      var s = this._map.layerManager.getLayer("shape", this.layerId);
      s.bindPopup(popup);
      if (opts.fillColor) {
        d3.select(s._path)
          .transition()
          .duration(750)
          .attr("fill", opts.fillColor);
      }
    ')
    if (length(sizeAreaVars) < 2) width <- 0
    else width <- areaWidth
  } else {
    onChange <- JS(NULL)
    width <- areaWidth
    if (length(sizeAreaVars) >= 2) {
      optsArea$color <- options$areaDefaultCol
      optsArea$pal <- NULL
    }
  }
  
  if(sizeMiniPlot)
  {
    if(is.matrix(optsArea$size))
    {
      if(ncol(optsArea$size) > 1 )
      {
        optsArea$Va <- rowSums(optsArea$size)
        optsArea$VaP <- optsArea$Va / max(optsArea$Va)
        fM <- 3
        optsArea$Ra <- 15 + (optsArea$VaP * fM * 30)/2
      }
    }
  }
  
  if(is.null(optsArea$Ra)){optsArea$Ra <- width}
  # Update areas
  
  #Apply colors defined in color.csv
  if(is.null(options$areaChartColors))
  {
  varS <- names(optsArea$maxSize)
  colorDef <- colorsVars$colors[match(varS, colorsVars$Column)]
  defCol <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b",
              "e377c2",  "#7f7f7f", "#bcbd22", "#17becf")
  nbNa <- sum(is.na(colorDef))
  if(nbNa > 0)
  {
  colorDef[is.na(colorDef)] <- defCol[1:nbNa]
  }
  options$areaChartColors <- colorDef
  }
  
  map <- updateMinicharts(map, optsArea$coords$area, chartdata = optsArea$size,
                          time = optsArea$coords$time,
                          maxValues = optsArea$maxSize, width = optsArea$Ra,
                          height = options$areaMaxHeight,
                          showLabels = showLabels, labelText = labels, 
                          type = areaChartType[[1]], 
                          colorPalette = options$areaChartColors,
                          fillColor = optsArea$color,
                          timeFormat = .getTimeFormat(timeStep),
                          legend = FALSE,
                          onChange = onChange,
                          popup = popupArgs(
                            showValues = showValuesInPopup,
                            digits = 2,
                            supValues = optsArea$coords[, optsArea$popupVars, with = FALSE]
                          ))
  
  # Update the legend
  #
  # Color scale legend
  if (!is.null(optsArea$pal)) {
    if (is.null(optsArea$levels)) {
      map <- updateAntaresLegend(map, htmlAreaColor = colorLegend(colAreaVar, optsArea$pal, optsArea$colorBreaks))
    } else {
      map <- updateAntaresLegend(map, htmlAreaColor = barChartLegend(optsArea$levels, colAreaVar, optsArea$pal))
    }
  } else {
    map <- updateAntaresLegend(map, htmlAreaColor = "")
  }
  
  # Size legend (radius, polar or bar chart legend)
  if (length(sizeAreaVars) > 0) {
    if (length(sizeAreaVars) == 1) {
      map <- updateAntaresLegend(map, htmlAreaSize = radiusLegend(sizeAreaVars, options$areaMaxSize / 2, optsArea$maxSize))
    } else {
      map <- updateAntaresLegend(
        map, 
        htmlAreaSize = barChartLegend(sizeAreaVars, colors = options$areaChartColors)
      )
    }
  } else {
    map <- updateAntaresLegend(map, htmlAreaSize = "")
  }
  
  map
}

# Update the links in an existing map
.redrawLinks <- function(map, x, mapLayout, mcy, t, colLinkVar, sizeLinkVar, 
                         popupLinkVars, options) {
  if (is.null(x$links)) return(map)
  if (nrow(x$links) == 0) return(map)
  
  
  timeStep <- attr(x, "timeStep")
  
  ml <- copy(mapLayout)
  
  # Get color and size of links
  optsLink <- .getColAndSize(x$links, mapLayout$links, "link", mcy, t,
                             colLinkVar, sizeLinkVar, popupLinkVars,  
                             options$linkColorScaleOpts)
  
  # Use default values if needed
  if (is.null(optsLink$color)) optsLink$color <- options$linkDefaultCol
  if (is.null(optsLink$size)) {
    optsLink$size <- options$linkDefaultSize
    optsLink$maxSize <- options$linkMaxSize
  }
  
  showValuesInPopup <- sizeLinkVar != "none"
  
  map <- map %>% updateFlows(layerId = optsLink$coords$link, 
                             color = optsLink$color,
                             flow = abs(optsLink$size),
                             maxFlow = unname(optsLink$maxSize),
                             maxThickness = options$linkMaxSize,
                             time = optsLink$coords$time,
                             timeFormat = .getTimeFormat(timeStep),
                             dir = optsLink$dir,
                             popup = popupArgs(
                               showValues = showValuesInPopup,
                               labels = sizeLinkVar,
                               digits = 2,
                               supValues = optsLink$coords[, optsLink$popupVars, with = FALSE]
                             ),
                             opacity = 1)
  
  # Update the legend
  
  # Color scale legend
  if (!is.null(optsLink$pal)) {
    if (is.null(optsLink$levels)) {
      map <- updateAntaresLegend(map, htmlLinkColor = colorLegend(colLinkVar, optsLink$pal, optsLink$colorBreaks))
    } else {
      map <- updateAntaresLegend(map, htmlLinkColor = barChartLegend(optsLink$levels, colLinkVar, optsLink$pal))
    }
  } else {
    map <- updateAntaresLegend(map, htmlLinkColor = "")
  }
  
  # Line width legend
  if (showValuesInPopup) {
    map <- updateAntaresLegend(map, htmlLinkSize = lineWidthLegend(sizeLinkVar, options$linkMaxSize, optsLink$maxSize))
  } else {
    map <- updateAntaresLegend(map, htmlLinkSize = "")
  }
  
  map
}
