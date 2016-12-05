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
.getColAndSize <- function(data, coords, mergeBy, t, colVar, sizeVar, 
                           popupVars, colorScaleOpts, labelVar = NULL) {
  
  neededVars <- setdiff(unique(c(colVar, sizeVar, popupVars, labelVar)), "none")
  if (is.null(t)) {
    if (length(neededVars) == 0) {
      data <- unique(data[, mergeBy, with = FALSE])
    } else {
      data <- data[, lapply(.SD, mean), 
                   keyby = mergeBy, 
                   .SDcols = neededVars]
    }
    dataFiltered <- data
  } else {
    if (!is.null(data$mcYear) & length(unique(data$mcYear)) > 1 & length(neededVars) > 0) {
      data <- data[, lapply(.SD, mean), 
                   keyby = c("timeId", mergeBy),
                   .SDcols = neededVars]
    }
    dataFiltered <- data[timeId == t]
  }
  coords <- merge(coords, dataFiltered, by = mergeBy)
  
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
  }
  
  # Direction
  if ("FLOW LIN." %in% names(coords)) {
    res$dir <- sign(coords$`FLOW LIN.`)
  } else {
    res$dir <- 0
  }
  
  # Pop-up
  res$popup <- .valuesToPopup(coords, union(colVar, union(sizeVar, popupVars)), coords[[mergeBy]])
  
  res
}

.valuesToPopup <- function(x, var, title) {
  var <- var[var %in% names(x)]
  if (length(var) == 0) return(title)
  
  popupTemplate <- '
  <div class = "popup">
  <h2>%s</h2>
  <hr/>
  <table class="">
    <tbody>
      %s
    </tbody>
  </table>
  </div>
  '
  
  rowTemplate <- '
      <tr>
        <td class="key">%s</td>
        <td class ="value">%s</td>
      </tr>
  '
  
  x <- as.matrix(x[, var, with = FALSE])
  rows <- apply(x, 1, function(x) {
    sprintf(rowTemplate, var, x) %>% 
      paste(collapse = "")
  })
  
  sprintf(popupTemplate, title, rows)
}

# Initialize a map with all elements invisible: links, circles and bar or polar 
# charts 
.initMap <- function(x, ml, options, width, height) {
  
  map <- plot(ml, areas = !is.null(x$areas), links = !is.null(x$links), 
              opacityArea = 1, opacityLinks = 1, addTiles = options$addTiles,
              polygons = options$polygons, 
              polygonOptions = options$polygonOptions,
              width = width, height = height) %>% 
    addAntaresLegend(display = options$legend)
  
  addShadows(map)
}

# Update the circles and polar charts representing areas in an existing map
.redrawCircles <- function(map, x, mapLayout, t, colAreaVar, sizeAreaVars,
                           popupAreaVars, uniqueScale, showLabels, labelAreaVar,
                           options) {
  if (is.null(x$areas)) return(map)
  
  # Just in case, we do not want to accidentally modify the original map layout.
  ml <- copy(mapLayout)
  
  # Compute color and size of areas for the given time step.
  optsArea <- .getColAndSize(x$areas, ml$coords, "area", t,
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
  if (uniqueScale) optsArea$maxSize <- max(optsArea$maxSize)
  
  # Labels
  labels <- NULL
  if (length(sizeAreaVars) < 2) {
    if (labelAreaVar == "none") {
      showLabels <- FALSE
    } else {
      showLabels <- TRUE
      labels <- optsArea$coords[[labelAreaVar]]
    }
    
  }
  
  # Update areas
  map <- updateD3charts(map, optsArea$coords$area, data = optsArea$size,
                        maxValues = optsArea$maxSize, width = areaWidth,
                        showLabels = showLabels, labelText = labels,
                        fillColor = optsArea$color, popup = optsArea$popup)
  
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
      if (options$areaChartType == "bar") {
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
.redrawLinks <- function(map, x, mapLayout, t, colLinkVar, sizeLinkVar, 
                         popupLinkVars, options) {
  if (is.null(x$links)) return(map)
  
  ml <- copy(mapLayout)
  
  # Get color and size of links
  optsLink <- .getColAndSize(x$links, mapLayout$links, "link", t,
                             colLinkVar, sizeLinkVar, popupLinkVars,  
                             options$linkColorScaleOpts)
  
  # Use default values if needed
  if (is.null(optsLink$color)) optsLink$color <- options$linkDefaultCol
  if (is.null(optsLink$size)) optsLink$size <- options$linkDefaultSize
  else optsLink$size <- optsLink$size /optsLink$ maxSize * options$linkMaxSize
  
  map <- map %>% updateDirectedSegments(layerId = ml$links$link, 
                                        color = optsLink$color,
                                        weight = optsLink$size,
                                        dir = optsLink$dir,
                                        popup = optsLink$popup,
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
