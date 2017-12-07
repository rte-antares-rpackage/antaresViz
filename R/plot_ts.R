#' Private function that draws a given time series for each element
#' 
#' @param dt
#'   data.table created by .prepareDataForPlot
#' @param timeStep
#'   Time step of the data
#' @param main
#'   Title of the plot
#' @param ylab
#'   Label of the y-axis
#' @param confInt
#'   A number between 0 and 1 indicating the size of the confidence interval to
#'   display If NULL, then confidence intervals are not computed and displayed.
#' 
#' @return dygraphwidget object.
#' 
#' @noRd
#' 
.plotTS <- function(dt, timeStep, variable, variable2Axe = NULL, confInt = 0, maxValue, 
                    colors = NULL,
                    main = NULL,
                    ylab = NULL,
                    legend = TRUE,
                    legendItemsPerRow = 5,
                    group = NULL,
                    width = NULL, height = NULL, highlight = FALSE, stepPlot = FALSE, drawPoints = FALSE, ...) {
  
  
  uniqueElements <- sort(unique(dt$element))
  plotConfInt <- FALSE
  if (is.null(group)) group <- sample(1e9, 1)
  
  # If dt contains several Monte-Carlo scenario, compute aggregate statistics
  if (!is.null(dt$mcYear)) {
    if (confInt == 0) {
      
      dt <- dt[, .(value = mean(value)), by = .(element, time)]
      
    } else {
      
      plotConfInt <- TRUE
      alpha <- (1 - confInt) / 2
      dt <- dt[, .(value = c(mean(value), quantile(value, c(alpha, 1 - alpha))),
                   suffix = c("", "_l", "_u")), 
               by = .(time, element)]
      dt[, element := paste0(element, suffix)]
    }
  }
  
  dt <- dcast(dt, time ~ element, value.var = "value")
  
  # Graphical parameters
  if(length(uniqueElements)> 1)
  {
  variable <- paste0(uniqueElements, collapse = " ; ")
  }else{
    variable <- paste0(uniqueElements, " - ", variable)
    
  }
  
  if (is.null(ylab)) ylab <- variable
  if (is.null(main) | isTRUE(all.equal("", main))) main <- paste("Evolution of", variable)
  if (is.null(colors)) {
    colors <- substring(rainbow(length(uniqueElements), s = 0.7, v = 0.7), 1, 7)
  } else {
    colors <- rep(colors, length.out = length(uniqueElements))
  }
  
  legendId <- sample(1e9, 1)
  
  g <- dygraph(as.xts.data.table(dt), main = main, group = group) %>% 
    dyOptions(
      includeZero = TRUE, 
      gridLineColor = gray(0.8), 
      axisLineColor = gray(0.6), 
      axisLabelColor = gray(0.6), 
      labelsKMB = TRUE,
      colors = colors, 
      useDataTimezone = TRUE,
      stepPlot = stepPlot,
      drawPoints = drawPoints
    ) %>% 
    dyAxis("x", rangePad = 10) %>% 
    dyAxis("y", label = ylab, pixelsPerLabel = 60, rangePad = 10) %>% 
    #dyRangeSelector() %>% 
    dyLegend(show = "never") %>% 
    dyCallbacks(
      highlightCallback = JS_updateLegend(legendId, timeStep),
      unhighlightCallback = JS_resetLegend(legendId)
    )
  if(length(variable2Axe)>0){
    for( i in variable2Axe)
    {
    g <- g %>% dySeries(i, axis = 'y2')
    }
  }
  
  
  if(highlight)
  {
    g  <- g  %>% dyHighlight(highlightSeriesOpts = list(strokeWidth = 2))
  }
  
  if (plotConfInt) {
    for (v in uniqueElements) {
      axis = NULL
      if(length(variable2Axe)>0)
      {
      if(v%in%variable2Axe)
      {
        axis <- "y2"
      } 
      }
      g <- g %>% dySeries(paste0(v, c("_l", "", "_u")), axis = axis)
    }
  }
  
  if (legend) {
    l <- tsLegend(uniqueElements, types = rep("line", length(uniqueElements)), 
                  colors = colors, legendId = legendId, legendItemsPerRow = legendItemsPerRow)
  } else l <- NULL
  
  
  combineWidgets(g, footer = l, width = width, height = height)
  
}