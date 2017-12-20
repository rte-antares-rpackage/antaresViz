# Copyright © 2016 RTE Réseau de transport d’électricité

#' Generate an interactive stack
#' 
#' @param x
#'   data.table containing at least a column "timeId" and numeric columns
#'   representing areas or lines to draw
#' @param timeStep
#'   timeStep
#' @param opts
#'   simulation options
#' @param colors
#'   vector of colors
#' @param lines
#'   names of the variables that need to be represented as lines.
#' 
#' @return 
#'   an htmlWidget created with function "dygraph"
#'   
#' @note 
#' When series have positive and negative values, stacked area graphs are not
#' clearly defined. In our case we want positive values shown in the part of
#' the graph above 0 and the negative values below 0. To achieve that, we have
#' to hack the default behavior of dygraphs:
#' 
#' 1 - for each time step, sum all negative values and plot the corresponding
#' area in white
#' 2 - plot the areas corresponding to the negative values of each series as if
#' they were positive. This will completely cover the area drawn in step 1.
#' 3 - plot areas corresponding to the positive values of each series.
#'
#' Notice that dygraphs plot series in reverse order, so the data table we need
#' to create must contain a time column, then the positive values of each
#' series, then the negative values of each column (absolute values) and finally
#' a column with the total of negative values.
#' 
#' dygraphs does not offer the possibility to add a curve over a stacked graph.
#' Once again this require a hack: before ploting any area, plot the curve series
#' without filling the area under the curve, then plot an invisible series equal
#' to the opposite of the curve in order to "go back" to zero. This way, the 
#' next series is drawn from 0.
#' 
#' @noRd
.plotStack <- function(x, timeStep, opts, colors, lines = NULL, lineColors = NULL, 
                       legendId = "", groupId = legendId, main = "", ylab = "",
                       width = NULL, height = NULL, dateRange = NULL, stepPlot = FALSE, drawPoints = FALSE) {
  
  variables <- setdiff(names(x), c("timeId", lines))
  
  # 1- Create a data.table containing the series defined by parameter "variables"
  dt <- data.table(timeId = x$timeId) #, area = x$area)
  dt$time <- .timeIdToDate(dt$timeId, timeStep, opts)
  dt[, timeId := NULL]
  dt[, c(rev(variables), paste0("neg", variables), "totalNeg", lines, paste0("opp", lines)) := 0]
  
  nvar <- length(variables)
  nlines <- length(lines)
  
  for (i in length(variables):1) {
    values <- x[, get(variables[i]) ]
    set(dt, j = nvar + 2L - i, value = values)
  }
  
  if (nlines > 0) {
    for (i in 1:nlines) {
      value <- x[, get(lines[i]) ]
      set(dt, j = 2L * nvar + 2L + i, value = value)
      set(dt, j = 2L * nvar + 2L + nlines + i, value = -value)
    }
  }
  
  # 2- Group by timeId
  #dt[, area := NULL]
  dt <- dt[, lapply(.SD, sum), by = time]
  
  # 3- Separate positive and negative values and compute total negative values
  for (i in length(variables):1) {
    values <- dt[[variables[i]]]
    posValues <- pmax(0, values)
    negValues <- pmin(0, values)
    
    set(dt, j = nvar + 2L - i, value = posValues)
    set(dt, j = nvar + 1L + i, value = - negValues)
    dt$totalNeg <- dt$totalNeg + negValues
  }
  
  ##Add first and last row of not in range
  if(!is.null(dateRange))
  {
  if(dt$time[1] > dateRange[1]){
    dt <- dt[c(NA, 1:nrow(dt))]
    dt$time[1] <- dateRange[1]
  }
  nrowTp <- nrow(dt)

  if(dt$time[nrowTp] < dateRange[2]){
    dt <- dt[c(1:nrow(dt), NA)]
    dt$time[nrowTp + 1] <- dateRange[2]
  }
  }
  
  # 5- Finally plot !!
  colors <- unname(c("#FFFFFF", rev(colors), colors))
  
  g <- dygraph(as.xts.data.table(dt), main = main, group = groupId, width = width, height = height)  %>%
    dyOptions(
      stackedGraph = TRUE, 
      colors = rev(colors), 
      fillAlpha = 0.6,
      includeZero = TRUE, 
      gridLineColor = gray(0.8), 
      axisLineColor = gray(0.6), 
      axisLabelColor = gray(0.6), 
      strokeWidth = 0,
      useDataTimezone = TRUE ,
      stepPlot = stepPlot,
      drawPoints = drawPoints
    ) %>% 
    dyAxis("x", rangePad = 10) %>% 
    dyAxis("y", label = ylab, rangePad = 10, pixelsPerLabel = 50, valueRange = c(min(dt$totalNeg, na.rm = TRUE) * 1.1, NA)) %>% 
    dyLegend(show = "never") %>% 
    dyCallbacks(
      highlightCallback = JS_updateLegend(legendId, timeStep),
      unhighlightCallback = JS_resetLegend(legendId)
    )
  
  if (length(lines) > 0) {
    for (i in 1:length(lines)) {
      g <- g %>% dySeries(name = paste0("opp", lines[i]), color = lineColors[i], 
                          fillGraph = FALSE, strokeWidth = 0)
      g <- g %>% dySeries(name = lines[i], color = lineColors[i], 
                          fillGraph = FALSE, strokeWidth = 3)
    }
  }
  
  g
}