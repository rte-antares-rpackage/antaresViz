#' @export
plot.antaresDataTable <- function(x, variable, main, ylab, ...) {
  if (missing(variable)) stop("Specify a variable to plot")
  
  if (missing(ylab)) ylab <- variable
  
  idVars <- names(x)
  timeStep <- attr(x, "timeStep")
  
  # Prepare data to plot
  # First, we construct a table with three columns:
  # - time: POSIXct or Date
  # - colvar: factor representing the name of each "element" in the input data.
  #           each element is represented with a different curve or bar in the
  #           final graphic.
  # - variable: values to visualize
  #
  # Then we reshape the data to have one line per date/time and one column per
  # "element". Cells contain the variable to visualize.
  dt <- x[, .(timeId, var = get(variable))]
  dt$time <- .timeIdToDate(dt$timeId, timeStep = timeStep)
  
  if ("cluster" %in% idVars) {
    dt$colvar <- paste(x$area, x$cluster)
  } else if ("district" %in% idVars) {
    dt$colvar <- x$district
  } else if ("link" %in% idVars) {
    dt$colvar <- x$link
  } else if ("area" %in% idVars) {
    dt$colvar <- x$area
  } else stop("No Id column")
  
  if ("mcYear" %in% names(x) && length(unique(x$mcYear)) > 1) {
    dt$mcYear <- x$mcYear
    
    uniqueColvar <- sort(unique(dt$colvar))
    
    dt <- dt[, .(var = c(mean(var), quantile(var, c(0.025, 0.975))),
                 suffix = c("", "_l", "_u")), 
             by = .(time, colvar)]
    dt[, colvar := paste0(colvar, suffix)]
  }
  
  if (timeStep != "annual") {
    # Plot time series curves, unless data is annual
    dt <- dcast(dt, time ~ colvar, value.var = "var")
    if (missing(main)) main <- paste("Evolution of", variable)
    
    g <- dygraph(dt, main = main) %>% 
      dyOptions(
        includeZero = TRUE, 
        gridLineColor = gray(0.8), 
        axisLineColor = gray(0.6), 
        axisLabelColor = gray(0.6), 
        labelsKMB = TRUE
      ) %>% 
      dyAxis("y", label = ylab, pixelsPerLabel = 60) %>% 
      dyRangeSelector()
    
    if (exists("uniqueColvar")) {
      for (v in uniqueColvar) {
        g <- g %>% dySeries(paste0(v, c("_l", "", "_u")))
      }
    }
    
  } else {
    # If data is annual, plot a barchart to compare elements
    if (missing(main)) main <- paste("Comparison of", variable)
    
    g <- highchart() %>%
      hc_yAxis(title = list(text = ylab)) %>% 
      hc_legend(enabled = FALSE) %>% 
      hc_title(text = main)
    
    if (!exists("uniqueColvar")) {
      g <- g %>% hc_xAxis(categories = dt$colvar) %>%
      hc_add_serie(g, name = variable, data = dt$var, type = "column") 
    } else {
      g <- g %>% hc_xAxis(categories = dt[suffix == "", colvar]) %>%
        hc_add_serie(name = variable, data = dt[suffix == "", var], type = "column") %>% 
        hc_add_serie(name = "range", 
                     data = cbind(dt[suffix == "_l", var], dt[suffix == "_u", var]), 
                     type = "errorbar")
    }
  }
  
  g
} 