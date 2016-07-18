#' @export
plot.antaresDataTable <- function(x, variable, main, ylab) {
  if (missing(variable)) stop("Specify a variable to plot")
  
  if (missing(ylab)) ylab <- variable
  
  idVars <- names(x)
  timeStep <- attr(x, "timeStep")
  
  dt <- x[, c("timeId", variable), with = FALSE]
  dt$time <- antaresRead:::.timeIdToDate(dt$timeId, timeStep = timeStep)
  
  if ("cluster" %in% idVars) {
    dt$colvar <- paste(x$area, x$cluster)
  } else if ("district" %in% idVars) {
    dt$colvar <- x$district
  } else if ("link" %in% idVars) {
    dt$colvar <- x$link
  } else if ("area" %in% idVars) {
    dt$colvar <- x$area
  } else stop("No Id column")
  
  if ("mcYear" %in% names(x)) dt[, colvar := sprintf("%s mc:%s", colvar, x$mcYear)]
  
  if (timeStep != "annual") {
    # If time step is annual plot a time series
    dt <- dcast(dt, time ~ colvar, value.var = variable)
    if (missing(main)) main <- paste("Evolution of", variable)
    
    dygraph(dt, main = main) %>% 
      dyOptions(
        includeZero = TRUE, 
        gridLineColor = gray(0.8), 
        axisLineColor = gray(0.6), 
        axisLabelColor = gray(0.6), 
        labelsKMB = TRUE
      ) %>% 
      dyAxis("y", label = ylab, pixelsPerLabel = 60) %>% 
      dyRangeSelector()
    
  } else {
    # else plot a barchart to compare elements
    if (missing(main)) main <- paste("Comparison of", variable)
    
    highchart() %>%
      hc_xAxis(categories = dt$colvar) %>%
      hc_yAxis(title = list(text = ylab)) %>% 
      hc_add_serie(data = dt[[variable]], type = "column") %>% 
      hc_legend(enabled = FALSE) %>% 
      hc_title(text = main)
  }
  
} 