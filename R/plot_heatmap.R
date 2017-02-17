#' Private function that draws heatmaps for a given time series
#' 
#' @param dt data.table with at least colums element, timeId and value
#' @param timeStep timeStep
#' @param variable variable name
#' @param main title of the generated plot
#' @param ylab title of the y axis
#' @param opts simOptions
#' @param colorScaleOpts list of parameters for function continuousColorPal
#' @param ... Unused
#'
#' @noRd
.heatmap <- function(dt, timeStep, variable, main = NULL, ylab = NULL, opts,
                     colorScaleOpts, ...) {
  if (!timeStep %in% c("hourly", "daily")) {
    stop("Heatmap are only for daily and hourly data")
  }
  
  if (!is.null(dt$mcYear)) {
    dt <- dt[, .(value = mean(value)), by = .(timeId, time, element)]
  }
  
  wdaysLabels <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  
  if (timeStep == "daily") {
    
    dt$weekId <- .getTimeId(dt$timeId * 24, "weekly", opts)
    dt$wday <- wdaysLabels[lubridate::wday(dt$time)]
    
    xCategories <- c(wdaysLabels,wdaysLabels)[which(wdaysLabels == opts$firstWeekday) + 0:6]
    
  } else if (timeStep == "hourly") {
    dt$weekId <- .getTimeId(dt$timeId, "weekly", opts)
    dt$wday <- wdaysLabels[lubridate::wday(dt$time)]
    dt$hour <- sprintf("%02.0f:00", lubridate::hour(dt$time))
    dt$wday <- paste(dt$wday, dt$hour)
    
    hours <- sprintf("%02.0f:00", 0:23)
    
    xCategories <- c(wdaysLabels,wdaysLabels)[which(wdaysLabels == opts$firstWeekday) + 0:6]
    xCategories <- lapply(xCategories, function(x) {
      paste(x, hours)
    })
    xCategories <- do.call(c, xCategories)
    
  }
  
  dt <- split(dt, f = droplevels(dt$element))
  
  plots <- lapply(dt, function(x) {
    rangevar <- range(x$value)
    if (rangevar[1] >= 0) {
      domain <- rangevar
    } else {
      domain <- c(-max(abs(rangevar)), max(abs(rangevar)))
    }
    
    colorScaleOpts$domain <- domain
    colorScaleOpts$x <- x$value
    
    colorPalette <- do.call(continuousColorPal, colorScaleOpts)
    minVal <- rangevar[1]
    scale <- diff(rangevar)
    colorScaleFun <- function(x) {
      x <- scale * x + minVal
      col <- attr(colorPalette, "pal")
      breaks <- attr(colorPalette, "breaks")
      
      res <- rep(col[1], length(x))
      for (i in 2:(length(breaks) - 1)) {
        res[x > breaks[i]] <- col[i]
      }
      res
    }
    
    if (is.null(ylab)) ylab <- variable
    
    plot_ly(x) %>% config(displayModeBar = FALSE) %>% 
      add_heatmap(x=~wday, y=~weekId, z=~value, colors = colorScaleFun) %>% 
      layout(
        title = ~element[1],
        xaxis = list(
          title = "",
          categoryarray = xCategories, 
          categoryorder = "array",
          tickvals = paste0(wdaysLabels, ifelse(timeStep == "hourly", " 12:00", "")),
          ticktext = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
        ),
        yaxis = list(title = ylab)
      )
  })
  
  combineWidgets(list=plots, title = main)
}