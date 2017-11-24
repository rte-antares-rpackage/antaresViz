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
                     colorScaleOpts, width = NULL, height = NULL, ...) {
  if (!timeStep %in% c("hourly", "daily")) {
    stop("Heatmap are only for daily and hourly data")
  }
  
  if (is.null(ylab)) ylab <- variable
  
  if (!is.null(dt$mcYear)) {
    dt <- dt[, .(value = mean(value)), by = .(timeId, time, element)]
  }
  
  wdaysLabels <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  wdaysAbbr <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  monthsAbbr <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep",
                  "oct", "nov", "dec")
  
  if (timeStep == "daily") {
    
    dt$weekId <- .getTimeId(dt$timeId * 24, "weekly", opts)
    dt$wday <- wdaysLabels[lubridate::wday(dt$time)]
    
    xCategories <- c(wdaysLabels,wdaysLabels)[which(wdaysLabels == opts$firstWeekday) + 0:6]
    
    # Text displayed on hover
    dt$hover <- sprintf(
      "%s %s %s (%s)<br>%s: %s",
      wdaysAbbr[lubridate::wday(dt$time)],
      lubridate::day(dt$time),
      monthsAbbr[lubridate::month(dt$time)],
      dt$timeId,
      ylab, dt$value
    )
    
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
    
    # Text displayed on hover
    dt$hover <- sprintf(
      "%s %s %s %02.f:00 (%s)<br>%s: %s",
      wdaysAbbr[lubridate::wday(dt$time)],
      lubridate::day(dt$time),
      monthsAbbr[lubridate::month(dt$time)],
      lubridate::hour(dt$time),
      dt$timeId,
      ylab, dt$value
    )
  }
  
  # Determine where to place tick marks on y-axis.
  wdayIds <- seq_along(xCategories)
  names(wdayIds) <- xCategories
  
  yticks <- dt[lubridate::day(time) == 1 & lubridate::hour(time) == 0, 
               .(time, weekId, wday)]
  yticks[, wdayId := wdayIds[wday]]
  
  if (is.factor(dt$element)) dt[, element := droplevels(element)]
  
  dt <- split(dt, f = dt$element)
  
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
    
    variable <- paste0(variable, collapse = " ; ")
    if (is.null(ylab)) ylab <- variable
    
    plot_ly(x) %>% config(displayModeBar = FALSE) %>% 
      add_heatmap(x=~wday, y=~weekId, z=~value, text = ~hover, 
                  colors = colorScaleFun, hoverinfo="text") %>% 
      layout(
        title = ~element[1],
        xaxis = list(
          title = "",
          categoryarray = xCategories, 
          categoryorder = "array",
          tickvals = paste0(wdaysLabels, ifelse(timeStep == "hourly", " 12:00", "")),
          ticktext = wdaysAbbr
        ),
        yaxis = list(
          title = "",
          tickvals = yticks$weekId + (yticks$wdayId - 1) / max(wdayIds) - 0.5,
          ticktext = monthsAbbr[lubridate::month(yticks$time)]
        )
      )
  })
  
  combineWidgets(list=unname(plots), title = main, width = width, height = height)
}