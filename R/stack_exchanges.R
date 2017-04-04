# Copyright © 2016 RTE Réseau de transport d’électricité

#' Plot the exchanges of an area
#' 
#' This function draws a stack representing the evolution of the exchanges of
#' an area with its neighbours. Positive values denotes exports and negative 
#' values imports.
#' 
#' @param x
#'   Object of class \code{antaresData} created with function
#'   \code{\link[antaresRead]{readAntares}}. It is required to contain link data.
#'   If it also contains area data with column `ROW BAL.`, then exchanges with
#'   the rest of the world are also displayed on the chart.
#' @param area
#'   Name of a single area. The flows from/to this area will be drawn by the
#'   function.
#' @param ylab Title of the Y-axis.
#' @inheritParams prodStack
#' 
#' @return 
#' A htmlwidget of class \code{dygraph}. It can be modified with functions from
#' package \code{dygraphs}.
#' 
#' @examples 
#' \dontrun{
#' mydata <- readAntares(links = "all", timeStep = "daily")
#' exchangeStack(mydata)
#' 
#' # Also display exchanges with the rest of the world
#' mydata <- readAntares(areas = "all", links = "all", timeStep = "daily")
#' exchangesStack(mydata)
#' }
#' 
#' @export
exchangesStack <- function(x, area = NULL, mcYear = NULL, dateRange = NULL, colors = NULL, 
                           main = NULL, ylab = NULL, unit = c("MWh", "GWh", "TWh"),
                           interactive = base::interactive(), 
                           legend = TRUE, legendId = sample(1e9, 1),
                           legendItemsPerRow = 5,
                           width = NULL, height = NULL) {
  
  if (!is(x, "antaresData")) stop("'x' should be an object of class 'antaresData created with readAntares()'")
  if (is.null(mcYear)) mcYear <- "synthesis"
  row <- NULL # exchanges with rest of the world
  
  if (is(x, "antaresDataTable")) {
    if (!attr(x, "type") == "links") stop("'x' should contain link data")
  } else if (is(x, "antaresDataList")) {
    if (is.null(x$links)) stop("'x' should contain link data")
    
    # If they are present, add the echanges with the rest of the world
    if (!is.null(x$areas) && !is.null(x$areas$`ROW BAL.`)) {
      if ("mcYear" %in% names(x$areas)) {
        row <- x$areas[, .(area, link = paste(area, " - ROW"), timeId, mcYear, 
                           flow = - `ROW BAL.`, to = "ROW", direction = 1)]
      } else {
        row <- x$areas[, .(area, link = paste(area, " - ROW"), timeId, 
                           flow = - `ROW BAL.`, to = "ROW", direction = 1)]
      }
      
    }
    x <- x$links
  }
  
  # shoudl mcYear parameter be displayed on the UI?
  displayMcYear <- !attr(x, "synthesis") && length(unique(x$mcYear)) > 1
  
  unit <- match.arg(unit)
  timeStep <- attr(x, "timeStep")
  opts <- simOptions(x)
  
  dataDateRange <- as.Date(.timeIdToDate(range(x$timeId), timeStep, opts))
  if (length(dateRange) < 2) dateRange <- dataDateRange
  
  linksDef <- getLinks(namesOnly = FALSE, withDirection = TRUE, opts = opts)
  linksDef <- linksDef[link %in% x$link]
  areaList <- linksDef[, unique(area)]
  
  if (is.null(area)) area = areaList[1]
  
  plotFun <- function(area, dateRange, unit, mcYear) {
    # Prepare data for stack creation
    a <- area
    linksDef <- getLinks(area, opts = simOptions(x), namesOnly = FALSE,
                         withDirection = TRUE)
    
    dt <- x
    
    if (mcYear == "synthesis") {
      dt <- synthesize(dt)
      row <- row[, .(flow = mean(flow)), by = .(area, link, timeId, to, direction)]
    } else if ("mcYear" %in% names(x)) {
      mcy <- mcYear
      dt <- dt[mcYear == mcy]
      if (!is.null(row)) row <- row[mcYear == mcy, .(area, link, timeId, flow, to, direction)]
    } 
    dt <- merge(dt[as.Date(.timeIdToDate(timeId, timeStep, simOptions(x))) %between% dateRange,
                  .(link, timeId, flow = `FLOW LIN.`)],
                linksDef,
                by = "link")
    if (!is.null(row)) dt <- rbind(dt, row[area == a])
    dt[, flow := flow * direction / switch(unit, MWh = 1, GWh = 1e3, TWh = 1e6)]
    
    dt <- dcast(dt, timeId ~ to, value.var = "flow")
    
    # Graphical parameters
    if (is.null(main)) main <- paste("Flows from/to", area)
    if (is.null(ylab)) ylab <- sprintf("Flows (%s)", unit)
    if (is.null(colors)) {
      colors <- substring(rainbow(ncol(dt) - 1, s = 0.7, v = 0.7), 1, 7)
    } else {
      colors <- rep(colors, length.out = ncol(dt - 1))
    }
    
    # Stack
    g <- .plotStack(dt, timeStep, opts, colors,
                    legendId = legendId, main = main, ylab = ylab)
    
    if (legend) {
      # Add a nice legend
      legend <- tsLegend(names(dt)[-1], colors, types = "area", 
                         legendItemsPerRow = legendItemsPerRow, 
                         legendId = legendId)
    } else legend <- NULL
    
    
    combineWidgets(g, footer = legend, width = width, height = height)
    
  }
  
  if (!interactive) return(plotFun(area, dateRange, unit, mcYear))
  
  manipulateWidget(
    plotFun(area, dateRange, unit, mcYear),
    mcYear = mwSelect(c("synthesis", unique(x$mcYear)), mcYear),
    area = mwSelect(areaList, area),
    dateRange = mwDateRange(dateRange, min = dataDateRange[1], max = dataDateRange[2]),
    unit = mwSelect(c("MWh", "GWh", "TWh"), unit),
    .display = list(mcYear = displayMcYear)
  )
  
}
