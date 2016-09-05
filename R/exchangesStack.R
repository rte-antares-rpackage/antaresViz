exchangesStack <- function(x, area = NULL, dateRange = NULL, colors = NULL, 
                            main = NULL, ylab = NULL, unit = c("MWh", "GWh", "TWh"),
                            width = "100%", height = "500px",
                            interactive = base::interactive(), 
                            legend = TRUE, legendId = sample(1e9, 1),
                            legendItemsPerRow = 5) {
  
  unit <- match.arg(unit)
  timeStep <- attr(x, "timeStep")
  opts <- simOptions(x)
  
  dataDateRange <- as.Date(.timeIdToDate(range(x$timeId), timeStep, opts))
  if (length(dateRange) < 2) dateRange <- dataDateRange
  
  linksDef <- getLinks(namesOnly = FALSE, withDirection = TRUE, opts = opts)
  linksDef <- linksDef[link %in% x$link]
  areaList <- linksDef[, unique(area)]
  
  if (is.null(area)) area = areaList[1]
  
  plotFun <- function(area, dateRange, unit) {
    # Prepare data for stack creation
    linksDef <- getLinks(area, opts = simOptions(x), namesOnly = FALSE,
                         withDirection = TRUE)
    dt <- merge(x[as.Date(.timeIdToDate(timeId, timeStep)) %between% dateRange,
                  .(link, timeId, flow = `FLOW LIN.`)],
                linksDef,
                by = "link")
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
    
    if (!legend) {
      return(g)
    }
    
    # Add a nice legend
    combineWidgets(vflex = c(1, NA),
                   g,
                   tsLegend(names(dt)[-1], colors,  
                            itemsByRow = legendItemsPerRow, 
                            legendId = legendId)
    )
    
  }
  
  if (!interactive) return(plotFun(area, dateRange, unit))
  
  manipulateWidget(
    plotFun(area, dateRange, unit),
    area = mwSelect(areaList, area),
    dateRange = mwDateRange(dateRange, min = dataDateRange[1], max = dataDateRange[2]),
    unit = mwSelect(c("MWh", "GWh", "TWh"), unit)
  )
  
}