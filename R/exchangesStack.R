exchangesStack <- function(x, area = NULL, colors = NULL, 
                            main = NULL, ylab = NULL, unit = c("MWh", "GWh", "TWh"),
                            width = "100%", height = "500px",
                            interactive = base::interactive(), 
                            legend = TRUE, legendId = sample(1e9, 1),
                            legendItemsPerRow = 5) {
  
  unit <- match.arg(unit)
  
  linksDef <- getLinks(namesOnly = FALSE, withDirection = TRUE, opts = simOptions(x))
  linksDef <- linksDef[link %in% x$link]
  areaList <- linksDef[, unique(area)]
  
  if (is.null(area)) area = areaList[1]
  
  plotFun <- function(area, unit) {
    linksDef <- getLinks(area, opts = simOptions(x), namesOnly = FALSE,
                         withDirection = TRUE)
    dt <- merge(x[, .(link, timeId, flow = `FLOW LIN.`)],
                linksDef,
                by = "link")
    dt[, flow := flow * direction]
    
    dt <- dcast(dt, timeId ~ to, value.var = "flow")
    
    if (is.null(main)) main <- paste("Flows from/to", area)
    if (is.null(ylab)) ylab <- sprintf("Flows (%s)", unit)
    if (is.null(colors)) {
      colors <- substring(rainbow(ncol(dt) - 1, s = 0.7, v = 0.7), 1, 7)
    } else {
      colors <- rep(colors, length.out = ncol(dt - 1))
    }
    
    g <- .plotStack(dt, attr(x, "timeStep"), simOptions(x), colors,
                    legendId = legendId, main = main, ylab = ylab)
    
    if (!legend) return(g)
    
    combineWidgets(vflex = c(1, NA),
                   g,
                   tsLegend(names(dt)[-1], colors,  
                            itemsByRow = legendItemsPerRow, 
                            legendId = legendId)
    )
    
  }
  
  if (!interactive) return(plotFun(area, unit))
  
  manipulateWidget(
    plotFun(area, unit),
    area = mwSelect(areaList, area),
    unit = mwSelect(c("MWh", "GWh", "TWh"), unit)
  )
  
}