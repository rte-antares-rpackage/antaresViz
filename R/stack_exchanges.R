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
#' 
#' @details 
#' Compare argument can take following values :
#' \itemize{
#'    \item "mcYear"
#'    \item "main"
#'    \item "unit"
#'    \item "area"
#'    \item "legend"
#'    \item "stepPlot"
#'    \item "drawPoints"
#'    }
#'  
#' @examples 
#' \dontrun{
#' mydata <- readAntares(links = "all", timeStep = "daily")
#' exchangesStack(mydata)
#' 
#' # Also display exchanges with the rest of the world
#' mydata <- readAntares(areas = "all", links = "all", timeStep = "daily")
#' exchangesStack(mydata)
#' 
#' # Use compare :
#' exchangesStack(mydata, compare = "mcYear")
#' exchangesStack(mydata, compare = "area")
#' exchangesStack(mydata, compare = "unit")
#' exchangesStack(mydata, compare = "legend")
#' 
#' }
#' 
#' @export
exchangesStack <- function(x, area = NULL, mcYear = "average", 
                           dateRange = NULL, colors = NULL, 
                           main = NULL, ylab = NULL, unit = c("MWh", "GWh", "TWh"),
                           compare = NULL, compareOpts = list(),
                           interactive = getInteractivity(), 
                           legend = TRUE, legendId = sample(1e9, 1), groupId = legendId,
                           legendItemsPerRow = 5,
                           width = NULL, height = NULL,
                           xyCompare = c("union","intersect"),
                           h5requestFiltering = list(),
                           stepPlot = FALSE, drawPoints = FALSE,  
                           timeSteph5 = "hourly",
                           mcYearh5 = NULL,
                           tablesh5 = c("areas", "links"), ...) {
  
  
  if(!is.null(compare) && !interactive){
    stop("You can't use compare in no interactive mode")
  }
  
  
  
  #Check compare
  .validCompare(compare,  c("mcYear", "main", "unit", "area", "legend", "stepPlot", "drawPoints"))

  unit <- match.arg(unit)
  if (is.null(mcYear)) mcYear <- "average"
  
  init_area <- area

  xyCompare <- match.arg(xyCompare)
  
  init_dateRange <- dateRange
  
  if(!is.null(compare) && "list" %in% class(x)){
    if(length(x) == 1) x <- list(x[[1]], x[[1]])
  }
  if(!is.null(compare) && ("antaresData" %in% class(x)  | "simOptions" %in% class(x))){
    x <- list(x, x)
  }
  # .testXclassAndInteractive(x, interactive)

  
  h5requestFiltering <- .convertH5Filtering(h5requestFiltering = h5requestFiltering, x = x)
  
  # Generate a group number for dygraph objects
  if (!("dateRange" %in% compare)) {
    group <- sample(1e9, 1)
  } else {
    group <- NULL
  }
  
  compareOptions <- .compOpts(x, compare)
  if(is.null(compare)){
    if(compareOptions$ncharts > 1){
      compare <- list()
    }
  }
  
  processFun <- function(x) {
    if (!is(x, "antaresData")) stop("'x' should be an object of class 'antaresData created with readAntares()'")
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
    
    # should mcYear parameter be displayed on the UI?
    displayMcYear <- !attr(x, "synthesis") && length(unique(x$mcYear)) > 1
    
    timeStep <- attr(x, "timeStep")
    opts <- simOptions(x)
    
    dataDateRange <- as.Date(.timeIdToDate(range(x$timeId), timeStep, opts))
    if (length(init_dateRange) < 2) init_dateRange <- dataDateRange
    
    linksDef <- getLinks(namesOnly = FALSE, withDirection = TRUE, opts = opts)
    linksDef <- linksDef[link %in% x$link]
    areaList <- linksDef[, unique(area)]
    
    if (is.null(init_area)) init_area = areaList[1]
    
    plotFun <- function(id, area, dateRange, unit, mcYear, legend, stepPlot, drawPoints, main) {
      # Prepare data for stack creation
      a <- area
      linksDef <- getLinks(area, opts = simOptions(x), namesOnly = FALSE,
                           withDirection = TRUE)
      
      dt <- x
      
      if (mcYear == "average") {
        dt <- synthesize(dt)
        if (!is.null(row)) row <- row[, .(flow = mean(flow)), by = .(area, link, timeId, to, direction)]
      } else if ("mcYear" %in% names(x)) {
        mcy <- mcYear
        dt <- dt[mcYear == mcy]
        if (!is.null(row)) row <- row[mcYear == mcy, .(area, link, timeId, flow, to, direction)]
      }else{
        .printWarningMcYear()
        }
      
      dt <- merge(dt[as.Date(.timeIdToDate(timeId, timeStep, simOptions(x))) %between% dateRange,
                     .(link, timeId, flow = `FLOW LIN.`)],
                  linksDef, by = "link")
      if (!is.null(row)) {
        row <- row[as.Date(.timeIdToDate(timeId, timeStep, simOptions(x))) %between% dateRange]
        dt <- rbind(dt, row[area == a])
      }
      dt[, flow := flow * direction / switch(unit, MWh = 1, GWh = 1e3, TWh = 1e6)]
      
      if(nrow(dt) == 0){return(combineWidgets("No data"))}
      
      dt <- dcast(dt, timeId ~ to, value.var = "flow")
      
      # Graphical parameters
      if (is.null(main) | isTRUE(all.equal("", main))) main <- paste("Flows from/to", area)
      if (is.null(ylab)) ylab <- sprintf("Flows (%s)", unit)
      if (is.null(colors)) {
        colors <- substring(rainbow(ncol(dt) - 1, s = 0.7, v = 0.7), 1, 7)
      } else {
        colors <- rep(colors, length.out = ncol(dt - 1))
      }
      
      # Stack
      g <- .plotStack(dt, timeStep, opts, colors,
                      legendId = legendId + id - 1, groupId = groupId, 
                      main = main, ylab = ylab, stepPlot = stepPlot, drawPoints = drawPoints)
      
      if (legend) {
        # Add a nice legend
        legend <- tsLegend(names(dt)[-1], colors, types = "area", 
                           legendItemsPerRow = legendItemsPerRow, 
                           legendId = legendId + id - 1)
      } else legend <- NULL
      
      combineWidgets(g, footer = legend, width = width, height = height)
    }
    
    list(
      plotFun = plotFun,
      areaList = areaList,
      area = init_area,
      dataDateRange = dataDateRange,
      dateRange = init_dateRange,
      displayMcYear = displayMcYear,
      x = x
    )
  }
  
  if (!interactive) {
    x <- .cleanH5(x, timeSteph5, mcYearh5, tablesh5, h5requestFiltering)
    
    params <- .getDataForComp(.giveListFormat(x), NULL, compare, compareOpts, processFun = processFun)
    L_w <- lapply(params$x, function(X){
      X$plotFun(1, X$area, X$dateRange, unit, mcYear, legend, stepPlot, drawPoints, main)
    })
    return(combineWidgets(list = L_w))  
    
    
  }
  
  table <- NULL
  
  ##remove notes
  mcYearH5 <- NULL
  paramsH5 <- NULL
  sharerequest <- NULL
  timeStepdataload <- NULL
  timeSteph5 <- NULL
  x_in <- NULL
  x_tranform <- NULL
  
  
  manipulateWidget(
    {
      .tryCloseH5()
      if(.id <= length(params$x)){
        widget <- params$x[[max(1,.id)]]$plotFun(.id, area, dateRange, unit, mcYear, legend, stepPlot, drawPoints, main)
        controlWidgetSize(widget)
      } else {
        combineWidgets("No data for this selection")
      }
    },
    x = mwSharedValue(x),
    h5requestFiltering = mwSharedValue({h5requestFiltering}),
    
    x_in = mwSharedValue({
      .giveListFormat(x)
    }),
    
    paramsH5 = mwSharedValue({
      .h5ParamList(X_I = x_in, xyCompare = xyCompare, h5requestFilter = h5requestFiltering)
    }),
    
    H5request = mwGroup(
      timeSteph5 = mwSelect(choices = paramsH5$timeStepS, 
                            value =  paramsH5$timeStepS[1], 
                            label = "timeStep", 
                            multiple = FALSE
      ),
      mcYearH5 = mwSelect(choices = c(paramsH5[["mcYearS"]]), 
                         value = {
                           if(.initial){paramsH5[["mcYearS"]][1]}else{NULL}
                         }, 
                         label = "mcYear", 
                         multiple = TRUE
      ),
      .display = {
        any(unlist(lapply(x_in, .isSimOpts)))
      }
    ),
    
    sharerequest = mwSharedValue({
      list(timeSteph5_l = timeSteph5, mcYearh_l = mcYearH5, tables_l = NULL)
    }),
    

    x_tranform = mwSharedValue({
      areas = "all"
      links = "all"
      if(length(paramsH5$h5requestFilt[[1]]) > 0){
        areas <- NULL
        links <- NULL
      }
      sapply(1:length(x_in),function(zz){
        .loadH5Data(sharerequest, x_in[[zz]], areas = areas, links = links, h5requestFilter = paramsH5$h5requestFilter[[zz]])
      }, simplify = FALSE)
    }),
    
    mcYear = mwSelect({
     allMcY <-  c("average", if(!is.null(params)){
        as.character(.compareOperation(lapply(params$x, function(vv){
          unique(vv$x$mcYear)
        }), xyCompare))})
     allMcY
    }, 
    value = {
      if(.initial) mcYear
      else NULL
    }, 
    .display = {
      length(c("average", if(!is.null(params)){
        as.character(.compareOperation(lapply(params$x, function(vv){
          unique(vv$x$mcYear)
        }), xyCompare))})) != 1}
    ),
    
    area = mwSelect({
      if(!is.null(params)){
        as.character(.compareOperation(lapply(params$x, function(vv){
          unique(vv$areaList)
        }), xyCompare))}
    }, 
    value = {
      if(.initial) area
      else NULL
    }),
    
    dateRange = mwDateRange(value = {
      if(.initial){
        res <- NULL
        if(!is.null(params)){
          res <- c(.dateRangeJoin(params = params, xyCompare = xyCompare, "min", tabl = NULL),
                   .dateRangeJoin(params = params, xyCompare = xyCompare, "max", tabl = NULL))
        }

        ##Lock 7 days for hourly data
        if(!is.null(attributes(params$x[[1]]$x)$timeStep))
        {
        if(attributes(params$x[[1]]$x)$timeStep == "hourly"){
          if(params$x[[1]]$dateRange[2] - params$x[[1]]$dateRange[1]>7){
            res[1] <- params$x[[1]]$dateRange[2] - 7
          }
        }
        }
        
        res
      }else{NULL}
    }, 
    min = {      
      if(!is.null(params)){
        .dateRangeJoin(params = params, xyCompare = xyCompare, "min", tabl = NULL)
      }
    }, 
    max = {      
      if(!is.null(params)){
        .dateRangeJoin(params = params, xyCompare = xyCompare, "max", tabl = NULL)
      }
    },
    .display = timeStepdataload != "annual"
    ),
    
    unit = mwSelect(c("MWh", "GWh", "TWh"), unit),
    
    legend = mwCheckbox(legend),
    stepPlot = mwCheckbox(stepPlot),
    drawPoints = mwCheckbox(drawPoints), 
    timeStepdataload = mwSharedValue({
      attributes(x_tranform[[1]])$timeStep
    }),
    
    main = mwText(main, label = "title"),
    
    params = mwSharedValue({
      .getDataForComp(x_tranform, NULL, compare, compareOpts, 
                      processFun = processFun)
    }),
    
    .compare = {
      compare
    },
    .compareOpts = {
      compareOptions
    },
    ...
  )
  
}
