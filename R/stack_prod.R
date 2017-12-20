# Copyright © 2016 RTE Réseau de transport d’électricité

#' Visualize the production stack of an area
#' 
#' \code{prodStack} draws the production stack for a set of areas or districts.
#' User can see available stacks with \code{prodStackAliases} and create new ones
#' with \code{setProdStackAlias}.
#' 
#' @param x
#'   An object of class \code{antaresData} created with function 
#'   \code{\link[antaresRead]{readAntares}} containing data for areas and or
#'   districts. it can be a list of \code{antaresData} objects. 
#'   In this case, one chart is created for each object. 
#'   Can also contains opts who refer to a h5 file or list of opts.
#' @param stack
#'   Name of the stack to use. One can visualize available stacks with 
#'   \code{prodStackAliases}
#' @param areas
#'   Vector of area or district names. The data of these areas or districts is
#'   aggregated by the function to construct the production stack.
#' @param mcYear
#'   If \code{x}, contains multiple Monte-Carlo scenarios, this parameter 
#'   determine which scenario is displayed. Must be an integer representing the
#'   index of the scenario or the word "average". In this case data are 
#'   averaged.
#' @param dateRange
#'   A vector of two dates. Only data points between these two dates are 
#'   displayed. If NULL, then all data is displayed.
#' @param main
#'   Title of the graph.
#' @param unit
#'   Unit used in the graph. Possible values are "MWh", "GWh" or "TWh".
#' @param compare
#'   An optional character vector containing names of parameters. When it is set,
#'   two charts are outputed with their own input controls. Alternatively, it can
#'   be a named list with names corresponding to parameter names and values being
#'   list with the initial values of the given parameter for each chart. See details
#'    if you are drawing a map.
#' @param compareOpts
#'   List of options that indicates the number of charts to create and their 
#'   position. Check out the documentation of 
#'   \code{\link[manipulateWidget]{compareOptions}} to see available options.
#' @param width
#'   Width of the graph expressed in pixels or in percentage of 
#'   the parent element. For instance "500px" and "100\%" are valid values.
#' @param height
#'   Height of the graph expressed in pixels or in percentage of 
#'   the parent element. For instance "500px" and "100\%" are valid values.
#' @param interactive
#'   LogicalValue. If \code{TRUE}, then a shiny gadget is launched that lets
#'   the user interactively choose the areas or districts to display.
#' @param legend
#'   Logical value indicating if a legend should be drawn. This argument is 
#'   usefull when one wants to create a shared legend with
#'   \code{\link{prodStackLegend}}
#' @param legendId Id of the legend linked to the graph. This argument is 
#'   usefull when one wants to create a shared legend with 
#'   \code{\link{prodStackLegend}}
#' @param groupId Parameter that can be used to synchronize the horizontal 
#'   zoom of multiple charts. All charts that need to be synchronized must
#'   have the same group. 
#' @param legendItemsPerRow
#'   Number of elements to put in each row of the legend.
#' @param name
#'   name of the stack to create or update
#' @param variables
#'   A named list of expressions created with \code{\link[base]{alist}}. The
#'   name of each element is the name of the variable to draw in the stacked
#'   graph. The element itself is an expression explaining how to compute the
#'   variable (see examples).
#' @param colors
#'   Vector of colors with same length as parameter \code{variables}. If 
#'   \code{variables} is an alias, then this argument should be \code{NULL} in 
#'   order to use default colors.
#' @param lines
#'   A named list of expressions created with \code{\link[base]{alist}}
#'   indicating how to compute the curves to display on top of the stacked graph.
#'   It should be \code{NULL} if there is no curve to trace or if parameter
#'   \code{variables} is an alias.
#' @param lineColors
#'   Vector of colors with same length as parameter \code{lines}. This argument
#'   should be \code{NULL} if there is no curve to trace or if parameter
#'   \code{variables} is an alias.
#' @param description
#'   Description of the stack. It is displayed by function 
#'   \code{prodStackAliases}.
#' @param xyCompare
#'   Use when you compare studies, can be "union" or "intersect". If union, all
#'   of mcYears in one of studies will be selectable. If intersect, only mcYears in all
#'   studies will be selectable.
#' @param h5requestFiltering Contains arguments used by default for h5 request,
#'   typically h5requestFiltering = list(select = "NUCLEAR")
#' @param stepPlot \code{boolean}, step style for curves.
#' @param drawPoints \code{boolean}, add points on graph
#' @param timeSteph5 \code{character} timeStep to read in h5 file. Only for Non interactive mode.
#' @param mcYearh5 \code{numeric} mcYear to read for h5. Only for Non interactive mode.
#' @param tablesh5 \code{character} tables for h5 ("areas" "links", "clusters" or "disticts"). Only for Non interactive mode.
#' @param ... Other arguments for \code{\link{manipulateWidget}}
#'  
#' @return 
#' \code{prodStack} returns an interactive html graphic. If argument
#' \code{interactive} is \code{TRUE}, then a shiny gadget is started and the
#' function returns an interactive html graphic when the user clicks on button
#' "Done".
#' 
#' \code{prodStackAliases} displays the list of available aliases.
#' 
#' \code{setProdStackAlias} creates or updates a stack alias.
#' 
#' @seealso \code{\link{prodStackLegend}}
#' 
#' @details 
#' compare argument can take following values :
#' \itemize{
#'    \item "mcYear"
#'    \item "main"
#'    \item "unit"
#'    \item "areas"
#'    \item "legend"
#'    \item "stack"
#'    \item "stepPlot"
#'    \item "drawPoints"
#'    }
#' @examples
#' \dontrun{
#' mydata <- readAntares(areas = "all", timeStep = "daily")
#' 
#' # Start a shiny gadget that permits to choose areas to display.
#' prodStack(x = mydata, unit = "GWh")
#' 
#' # Use in a non-interactive way
#' prodStack(x = mydata, unit = "GWh", areas = "fr", interactive = FALSE)
#' 
#' # Define a custom stack
#' setProdStackAlias(
#'   name = "Wind and solar",
#'   variables = alist(wind = WIND, solar = SOLAR),
#'   colors = c("green", "orange")
#' )
#' 
#' prodStack(x = mydata, unit = "GWh", stack = "Wind and solar")
#'                 
#' # In a custom stack it is possible to use computed values
#' setProdStackAlias(
#'   name = "Renewable",
#'   variables = alist(
#'     renewable = WIND + SOLAR + `H. ROR` + `H. STOR` + `MISC. NDG`, 
#'     thermal = NUCLEAR + LIGNITE + COAL + GAS + OIL + `MIX. FUEL` + `MISC. DTG`
#'   ),
#'   colors = c("green", gray(0.3)),
#'   lines = alist(goalRenewable = LOAD * 0.23),
#'   lineColors = "#42EB09"
#' )
#' 
#' prodStack(x = mydata, unit = "GWh", stack = "renewable")
#' 
#' # Use compare
#' prodStack(x = mydata, compare = "areas")
#' prodStack(x = mydata, unit = "GWh", compare = "mcYear")
#' prodStack(x = mydata, unit = "GWh", compare = "main")
#' prodStack(x = mydata, unit = "GWh", compare = "unit")
#' prodStack(x = mydata, unit = "GWh", compare = "areas")
#' prodStack(x = mydata, unit = "GWh", compare = "legend")
#' prodStack(x = mydata, unit = "GWh", compare = "stack")
#' prodStack(x = mydata, unit = "GWh", compare = c("mcYear", "areas"))
#' 
#' 
#' # Compare studies
#' prodStack(list(mydata, mydata))
#' 
#' 
#' # Use h5 opts
#' # Set path of simulaiton
#' setSimulationPath(path = path1)
#' 
#' # Convert your study in h5 format
#' writeAntaresH5(path = mynewpath)
#' 
#' # Redefine sim path with h5 file
#' opts <- setSimulationPath(path = mynewpath)
#' prodStack(x = opts)
#' 
#' # Compare elements in a single study
#' prodStack(x = opts, .compare = "mcYear")
#' 
#' # Compare 2 studies
#' prodStack(x = list(opts, opts2))
#' 
#' 
#'                 
#' }
#' 
#' @export
prodStack <- function(x,
                      stack = "eco2mix",
                      areas = NULL, 
                      mcYear = "average",
                      dateRange = NULL,
                      main = "Production stack", unit = c("MWh", "GWh", "TWh"),
                      compare = NULL,
                      compareOpts = list(),
                      interactive = getInteractivity(), 
                      legend = TRUE, legendId = sample(1e9, 1),
                      groupId = legendId,
                      legendItemsPerRow = 5,
                      width = NULL, height = NULL, xyCompare = c("union","intersect"),
                      h5requestFiltering = list(), stepPlot = FALSE, drawPoints = FALSE,
                      timeSteph5 = "hourly",
                      mcYearh5 = NULL,
                      tablesh5 = c("areas", "links"),...) {
  
  if(!is.null(compare) && !interactive){
    stop("You can't use compare in no interactive mode")
  }
  
  #Check compare
  .validCompare(compare,  c("mcYear", "main", "unit", "areas", "legend", "stack", "stepPlot", "drawPoints"))

  xyCompare <- match.arg(xyCompare)
  unit <- match.arg(unit)
  if (is.null(mcYear)) mcYear <- "average"
  
  if(!is.null(compare) && "list" %in% class(x)){
    if(length(x) == 1) x <- list(x[[1]], x[[1]])
  }
  if(!is.null(compare) && ("antaresData" %in% class(x)  | "simOptions" %in% class(x))){
    x <- list(x, x)
  }
  
  # .testXclassAndInteractive(x, interactive)
  
  h5requestFiltering <- .convertH5Filtering(h5requestFiltering = h5requestFiltering, x = x)
  
  compareOptions <- .compOpts(x, compare)
  if(is.null(compare)){
    if(compareOptions$ncharts > 1){
      compare <- ""
    }
  }
  
  init_areas <- areas
  init_dateRange <- dateRange
  
  processFun <- function(x) {
    
    # Check that input contains area or district data
    if (!is(x, "antaresData")) stop("'x' should be an object of class 'antaresData created with readAntares()'")
    
    if (is(x, "antaresDataTable")) {
      if (!attr(x, "type") %in% c("areas", "districts")) stop("'x' should contain area or district data")
    } else if (is(x, "antaresDataList")) {
      if (is.null(x$areas) & is.null(x$districts)) stop("'x' should contain area or district data")
      if (!is.null(x$areas)) x <- x$areas
      else x <- x$districts
    }
    
    if (is.null(x$area)) x$area <- x$district
    timeStep <- attr(x, "timeStep")
    opts <- simOptions(x)
    if (is.null(init_areas)) {
      init_areas <- unique(x$area)[1]
    }
    
    # should mcYear parameter be displayed on the UI?
    displayMcYear <- !attr(x, "synthesis") && length(unique(x$mcYear)) > 1
    
    dataDateRange <- as.Date(.timeIdToDate(range(x$timeId), timeStep, opts))
    if (length(init_dateRange) < 2) init_dateRange <- dataDateRange
    
    plotWithLegend <- function(id, areas, main = "", unit, stack, dateRange, mcYear, legend, stepPlot, drawPoints) {
      if (length(areas) == 0) return (combineWidgets("Please choose an area"))
      stackOpts <- .aliasToStackOptions(stack)
      dt <- x[area %in% areas]
      
      if (mcYear == "average") dt <- synthesize(dt)
      else if ("mcYear" %in% names(dt)) {
        mcy <- mcYear
        dt <- dt[mcYear == mcy]
      }else{
        .printWarningMcYear()
      }
      
      if (!is.null(dateRange)) {
        dt <- dt[as.Date(.timeIdToDate(dt$timeId, timeStep, opts = opts)) %between% dateRange]
      }
      
      if(nrow(dt) == 0){
        return (combineWidgets("No data for this selection"))
      }
      p <- try(.plotProdStack(dt,
                              stackOpts$variables,
                              stackOpts$colors,
                              stackOpts$lines,
                              stackOpts$lineColors,
                              main = main,
                              unit = unit,
                              legendId = legendId + id - 1,
                              groupId = groupId,
                              dateRange = dateRange,
                              stepPlot = stepPlot, drawPoints = drawPoints), silent = TRUE)
      
      if("try-error" %in% class(p)){
        return (combineWidgets(paste0("Can't visualize stack '", stack, "'<br>", p[1])))
      }
      
      if (legend) {
        l <- prodStackLegend(stack, legendItemsPerRow, legendId = legendId + id - 1)
      } else {
        l <- NULL
      }
      
      combineWidgets(p, footer = l, width = width, height = height)
    }
    
    list(
      plotWithLegend = plotWithLegend,
      x = x,
      timeStep = timeStep,
      opts = opts,
      areas = init_areas,
      displayMcYear = displayMcYear,
      dataDateRange = dataDateRange,
      dateRange = init_dateRange
    )
  }
  if (!interactive) {
    x <- .cleanH5(x, timeSteph5, mcYearh5, tablesh5, h5requestFiltering)
    
    
    params <- .getDataForComp(x = .giveListFormat(x),
                              y = NULL, compare = compare,
                              compareOpts = compareOptions,
                              processFun = processFun)
    
    
    
    L_w <- lapply(params$x, function(X){
      X$plotWithLegend(1, areas, main, unit,
                       stack, params$x[[1]]$dateRange,
                       mcYear, legend, stepPlot, drawPoints)
    })
    return(combineWidgets(list = L_w))
    
  } else {
    # just init for compare & compareOpts
    # init_params <- .getDataForComp(x, y, compare, compareOpts, function(x) {})
  }
  
  
  table <- NULL
  
  ##remove notes
  mcYearhH5 <- NULL
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
        widget <- params$x[[max(1,.id)]]$plotWithLegend(.id, areas, main,
                                                        unit, stack, dateRange,
                                                        mcYear, legend,
                                                        stepPlot, drawPoints)
        controlWidgetSize(widget)
      } else {
        combineWidgets("No data for this selection")
      }
    },
    x = mwSharedValue({x}),
    x_in = mwSharedValue({
      .giveListFormat(x)
    }),
    h5requestFiltering = mwSharedValue({
      h5requestFiltering
    }),
    paramsH5 = mwSharedValue({
      tmp <- .h5ParamList(X_I = x_in, xyCompare = xyCompare,
                          h5requestFilter = h5requestFiltering)
      tmp
    }),
    H5request = mwGroup(
      timeSteph5 = mwSelect(choices = paramsH5$timeStepS, 
                            value =  paramsH5$timeStepS[1], 
                            label = "timeStep", 
                            multiple = FALSE
      ),
      tables = mwSelect(choices = paramsH5[["tabl"]][paramsH5[["tabl"]]%in%c("areas", "districts")], 
                        value = {
                          if(.initial) {paramsH5[["tabl"]][paramsH5[["tabl"]]%in%c("areas", "districts")][1]}else{NULL}
                        }, 
                        label = "table", 
                        multiple = FALSE
      ),
      mcYearhH5 = mwSelect(choices = c(paramsH5[["mcYearS"]]), 
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
      list(timeSteph5_l = timeSteph5, mcYearh_l = mcYearhH5, tables_l = tables)
    }),
    

    
    x_tranform = mwSharedValue({
      
      h5requestFilteringTp <- paramsH5$h5requestFilter
      if(!is.null(sharerequest))
      {
        for(i in 1:length(h5requestFilteringTp))
        {
          if(sharerequest$tables == "areas"){
            h5requestFilteringTp[[i]]$districts = NULL
          }
          if(sharerequest$tables == "districts"){
            h5requestFilteringTp[[i]]$areas = NULL
          }
        }
      }

      sapply(1:length(x_in),function(zz){
        .loadH5Data(sharerequest, x_in[[zz]], h5requestFilter = h5requestFilteringTp[[zz]])
      }, simplify = FALSE)
    }),
    
    params = mwSharedValue({
      .getDataForComp(x_tranform, NULL, compare,
                      compareOpts = compareOptions, 
                      processFun = processFun)
    }),
    
    ##End h5
    mcYear = mwSelect({
      c("average",  .compareOperation(lapply(params$x, function(vv){
        unique(vv$x$mcYear)
      }), xyCompare))
    }),
    
    main = mwText(main, label = "title"),
    
    dateRange = mwDateRange(value = {
      if(.initial){
        res <- NULL
        if(!is.null(params)){
          res <- c(.dateRangeJoin(params = params, xyCompare = xyCompare, "min", tabl = table),
                   .dateRangeJoin(params = params, xyCompare = xyCompare, "max", tabl = table))
          ##Lock 7 days for hourly data
          if(params$x[[1]]$timeStep == "hourly"){
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
        .dateRangeJoin(params = params, xyCompare = xyCompare, "min", tabl = table)
      }
    }, 
    max = {      
      if(!is.null(params)){
        .dateRangeJoin(params = params, xyCompare = xyCompare, "max", tabl = table)
      }
    }
    ),
    
    
    
    stack = mwSelect(names(pkgEnv$prodStackAliases), stack),
    
    unit = mwSelect(c("MWh", "GWh", "TWh"), unit),
    
    areas = mwSelect({
      as.character(.compareOperation(lapply(params$x, function(vv){
        unique(vv$x$area)
      }), xyCompare))
    },
    value = {
      if(.initial){
        as.character(.compareOperation(lapply(params$x, function(vv){
          unique(vv$x$area)
        }), xyCompare))[1]
      }
      else{NULL}},
    multiple = TRUE
    ),
    
    legend = mwCheckbox(legend),
    stepPlot = mwCheckbox(stepPlot),
    drawPoints = mwCheckbox(drawPoints),
    .compare = {
      compare
    },
    .compareOpts = {
      compareOptions
    },
    ...
  )
}


#' Returns the variables and colors corresponding to an alias
#' 
#' @param variables
#'   character string représenting an alias
#'   
#' @return 
#' This function returns a list with four components:
#' \item{variables}{Definition of the variables of the stack}
#' \item{colors}{colors for the variables}
#' \item{lines}{Definition of the curves to draw on top of the production stack}
#' \item{lineColors}{colors for the curves}
#' @noRd
.aliasToStackOptions <- function(variables) {
  if (! variables %in% names(pkgEnv$prodStackAliases)) {
    stop("Unknown alias '", variables, "'.")
  }
  pkgEnv$prodStackAliases[[variables]]
}

#' Generate an interactive stack
#' 
#' @param x
#'   data.table of class "antaresDataTable" containing data for one and only one
#'   area.
#' @param variables
#'   list created with function "alist" representing the definition of the
#'   variables to plot. 
#' @param colors
#'   vector of colors. It must have the same length as variables.
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
.plotProdStack <- function(x, variables, colors, lines, lineColors, 
                           main = NULL, unit = "MWh", legendId = "",
                           groupId = legendId,
                           width = NULL, height = NULL, dateRange = NULL, stepPlot = FALSE, drawPoints = FALSE) {
  
  timeStep <- attr(x, "timeStep")
  
  formulas <- append(variables, lines)
  variables <- names(variables)
  lines <- names(lines)
  
  dt <- data.table(timeId = x$timeId)
  for (n in names(formulas)) {
    dt[,c(n) := x[, eval(formulas[[n]]) / switch(unit, MWh = 1, GWh = 1e3, TWh = 1e6)]]
  }
  .plotStack(dt, timeStep, simOptions(x), colors, lines, lineColors, legendId,
             groupId,
             main = main, ylab = sprintf("Production (%s)", unit), 
             width = width, height = height, dateRange = dateRange, stepPlot = stepPlot, drawPoints = drawPoints)
}

#' @rdname tsLegend
#' @export
prodStackLegend <- function(stack = "eco2mix", 
                            legendItemsPerRow = 5, legendId = "") {
  
  stackOpts <- .aliasToStackOptions(stack)
  
  tsLegend(
    labels = c(names(stackOpts$variables), names(stackOpts$lines)), 
    colors = c(stackOpts$colors, stackOpts$lineColors),
    types = c(rep("area", length(stackOpts$variables)), rep("line", length(stackOpts$lines))),
    legendItemsPerRow = legendItemsPerRow,
    legendId = legendId
  )
}




