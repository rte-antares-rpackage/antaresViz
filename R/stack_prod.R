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
#'   districts.
#' @param y
#'   Optional object of class \code{antaresData}. If it is specified, then two
#'   charts are generated.
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
#'   list with the initial values of the given parameter for each chart.
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
#'  
#' @return 
#' \code{prodStackAliases} returns an interactive html graphic. If argument
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
#' @examples
#' \dontrun{
#' mydata <- readAntares(areas = "all", timeStep = "daily")
#' 
#' # Start a shiny gadget that permits to choose areas to display.
#' prodStack(mydata, unit = "GWh")
#' 
#' # Use in a non-interactive way
#' prodStack(mydata, unit = "GWh", areas = "fr", interactive = FALSE)
#' 
#' # Define a custom stack
#' setProdStackAlias(
#'   name = "Wind and solar",
#'   variables = alist(wind = WIND, solar = SOLAR),
#'   colors = c("green", "orange")
#' )
#' 
#' prodStack(mydata, unit = "GWh", stack = "Wind and solar")
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
#' prodStack(mydata, unit = "GWh", stack = "renewable")
#'                 
#' }
#' 
#' @export
prodStack <- function(x, y = NULL, 
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
                      width = NULL, height = NULL) {
  
  unit <- match.arg(unit)
  if (is.null(mcYear)) mcYear <- "average"
  
  params <- .getDataForComp(x, y, compare, compareOpts, function(x) {
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
    if (is.null(areas)) {
      areas <- unique(x$area)[1]
    }
    
    # should mcYear parameter be displayed on the UI?
    displayMcYear <- !attr(x, "synthesis") && length(unique(x$mcYear)) > 1
    
    dataDateRange <- as.Date(.timeIdToDate(range(x$timeId), timeStep, opts))
    if (length(dateRange) < 2) dateRange <- dataDateRange
    
    plotWithLegend <- function(id, areas, main = "", unit, stack, dateRange, mcYear, legend) {
      if (length(areas) == 0) return ("Please choose an area")
      stackOpts <- .aliasToStackOptions(stack)
      
      dt <- x[area %in% areas]
      
      if (mcYear == "average") dt <- synthesize(dt)
      else if ("mcYear" %in% names(dt)) {
        mcy <- mcYear
        dt <- dt[mcYear == mcy]
      }
      
      if (!is.null(dateRange)) {
        dt <- dt[as.Date(.timeIdToDate(dt$timeId, timeStep, opts = opts)) %between% dateRange]
      }
      
      p <- .plotProdStack(dt, 
                          stackOpts$variables, 
                          stackOpts$colors,
                          stackOpts$lines,
                          stackOpts$lineColors,
                          main = main,
                          unit = unit,
                          legendId = legendId + id - 1, groupId = groupId)
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
      areas = areas,
      displayMcYear = displayMcYear,
      dataDateRange = dataDateRange,
      dateRange = dateRange
      
    )
  })
  
  if (!interactive) {
    return(params$x[[1]]$plotWithLegend(1, areas, main, unit, stack, params$x[[1]]$dateRange, mcYear, legend))
  }
  
  manipulateWidget(
    params$x[[.id]]$plotWithLegend(.id, areas, main, unit, stack, dateRange, mcYear, legend),
    mcYear = mwSelect(c("average", unique(params$x[[.id]]$x$mcYear)), .display = params$x[[.id]]$displayMcYear),
    main = mwText(main, label = "title"),
    dateRange = mwDateRange(params$x[[1]]$dateRange, 
                            min = params$x[[.id]]$dataDateRange[1], 
                            max = params$x[[.id]]$dataDateRange[2]),
    stack = mwSelect(names(pkgEnv$prodStackAliases), stack),
    unit = mwSelect(c("MWh", "GWh", "TWh"), unit),
    areas = mwSelect(as.character(unique(params$x[[.id]]$x$area)), areas, multiple = TRUE),
    legend = mwCheckbox(legend),
    .compare = params$compare,
    .compareOpts = params$compareOpts
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
                                 width = NULL, height = NULL) {
  
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
             width = width, height = height)
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
