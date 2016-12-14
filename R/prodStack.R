# Copyright © 2016 RTE Réseau de transport d’électricité

#' Visualize the production stack of an area
#' 
#' This function draws the production stack for a set of areas or districts. User
#' can choose predefined stacks or define its own stacks. 
#' 
#' @param x
#'   An object of class \code{antaresData} created with function 
#'   \code{\link[antaresRead]{readAntares}} containing data for areas and or
#'   districts.
#' @param variables
#'   Either a character string containing an alias or a named list of expressions 
#'   created with \code{\link[base]{alist}}. The name of each element is the name
#'   of the variable to draw in the stacked graph. The element itself is an
#'   expression explaining how to compute the variable (see examples).
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
#' @param areas
#'   Vector of area or district names. The data of these areas or districts is
#'   aggregated by the function to construct the production stack.
#' @param dateRange
#'   A vector of two dates. Only data points between these two dates are 
#'   displayed. If NULL, then all data is displayed.
#' @param main
#'   Title of the graph.
#' @param unit
#'   Unit used in the graph. Possible values are "MWh", "GWh" or "TWh".
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
#' @param legendId
#'   Id of the legend linked to the graph. This argument is 
#'   usefull when one wants to create a shared legend with
#'   \code{\link{prodStackLegend}}
#' @param legendItemsPerRow
#'   Number of elements to put in each row of the legend.
#'   
#' @return 
#' An interactive html graphic. If argument \code{interactive} is \code{TRUE},
#' then a shiny gadget is started and the function returns an interactive html
#' graphic when the user clicks on button "Done".
#' 
#' @seealso \code{\link{prodStackLegend}}
#' 
#' @examples
#' \dontrun{
#' mydata <- readAntares(areas = "all", timeStep = "daily")
#' 
#' # Start a shiny gadget that permits to choose areas to display.
#' prodStack(mydata, unit = "GWh", height="100%")
#' 
#' # Use in a non-interactive way
#' prodStack(mydata, unit = "GWh", height="100%", areas = "fr", interactive = FALSE)
#' 
#' # Define a custom stack
#' prodStack(mydata, unit = "GWh", height="100%", 
#'                 variables = alist(wind = WIND, solar = SOLAR),
#'                 colors = c("green", "orange"))
#'                 
#' # In a custom stack it is possible to use computed values
#' prodStack(mydata, unit = "GWh", height="100%", 
#'                 variables = alist(
#'                   renewable = WIND + SOLAR + `H. ROR` + `H. STOR` + `MISC. NDG`, 
#'                   thermal = NUCLEAR + LIGNITE + COAL + GAS + OIL + `MIX. FUEL` + `MISC. DTG`
#'                 ),
#'                 colors = c("green", gray(0.3)),
#'                 lines = alist(goalRenewable = LOAD * 0.23),
#'                 lineColors = "#42EB09")
#' }
#' 
#' @export
prodStack <- function(x, variables = "eco2mix", colors = NULL, lines = NULL,
                      lineColors = NULL,
                      areas = NULL, 
                      dateRange = NULL,
                      main = "Production stack", unit = c("MWh", "GWh", "TWh"),
                      interactive = base::interactive(), 
                      legend = TRUE, legendId = sample(1e9, 1),
                      legendItemsPerRow = 5,
                      width = NULL, height = NULL) {
  
  unit <- match.arg(unit)
  
  # If parameter "variables" is an alias, then use the variables and colors 
  # corresponding to that alias
  if (is.character(variables)) { # variables is an alias
    .stack <- variables
  } else {
    .stack <- NULL
    if (is.null(colors)) stop("Colors need to be specified when using custom variables.")
    if (!is.null(lines) && is.null(lineColors)) {
      stop("lineColors need to be specified when using custom lineCurves.")
    }
    # Check that there are as much colors as variables
    if (length(colors) != length(variables)) {
      stop("Number of colors and number of variables should be equal.")
    }
    if (length(lineColors) != length(lines)) {
      stop("Number of line colors and number of lines should be equal.")
    }
  }
  
  # Check that input contains area or district data
  if (!is(x, "antaresData")) stop("'x' should be an object of class 'antaresData created with readAntares()'")
  if (is(x, "antaresDataTable")) {
    if (!attr(x, "type") %in% c("areas", "districts")) stop("'x' should contain area or district data")
  }
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
  
  dataDateRange <- as.Date(.timeIdToDate(range(x$timeId), timeStep, opts))
  if (length(dateRange) < 2) dateRange <- dataDateRange
  
  plotWithLegend <- function(areas, main = "", unit, .stack, dateRange) {
    if (length(areas) == 0) return ("Please choose an area")
    
    # If user has provided an alias, then stack contains this alias and
    # the corresponding options are retrieved. Else use the options
    # provided by the user.
    if (!is.null(.stack)) {
      stackOptions <- .aliasToStackOptions(.stack)
      variables <- stackOptions$variables
      colors <- stackOptions$colors
      lines <- stackOptions$lines
      lineColors <- stackOptions$lineColors
    }
    
    dt <- x[area %in% areas]
    if (!is.null(dateRange)) {
      dt <- dt[as.Date(.timeIdToDate(dt$timeId, timeStep)) %between% dateRange]
    }
    
    p <- .plotProdStack(dt, 
                              variables, 
                              colors,
                              lines,
                              lineColors,
                              main = main,
                              unit = unit,
                              legendId = legendId)
    if (legend) {
      l <- prodStackLegend(variables, colors, lines, lineColors, 
                           legendItemsPerRow, legendId = legendId)
    } else {
      l <- NULL
    }
    
    combineWidgets(p, footer = l, width = width, height = height)
  }
  
  if (!interactive) {
    return(plotWithLegend(areas, main, unit, .stack, dateRange))
  }
  
  manipulateWidget(
    plotWithLegend(areas, main, unit, stack, dateRange),
    main = mwText(main, label = "title"),
    dateRange = mwDateRange(dateRange, min = dataDateRange[1], max = dataDateRange[2]),
    stack = mwSelect(names(pkgEnv$prodStackAliases), .stack),
    unit = mwSelect(c("MWh", "GWh", "TWh"), unit),
    areas = mwSelect(as.character(unique(x$area)), areas, multiple = TRUE),
    .main = "Production stack",
    .display = list(stack = !is.null(.stack))
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
             main = main, ylab = sprintf("Production (%s)", unit), 
             width = width, height = height)
}

#' @rdname tsLegend
#' @export
prodStackLegend <- function(variables = "eco2mix", colors = NULL, lines = NULL, 
                                  lineColors = NULL, 
                                  legendItemsPerRow = 5, legendId = "") {
  if (is.character(variables)) { # variables is an alias
    
    stackOptions <- .aliasToStackOptions(variables)
    variables <- stackOptions$variables
    if (is.null(colors)) colors <- stackOptions$colors
    if (is.null(lines)) lines <- stackOptions$lines
    if (is.null(lineColors)) lineColors <- stackOptions$lineColors
    
  }
  
  tsLegend(
    labels = c(names(variables), names(lines)), 
    colors = c(colors, lineColors),
    type = c(rep("area", length(variables)), rep("line", length(lines))),
    legendItemsPerRow = legendItemsPerRow,
    legendId = legendId
  )
}
