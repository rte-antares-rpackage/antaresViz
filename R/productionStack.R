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
#'   \code{\link{productionStackLegend}}
#' @param legendId
#'   Id of the legend linked to the graph. This argument is 
#'   usefull when one wants to create a shared legend with
#'   \code{\link{productionStackLegend}}
#' @param legendItemsPerRow
#'   Number of elements to put in each row of the legend.
#'   
#' @return 
#' An interactive html graphic. If argument \code{interactive} is \code{TRUE},
#' then a shiny gadget is started and the function returns an interactive html
#' graphic when the user clicks on button "Done".
#' 
#' @seealso \code{\link{productionStackLegend}}
#' 
#' @examples
#' \dontrun{
#' mydata <- readAntares(areas = "all", timeStep = "daily")
#' 
#' # Start a shiny gadget that permits to choose areas to display.
#' productionStack(mydata, unit = "GWh", height="100%")
#' 
#' # Use in a non-interactive way
#' productionStack(mydata, unit = "GWh", height="100%", areas = "fr", interactive = FALSE)
#' 
#' # Define a custom stack
#' productionStack(mydata, unit = "GWh", height="100%", 
#'                 variables = alist(wind = WIND, solar = SOLAR),
#'                 colors = c("green", "orange"))
#'                 
#' # In a custom stack it is possible to use computed values
#' productionStack(mydata, unit = "GWh", height="100%", 
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
productionStack <- function(x, variables = "eco2mix", colors = NULL, lines = NULL,
                            lineColors = NULL,
                            areas = NULL, 
                            dateRange = NULL,
                            main = "Production stack", unit = c("MWh", "GWh", "TWh"),
                            width = "100%", height = "500px",
                            interactive = base::interactive(), 
                            legend = TRUE, legendId = sample(1e9, 1),
                            legendItemsPerRow = 5) {
  
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
  
  plotWithLegend <- function(areas, main = "", unit, stack, dateRange) {
    if (length(areas) == 0) return ("Please choose an area")
    
    # If user has provided an alias, then stack contains this alias and
    # the corresponding options are retrieved. Else use the options
    # provided by the user.
    if (!is.null(.stack)) {
      stackOptions <- .aliasToStackOptions(stack)
      variables <- stackOptions$variables
      colors <- stackOptions$colors
      lines <- stackOptions$lines
      lineColors <- stackOptions$lineColors
    }
    
    dt <- x[area %in% areas]
    if (!is.null(dateRange)) {
      dt <- dt[as.Date(.timeIdToDate(dt$timeId, timeStep)) %between% dateRange]
    }
    
    p <- .plotProductionStack(dt, 
                              variables, 
                              colors,
                              lines,
                              lineColors,
                              main = main,
                              unit = unit,
                              legendId = legendId)
    if (legend) {
      combineWidgets(vflex = c(1, NA),
                     p,
                     productionStackLegend(variables, colors, lines, lineColors, legendItemsPerRow, legendId = legendId)
      )
    } else {
      combineWidgets(p)
    }
  }
  
  if (!interactive) {
    return(plotWithLegend(areas, main, unit, stack, dateRange))
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

#' Generate an interactive production stack
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
.plotProductionStack <- function(x, variables, colors, lines, lineColors, 
                                 main = NULL, unit = "MWh", legendId = "") {

  timeStep <- attr(x, "timeStep")
  
  # 1- Create a data.table containing the series defined by parameter "variables"
  dt <- data.table(timeId = x$timeId, area = x$area)
  dt$time <- .timeIdToDate(dt$timeId, timeStep, simOptions(x))
  dt[, timeId := NULL]
  dt[, c(rev(names(variables)), paste0("neg", names(variables)), "totalNeg", names(lines), paste0("opp", names(lines))) := 0]
  
  nvar <- length(variables)
  nlines <- length(lines)
  
  for (i in length(variables):1) {
      values <- x[, eval(variables[[i]]) ] / switch(unit, MWh = 1, GWh = 1e3, TWh = 1e6)
    set(dt, j = nvar + 3L - i, value = values)
  }
  
  if (nlines > 0) {
    for (i in 1:nlines) {
        value <- x[, eval(lines[[i]]) ] / switch(unit, MWh = 1, GWh = 1e3, TWh = 1e6)
      set(dt, j = 2L * nvar + 3L + i, value = value)
      set(dt, j = 2L * nvar + 3L + nlines + i, value = -value)
    }
  }
  
  # 2- Group by timeId
  dt[, area := NULL]
  dt <- dt[, lapply(.SD, sum), by = time]
  
  # 3- Separate positive and negative values and compute total negative values
  for (i in length(variables):1) {
    values <- dt[[names(variables)[i]]]
    posValues <- pmax(0, values)
    negValues <- pmin(0, values)
    
    set(dt, j = nvar + 2L - i, value = posValues)
    set(dt, j = nvar + 1L + i, value = - negValues)
    dt$totalNeg <- dt$totalNeg + negValues
  }
  
  # 5- Finally plot !!
  colors <- unname(c("#FFFFFF", rev(colors), colors))
  
  g <- dygraph(dt, main = main, width = "100%", height = "100%")  %>%
    dyOptions(
      stackedGraph = TRUE, 
      colors = rev(colors), 
      fillAlpha = 0.6,
      includeZero = TRUE, 
      gridLineColor = gray(0.8), 
      axisLineColor = gray(0.6), 
      axisLabelColor = gray(0.6), 
      labelsKMB = FALSE
    ) %>% 
    dyAxis("y", label = sprintf("Production (%s)", unit), pixelsPerLabel = 60, valueRange = c(min(dt$totalNeg) * 1.1, NA)) %>% 
    dyLegend(show = "never") %>% 
    dyCallbacks(
      highlightCallback = JS(sprintf(
        "function(e, timestamp, data) {
           var values = {}
           data.forEach(function(d) {
             var sign = d.name.match(/^neg/) ? -1 : 1;
             var varname = d.name.replace(/^neg/, '');
             if (values[varname]) values[varname] += d.yval * sign;
             else values[varname] = d.yval * sign;
           })
           for (k in values) {
             if (!values.hasOwnProperty(k)) continue; 
             var el = document.getElementById(k + '%s');
             if (el) el.innerHTML = '<b style=\"font-size:1.5em;\">' + Math.round(values[k]) + '</b> %s';
           }

         }",
        legendId, unit
      )),
      unhighlightCallback = JS(
        "function(e) {
           var els = document.getElementsByClassName('legvalue');
           for (var i = 0; i < els.length; ++i) {els[i].innerHTML = '';}
         }"
      )
    )
  
  if (length(lines) > 0) {
    for (i in 1:length(lines)) {
      g <- g %>% dySeries(name = paste0("opp", names(lines)[i]), color = lineColors[i], 
                          fillGraph = FALSE, strokeWidth = 0)
      g <- g %>% dySeries(name = names(lines)[i], color = lineColors[i], 
                          fillGraph = FALSE, strokeWidth = 3)
    }
  }
  
  g
}

#' Plot an interactive legend for a production stack plot
#' 
#' This function create an nice looking legend that displays values when the user
#' hovers a production stack created with \code{\link{productionStack}}. By 
#' default \code{\link{productionStack}} already outputs a legend. This function
#' is mostly usefull to share legend between two or more production stacks.
#' 
#' @inheritParams productionStack
#' 
#' @details 
#' This function can be used to create a legend shared by multiple production 
#' stacks in a Shiny application or an interactive document created with Rmarkdown.
#' For instance, let assume one wants to display four productions stacks in a 2x2
#' layout and have a unique legend below them in a Rmarkdown document. To do so,
#' one can use the following chunck code:
#' 
#' \preformatted{
#' ```{R, echo = FALSE}
#' library(shiny)
#' 
#' fillCol(height = "600px", flex = c(1, 1, NA),
#'   fillRow(
#'     productionStack(mydata, areas = "fr", 
#'                     main = "Production stack in France", unit = "GWh", 
#'                     legend = FALSE, legendId = 1, height = "100\%"),
#'     productionStack(mydata, areas = "de", 
#'                     main = "Production stack in Germany", unit = "GWh", 
#'                     legend = FALSE, legendId = 1, height = "100\%"),
#'   ),
#'   fillRow(
#'     productionStack(mydata, areas = "es", 
#'                     main = "Production stack in Spain", unit = "GWh", 
#'                     legend = FALSE, legendId = 1, height = "100\%"),
#'     productionStack(mydata, areas = "be", 
#'                     main = "Production stack in Belgium", unit = "GWh", 
#'                     legend = FALSE, legendId = 1, height = "100\%"),
#'   ),
#'   productionStackLegend(legendId = 1)
#' )
#' ```
#' }
#' 
#' 
#' 
#' @export
productionStackLegend <- function(variables = "eco2mix", colors = NULL, lines = NULL, 
                                  lineColors = NULL, 
                                  itemsByRow = 5, legendId = "") {
  if (is.character(variables)) { # variables is an alias
    
    stackOptions <- .aliasToStackOptions(variables)
    variables <- stackOptions$variables
    if (is.null(colors)) colors <- stackOptions$colors
    if (is.null(lines)) lines <- stackOptions$lines
    if (is.null(lineColors)) lineColors <- stackOptions$lineColors
    
  }
  
  legendItems <- mapply(.productionStackLegendItem, 
                        label = c(names(variables), names(lines)), 
                        color = c(colors, lineColors), 
                        legendId = legendId,
                        SIMPLIFY = FALSE, 
                        USE.NAMES = FALSE)
  
  nbRows <- ceiling(length(legendItems) / itemsByRow) 
  
  legendItems <- rev(legendItems)
  
  legendRows <- list()
  for (i in 1:nbRows) {
    j <- ((i - 1) * itemsByRow + 1):(i * itemsByRow)
    legendRows[[i]] <- do.call(fillRow, legendItems[j])
  } 
  
  tags$div(fillRow(do.call(fillCol, legendRows), height = i * 60), height = i * 60)
}

.productionStackLegendItem <- function(label, color, legendId) {
  txtColor <- sprintf("color:%s; text-align: right;", color)
  bgColor <- sprintf("background-color:%s", color)
  
  fillCol(flex = c(1.5,NA,1), style = "padding:4px;",
          tags$div("", style=txtColor, id = paste0(label, legendId), class =  "legvalue"),
          tags$div(style = paste(c(bgColor, "height:6px", "margin:2px 0"), collapse = ";")),
          tags$div(label, style = txtColor))
}
