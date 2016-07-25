#' Visualize the production stack of an area
#'
#' @export
productionStack <- function(x, variables = "eco2mix", colors = NULL, lines = NULL,
                            lineColors = NULL,
                            areas = NULL, 
                            main = "Production stack", unit = c("MWh", "GWh", "TWh"),
                            width = "100%", height = "500px",
                            interactive = base::interactive(), 
                            legend = TRUE, legendId = sample(1e9, 1),
                            legendItemsPerRow = 5) {
  
  unit <- match.arg(unit)
  
  # If parameter "variables" is an alias, then use the variables and colors 
  # corresponding to that alias
  if (is.character(variables)) { # variables is an alias
    
    stackOptions <- .aliasToStackOptions(variables)
    variables <- stackOptions$variables
    if (is.null(colors)) colors <- stackOptions$colors
    if (is.null(lines)) lines <- stackOptions$lines
    if (is.null(lineColors)) lineColors <- stackOptions$lineColors
    
  } else {
    
    if (is.null(colors)) stop("Colors need to be specified when using custom variables.")
    if (!is.null(lines) && is.null(lineColors)) {
      stop("lineColors need to be specified when using custom lineCurves.")
    }
  
  }
  
  # Check that there are as much colors as variables
  if (length(colors) != length(variables)) {
    stop("Number of colors and number of variables should be equal.")
  }
  if (length(lineColors) != length(lines)) {
    stop("Number of line colors and number of lines should be equal.")
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
  
  plotWithLegend <- function(areas, main = "") {
    res <- miniPage(
      style=sprintf("width:%s;height:%s;", width, height),
      fillCol(flex = c(1,NA),
        .plotProductionStack(x[area %in% areas], 
                             variables, 
                             colors,
                             lines,
                             lineColors,
                             main = main,
                             unit = unit,
                             legendId = legendId),
        if (legend) productionStackLegend(variables, colors, lines, lineColors, legendItemsPerRow, legendId = legendId) else ""
      )
    )
    class(res) <- append("productionStack", class(res))
    res
  }
  
  if (!interactive) {
    return(plotWithLegend(areas, main))
  }
  
  ui <- miniPage(
    gadgetTitleBar(textOutput("title", inline = TRUE)),
    miniContentPanel(
      fillRow(flex = c(1,3),
              
        fillCol(flex = c(NA, 1),
          textInput("main", "Title", value = main),
          selectInput(
            "area", "Areas", 
            choices = as.list(levels(x$area)), 
            multiple = TRUE, 
            selected = areas
          )
        ),
        
        fillCol(flex = c(1, NA),
          dygraphOutput("chart", height = "100%"),
          productionStackLegend(variables, colors, lines, lineColors, legendItemsPerRow, legendId = legendId)
        )
        
      )
    )
  )
  
  server <- function(input, output, session) {
    
    output$title <- renderText(input$main)
    
    output$chart <- renderDygraph({
      if(length(input$area) > 0) {
        .plotProductionStack(x[area %in% input$area], variables, colors, lines, lineColors, unit = unit, legendId = legendId)
      }
    })
    
    observeEvent(input$done, {
      returnValue <- plotWithLegend(input$area, input$main)
      
      stopApp(returnValue)
    })
  }
  
  runGadget(ui, server)
}

#' Returns the variables and colors corresponding to an alias
#' 
#' @param variables
#'   character string représenting an alias
#'   
#' @return 
#' This function returns a list with two components:
#' \item{variables}{Definition of the variables of the stack}
#' \item{colors}{colors for the variables}
#' 
#' @details 
#' For now, only the alias "eco2mix" is recognised. If an unknown alias is used,
#' the function returns an error.
#' 
#' @noRd
.aliasToStackOptions <- function(variables) {
  if (variables == "eco2mix") {
    
    variables <- alist(
      pumpedStorage  = PSP,
      exports = - (BALANCE + `ROW BAL.`),
      wind = WIND,
      solar = SOLAR,
      nuclear = NUCLEAR,
      hydraulic = `H. ROR` + `H. STOR`,
      gas = GAS,
      coal = COAL + LIGNITE,
      fuel = `MIX. FUEL` + OIL + `MISC. DTG` + `MISC. NDG`
    )
    
    colors <- rgb(
      red =   c( 17, 150, 116, 242, 245,  39, 243, 172, 131),
      green = c( 71, 150, 205, 116, 179, 114,  10, 140,  86),
      blue =  c(185, 150, 185,   6,   0, 178,  10,  53, 162),
      maxColorValue = 255
    )
    
    lines <- alist(load = LOAD)
    
    lineColors <- c("#000000")
    
  } else {
    stop("Unknown alias '", variables, "'.")
  }
  
  list(variables = variables, colors = colors, lines = lines, lineColors = lineColors) 
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
#' @noRd
.plotProductionStack <- function(x, variables, colors, lines, lineColors, 
                                 main = NULL, unit = "MWh", legendId = "") {

  timeStep <- attr(x, "timeStep")
  
  # 1- Create a data.table containing the series defined by parameter "variables"
  dt <- data.table(timeId = x$timeId, area = x$area)
  dt$time <- .timeIdToDate(dt$timeId, timeStep)
  dt[, timeId := NULL]
  dt[, c(rev(names(variables)), paste0("neg", names(variables)), "totalNeg", names(lines), paste0("opp", names(lines))) := 0]
  
  nvar <- length(variables)
  nlines <- length(lines)
  
  for (i in length(variables):1) {
    values <- x[, eval(variables[[i]])] / switch(unit, MWh = 1, GWh = 1e3, TWh = 1e6)
    set(dt, j = nvar + 3L - i, value = values)
  }
  
  if (nlines > 0) {
    for (i in nlines) {
      value <- x[, eval(lines[[i]])] / switch(unit, MWh = 1, GWh = 1e3, TWh = 1e6)
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
  colors <- c("#FFFFFF", rev(colors), colors)
  
  g <- dygraph(dt, main = main, width = "100%", height = "100%")  %>%
    dyOptions(
      stackedGraph = TRUE, 
      colors = rev(colors), 
      fillAlpha = 0.7,
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
    for (i in length(lines)) {
      g <- g %>% dySeries(name = paste0("opp", names(lines)[i]), color = lineColors[i], 
                          fillGraph = FALSE, strokeWidth = 0)
      g <- g %>% dySeries(name = names(lines)[i], color = lineColors[i], 
                          fillGraph = FALSE, strokeWidth = 2)
    }
  }
  
  g
}

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
  
  
  fillRow(do.call(fillCol, legendRows), height = i * 60)
}

.productionStackLegendItem <- function(label, color, legendId) {
  txtColor <- sprintf("color:%s; text-align: right;", color)
  bgColor <- sprintf("background-color:%s", color)
  
  fillCol(flex = c(1.5,NA,1), style = "padding:4px;",
          tags$div("", style=txtColor, id = paste0(label, legendId), class =  "legvalue"),
          tags$div(style = paste(c(bgColor, "height:6px", "margin:2px 0"), collapse = ";")),
          tags$div(label, style = txtColor))
}

#' Visualize the object generated by function productionStack
#' @export
print.productionStack <- function(x, ...) {
  htmltools:::html_print(x)
}
