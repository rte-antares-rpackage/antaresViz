#' @export
productionStack <- function(x, variables = "eco2mix", colors = NULL, areas = NULL, 
                            main = "Production stack", unit = c("MWh", "GWh", "TWh")) {
  
  unit <- match.arg(unit)
  
  # If parameter "variables" is an alias, then use the variables and colors 
  # corresponding to that alias
  if (is.character(variables)) { # variables is an alias
    
    stackOptions <- .aliasToStackOptions(variables)
    variables <- stackOptions$variables
    if (is.null(colors)) colors <- stackOptions$colors
    
  } else {
    
    if (is.null(colors)) stop("Colors need to be specified when using custom variables.")
  
  }
  
  # Check that there are as much colors as variables
  if (length(colors) != length(variables)) {
    stop("Number of colors and number of variables should be equal.")
  }
  
  x <- .checkColumns(x, list(areas = c("timeId")))
  
  ui <- miniPage(
    gadgetTitleBar(textOutput("title", inline = TRUE)),
    miniContentPanel(
      fillRow(flex = c(1,3),
              
        fillCol(flex = c(NA, 1),
          textInput("main", "Title", value = main),
          selectInput(
            "area", "Areas", 
            choices = as.list(levels(x$areas$area)), 
            multiple = TRUE, 
            selected = areas
          )
        ),
        
        fillCol(flex = c(1, NA),
          dygraphOutput("chart", height = "100%"),
          .productionStackLegend(variables, colors)
        )
        
      )
    )
  )
  
  server <- function(input, output, session) {
    
    output$title <- renderText(input$main)
    
    output$chart <- renderDygraph({
      if(length(input$area) > 0) {
        .plotProductionStack(x$areas[area %in% input$area], variables, colors, unit = unit)
      }
    })
    
    observeEvent(input$done, {
      returnValue <- miniPage(fillCol(flex = c(1,NA),
        .plotProductionStack(x$areas[area %in% input$area], 
                             variables, 
                             colors,
                             main = input$main,
                             unit = unit),
        .productionStackLegend(variables, colors)
      ))
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
      fuel = `MIX. FUEL`
    )
    
    colors <- rgb(
      red =   c( 17, 150, 116, 242, 245,  39, 243, 172, 131),
      green = c( 71, 150, 205, 116, 179, 114,  10, 140,  86),
      blue =  c(185, 150, 185,   6,   0, 178,  10,  53, 162),
      maxColorValue = 255
    )
    
  } else {
    stop("Unknown alias '", variables, "'.")
  }
  
  list(variables = variables, colors = colors ) 
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
.plotProductionStack <- function(x, variables, colors, main = NULL, unit = "MWh") {

  timeStep <- attr(x, "timeStep")
  
  # 1- Create a data.table containing the series defined by parameter "variables"
  dt <- data.table(timeId = x$timeId, area = x$area)
  dt$time <- .timeIdToDate(dt$timeId, timeStep)
  dt[, timeId := NULL]
  dt[, c(rev(names(variables)), paste0("neg", names(variables))) := 0]
  
  nvar <- length(variables)
  
  for (i in length(variables):1) {
    values <- x[, eval(variables[[i]])] / switch(unit, MWh = 1, GWh = 1e3, TWh = 1e6)
    set(dt, j = nvar + 3L - i, value = values)
  }
  
  # 2- Group by timeId
  dt[, area := NULL]
  dt <- dt[, lapply(.SD, sum), by = time]
  
  # 3- Separate positive and negative values and compute total negative values
  totalNeg <- rep(0, nrow(dt))
  
  for (i in length(variables):1) {
    values <- dt[[names(variables)[i]]]
    posValues <- pmax(0, values)
    negValues <- pmin(0, values)
    
    set(dt, j = nvar + 2L - i, value = posValues)
    set(dt, j = nvar + 1L + i, value = - negValues)
    totalNeg <- totalNeg + negValues
  }
  
  dt[, totalNeg := totalNeg]
  
  # 4- Finally plot !!
  colors <- c("#FFFFFF", rev(colors), colors)
  
  dygraph(dt, main = main, width = "100%")  %>%
    dyOptions(
      stackedGraph = TRUE, 
      colors = rev(colors), 
      fillAlpha = 0.8,
      includeZero = TRUE, 
      gridLineColor = gray(0.8), 
      axisLineColor = gray(0.6), 
      axisLabelColor = gray(0.6), 
      labelsKMB = FALSE
    ) %>% 
    dyAxis("y", label = sprintf("Production (%s)", unit), pixelsPerLabel = 60) %>% 
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
             var el = document.getElementById(k);
             if (el) el.innerHTML = '<b style=\"font-size:1.5em;\">' + Math.round(values[k]) + '</b> %s';
           }

         }",
        unit
      )),
      unhighlightCallback = JS(
        "function(e) {
           var els = document.getElementsByClassName('legvalue');
           for (var i = 0; i < els.length; ++i) {els[i].innerHTML = '';}
         }"
      )
    )
}

.productionStackLegend <- function(variables, colors, itemsByRow = 5) {
  legendItems <- mapply(.productionStackLegendItem, 
                        label = names(variables), 
                        color = colors, 
                        SIMPLIFY = FALSE, 
                        USE.NAMES = FALSE)
  
  nbRows <- ceiling(length(legendItems) / itemsByRow) 
  
  legendItems <- rev(legendItems)
  
  legendRows <- list()
  for (i in 1:nbRows) {
    j <- ((i - 1) * 5 + 1):(i * 5)
    legendRows[[i]] <- do.call(fillRow, legendItems[j])
  } 
  
  
  fillRow(do.call(fillCol, legendRows), height = i * 60)
}

.productionStackLegendItem <- function(label, color) {
  txtColor <- sprintf("color:%s; text-align: right;", color)
  bgColor <- sprintf("background-color:%s", color)
  
  fillCol(flex = c(1.5,NA,1), style = "padding:4px;",
          tags$div("", style=txtColor, id = label, class =  "legvalue"),
          tags$div(style = paste(c(bgColor, "height:6px", "margin:2px 0"), collapse = ";")),
          tags$div(label, style = txtColor))
}
