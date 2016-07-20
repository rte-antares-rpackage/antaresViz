#' @export
productionStack <- function(x, variables = "eco2mix", colors = NULL, areas = NULL) {
  
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
    gadgetTitleBar("Production stack"),
    miniContentPanel(
      fillCol(flex = c(1,4,1),
        selectInput(
          "area", "Area", 
          choices = as.list(levels(x$areas$area)), 
          multiple = TRUE, 
          selected = areas
        ),
        dygraphOutput("chart", height = "100%"),
        tags$div("coucou")
      )
    )
  )
  
  server <- function(input, output, session) {
    output$chart <- renderDygraph({
      if(length(input$area) > 0) {
        .plotProductionStack(x$areas[area %in% input$area], variables, colors)
      }
    })
    
    observeEvent(input$done, {
      returnValue <- .plotProductionStack(x$areas[area %in% input$area], variables, colors)
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
#' @noRd
.plotProductionStack <- function(x, variables, colors) {
  timeStep <- attr(x, "timeStep")
  
  dt <- data.table(timeId = x$timeId, area = x$area)
  dt$time <- .timeIdToDate(dt$timeId, timeStep)
  dt[, timeId := NULL]
  dt[, c(rev(names(variables)), paste0("neg", names(variables))) := 0]
  
  
  nvar <- length(variables)
  
  for (i in length(variables):1) {
    values <- x[, eval(variables[[i]])]
    set(dt, j = nvar + 3L - i, value = values)
  }
  
  # Group by timeId
  dt[, area := NULL]
  dt <- dt[, lapply(.SD, sum), by = time]
  
  # Separate positive and negative values
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
  
  
  colors <- c("#FFFFFF", rev(colors), colors)
  
  dygraph(dt)  %>%
    dyOptions(
      stackedGraph = TRUE, 
      colors = rev(colors), 
      fillAlpha = 0.8,
      includeZero = TRUE, 
      gridLineColor = gray(0.8), 
      axisLineColor = gray(0.6), 
      axisLabelColor = gray(0.6), 
      labelsKMB = TRUE
    ) %>% 
    dyLegend(show = "never")
}
