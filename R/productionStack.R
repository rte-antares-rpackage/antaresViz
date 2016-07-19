#' @export
productionStack <- function(x, variables = "eco2mix", colors) {
  
  # If parameter "variables" is an alias, then use the variables and colors 
  # corresponding to that alias
  if (is.character(variables)) { # variables is an alias
    
    stackOptions <- .aliasToStackOptions(variables)
    variables <- stackOptions$variables
    if (missing(colors)) colors <- stackOptions$colors
    
  } else {
    
    if (missing(colors)) stop("Colors need to be specified when using custom variables.")
  
  }
  
  # Check that there are as much colors as variables
  if (length(colors) != length(variables)) {
    stop("Number of colors and number of variables should be equal.")
  }
  
  x <- .checkColumns(x, list(areas = c("timeId")))
  
  .plotProductionStack(x$areas, variables, colors)
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
      exports = - (BALANCE + `ROW BAL.`),
      pumpedStorage  = PSP,
      wind = WIND,
      solar = SOLAR,
      nuclear = NUCLEAR,
      hydraulic = `H. ROR` + `H. STOR`,
      gas = GAS,
      coal = COAL + LIGNITE,
      fuel = `MIX. FUEL`
    )
    
    colors <- rgb(
      red =   c(150,  17, 116, 242, 245,  39, 243, 172, 131),
      green = c(150,  71, 205, 116, 179, 114,  10, 140,  86),
      blue =  c(150, 116, 185,   6,   0, 178,  10,  53, 162),
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
  
  dt <- data.table(timeId = x$timeId)
  dt$time <- .timeIdToDate(dt$timeId, timeStep)
  dt[, timeId := NULL]
  dt[, c(rev(names(variables))) := 0]
  
  totalNeg <- rep(0, nrow(dt))
  
  for (i in length(variables):1) {
    values <- x[, eval(variables[[i]])]
    posValues <- pmax(0, values)
    negValues <- pmin(0, values)
    
    set(dt, j = length(variables) - i + 2L, value = posValues)
    set(dt, j = paste0("neg", names(variables)[i]), value = - negValues)
    totalNeg <- totalNeg + negValues
  }
  
  dt[, totalNeg := totalNeg]
  
  colors <- c("#FFFFFF", colors, colors)
  
  dygraph(dt)  %>%  
    dyOptions(stackedGraph = TRUE, colors = rev(colors), fillAlpha = 0.8)
}
