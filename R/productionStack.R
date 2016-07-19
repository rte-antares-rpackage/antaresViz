#' @export
productionStack <- function(x, variables = "eco2mix", colors) {
  if (is.character(variables)) {
    
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
      
      if (missing(colors)) {
        colors <- rgb(
          red =   c(150,  17, 116, 242, 245,  39, 243, 172, 131),
          green = c(150,  71, 205, 116, 179, 114,  10, 140,  86),
          blue =  c(150, 116, 185,   6,   0, 178,  10,  53, 162),
          maxColorValue = 255
        )
      }
      
    } else {
      stop("Unknown alias '", variables, "'.")
    }
  } else {
    if (missing(colors)) stop("Colors need to be specified when using custom variables.")
  }
  
  if (length(colors) != length(variables)) {
    stop("Number of colors and number of variables should be equal.")
  }
  
  x <- .checkColumns(x, list(areas = c("timeId")))
  
  timeStep <- attr(x, "timeStep")
  
  dt <- data.table(timeId = x$areas$timeId)
  dt$time <- .timeIdToDate(dt$timeId, timeStep)
  dt[, timeId := NULL]
  dt[, c(rev(names(variables))) := 0]
  
  totalNeg <- rep(0, nrow(dt))
  
  for (i in length(variables):1) {
    values <- x$areas[, eval(variables[[i]])]
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
