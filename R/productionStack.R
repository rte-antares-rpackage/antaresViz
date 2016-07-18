#' @export
productionStack <- function(x) {
  x <- .checkColumns(x, list(areas = c(pkgEnv$production, "BALANCE", "ROW BAL.")))
  
  timeStep <- attr(x, "timeStep")
  
  dt <- x$areas[, c("timeId", pkgEnv$production, "BALANCE", "ROW BAL."), with = FALSE]
  dt$time <- .timeIdToDate(dt$timeId, timeStep)
  
  dt <- dt[, .(
    time, 
    fuel = `MIX. FUEL`,
    coal = COAL,
    gas = GAS,
    hydraulic = `H. ROR` + `H. STOR`,
    nuclear = NUCLEAR,
    solar = SOLAR,
    wind = WIND,
    pumpedStorage  = PSP,
    negExports = BALANCE + `ROW BAL.`,
    exports = - (BALANCE + `ROW BAL.`)
  )]
  
  colors <- rgb(
    red =   c(150, 150,  17, 116, 242, 245,  39, 243, 172, 131),
    green = c(150, 150,  71, 205, 116, 179, 114,  10, 140,  86),
    blue =  c(150, 150, 116, 185,   6,   0, 178,  10,  53, 162),
    maxColorValue = 255
  )
  
  dygraph(dt)  %>%  
    dyOptions(stackedGraph = TRUE, colors = rev(colors), fillAlpha = 0.8)
}