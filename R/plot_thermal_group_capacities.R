#' Plot for Thermal Group Capacities
#' 
#' @param data data.table of Thermal Group capacities
#' @param area areas to select, default all
#' @param main title
#' 
#' @examples
#' \dontrun{
#' opts <- setSimulationPath(getwd())
#' plotThermalGroupCapacities( thermalGroupCapacities(opts))
#' }
#' 
#' @export
plotThermalGroupCapacities <- function(data, area = 'all', main = "Thermal group capacities"){
  if(area != 'all'){
    areaTp <- area
    data <- data[area %in% areaTp]
  }
  data <- data.table::dcast(data, area~group, value.var = "thermalGroupCapacity")
  data <- data[,lapply(.SD, function(X){X[is.na(X)] <- 0;X}), .SDcols = 1:ncol(data)]
  toPLot <- names(data)[names(data)!="area"]
  p <- plot_ly(data,  type = 'bar') %>%
    layout(title  = main, yaxis = list(title = 'MWh'), barmode = 'stack')
  for(i in toPLot){
    p <- p %>%  add_trace(x = ~area,y = as.formula(paste0("~`", i, "`")), name = i) 
  }
  suppressWarnings(print(p))
}

