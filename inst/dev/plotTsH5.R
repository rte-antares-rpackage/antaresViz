library(antaresViz)
library(antaresHdf5)
library(rhdf5)
library(manipulateWidget)
library(data.table)
library(rAmCharts)

devtools::load_all(".")
path <- "D:/Users/titorobe/Desktop/Antares/antaresHdf5/bigStd.h5"

opts <- setSimulationPathH5(path)
h5Options <- getOptionsH5(path)
fid <- H5Fopen(h5Options$h5path)
timeStep = "hourly"
tables <- .getTableInH5(fid, timeStep)
variables <- .getVariablesH5(fid,timeStep,  tables)
MCyears <- h5Options$mcYears
elems <- .getElements(opts, tables, fid, timeStep)
dateR <- .getDateRange(fid, timeStep)
typeChoices <- c("ts", "barplot", "monotone", "density", "cdf", "heatmap")



getData <- function(path, table, mcYear, variable, elements, dateRange){
  if(mcYear == "MC-All")
  {
    mcYear <- NULL
  }
  areas <- links <- clusters <- districts <- NULL
  assign(table, as.character(elements))
  data <- h5ReadAntares(path, areas = areas, links = links, clusters = clusters, districts = districts, mcYears = mcYear, select = variable)
  colsId <- antaresRead::getIdCols(data)
  
  if("cluster" %in% colsId){
    idV <- c("area", "cluster")
  } else if ("area"%in%colsId){
    idV <- "area"
  } else if ("link"%in%colsId){
    idV <- "link"
  } else if ("district"%in%colsId){
    idV <- "district"
  }
  valueCol <- setdiff(names(data), colsId)
  data <- data[,.SD, .SDcols = c("timeId", "time", valueCol, idV)]
  if(length(idV) > 1){
    data[, newKey := paste0(lapply(.SD, as.character), collapse = " < "), .SDcols = idV,by=1:nrow(data)]
    data[,c(idV) := NULL]
    idV <- "newKey"
  }
  odc <- c("timeId", "time", valueCol, idV)
  setcolorder(data, odc)
  setnames(data, names(data), c("timeId", "time", "value", "element"))
  data[time>dateRange[1] & time<dateRange[2]]
  
}

doGraph <- function(.id, path, table, mcYear, variable, elements, timeStep, data, dateRange, type){
  data <- getData(path, table, mcYear, variable, elements, dateRange)
 
  
  f <- .getGraphFunction(type)
  f(
    data, 
    timeStep = timeStep, 
    variable = variable, 
    maxValue = 0
  )
  
  #.plotTS(data, timeStep, variable, maxValue = 0)
}

library(dygraphs)
manipulateWidget(doGraph(.id, path, table, mcYear, variable, elements, timeStep, data, dateRange, type),
                 table = mwSelect(tables, "tables"),
                 mcYear = mwSelect(c("MC-All", MCyears), "MCyear"),
                 dateRange = mwDateRange(value = dateR, min = dateR[1], max = dateR[2]) ,
                 variable = mwSelect(variables[[table]], "variable"),
                 type = mwSelect(typeChoices, "type"),
                 elements = mwSelect(c("all", elems[[table]]), "elements"),
                 .compare = "elements")




