library(antaresViz)
library(antaresHdf5)
library(rhdf5)
library(manipulateWidget)
library(data.table)
library(rAmCharts)
library(dygraphs)

devtools::load_all(".")
path <- c("D:/Users/titorobe/Desktop/Antares/antaresViz/sim1.h5")
plotH5ts(path, main = "rr")




plotH5ts <- function(path,
                     confInt = 0.5,
                     minValue = NULL,
                     maxValue = NULL,
                     colors = NULL,
                     legend = TRUE,
                     legendItemsPerRow = 5,
                     width = NULL,
                     height = NULL,
                     main = NULL,
                     ylab  = NULL,
                     colorScaleOpts = colorScaleOptions(20),
                     compare = NULL, ...){
  
  opts <- setSimulationPathH5(path[1])
  fid <- H5Fopen(opts$h5path)
 
  if(length(path)>1) compare <- list()
  timeSteps <- .getTimStep(fid)
  timeStep <- timeSteps[1]
  typeChoices <- c("ts", "barplot", "monotone", "density", "cdf", "heatmap")
  tables <- .getTableInH5(fid, timeStep)
  variables <- .getVariablesH5(fid, timeStep,  tables)
  MCyears <- opts$mcYears
  elems <- .getElements(opts, tables, fid, timeStep)
  
  # Generate a group number for dygraph objects
  if (!("dateRange" %in% compare)) {
    group <- sample(1e9, 1)
  } else {
    group <- NULL
  }
  manipulateWidget(.doPlot(.id = .id,
                           path = path,
                           table = table,
                           mcYear = mcYear,
                           variable = variable,
                           elements = elements,
                           timeStep = timeStep,
                           data = data,
                           dateRange = dateRange,
                           type = type,
                           minValue = minValue,
                           maxValue = maxValue,
                           colors = colors,
                           main = main,
                           ylab = ylab,
                           legend = legend,
                           legendItemsPerRow = legendItemsPerRow,
                           width = width,
                           height = height,
                           opts = opts,
                           colorScaleOpts = colorScaleOpts,
                           group = group),
                   table = mwSelect(tables, value = tables[1], label = "tables"),
                   timeStep = mwSelect(choices = timeSteps, value = timeSteps[1], label ="timeStep"),
                   mcYear = mwSelect(c("MC-All", MCyears), value = "MC-All", label = "MCyear"),
                   dateRange = mwDateRange(value = c(as.Date("1900-01-01"), as.Date("2200-01-01")), min = dateRan[1], max = dateRan[2]),
                   variable = mwSelect(variables[[table]], variables[[tables[1]]][1], label = "variable", multiple = TRUE),
                   type = mwSelect(typeChoices,value = typeChoices[1], label = "type"),
                   elements = mwSelect(c( elems[[table]]), value = elems[[tables[1]]][1],label = "elements", multiple = TRUE),
                   minValue = mwNumeric(minValue, label = "min value", .display = type %in% c("density", "cdf")),
                   maxValue = mwNumeric(maxValue, label = "max value", .display = type %in% c("density", "cdf")),
                   dateRan = mwSharedValue(expr = .getDateRange(opts, timeStep) ),
                   .compare = compare,
                   ...)
  
  
}



