
.getData <- function(path, table, mcYear, variable, elements, dateRange, timeStep){
  opts <- antaresRead::.getOptionsH5(path)
  if(mcYear == "MC-All")
  {
    mcYear <- NULL
  }
  areas <- links <- clusters <- districts <- NULL
  assign(table, as.character(elements))
  data <- antaresRead::.h5ReadAntares(path = path,
                        areas = areas,
                        links = links,
                        clusters = clusters,
                        districts = districts,
                        mcYears = mcYear, 
                        select = variable,
                        timeStep = timeStep,
                        perf = FALSE)
  if(nrow(data) == 0){return(data)}
  
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
  valueCol <- intersect(valueCol, variable)
  
  data <- data[,.SD, .SDcols = c("timeId", "time", valueCol, idV)]
  if(length(idV) > 1){
    data[, "newKey" := paste0(lapply(.SD, as.character), collapse = " < "), .SDcols = idV,by=1:nrow(data)]
    data[,c(idV) := NULL]
    idV <- "newKey"
  }else{
    setnames(data, idV, "newKey")
  }
  data$newKey <- as.character(data$newKey)

  if(ncol(data) > 4){
    data <- melt(data, c("newKey", "timeId", "time"))
    data[, "newKey" := paste0(data$newKey, " - ",  as.character(variable)),by=1:nrow(data)]
    data[, variable := NULL]
    valueCol <- "value"
  }
  
  
  odc <- c("timeId", "time", valueCol, "newKey")
  setcolorder(data, odc)
  setnames(data, names(data), c("timeId", "time", "value", "element"))
  
  data[,time := .timeIdToDate(timeId, timeStep, opts)]
  data[time >= dateRange[1] & time <= dateRange[2]]
  
}

.doPlot <- function(.id,
                    path,
                    table,
                    mcYear,
                    variable,
                    elements,
                    timeStep,
                    data,
                    dateRange,
                    type,
                    minValue,
                    maxValue,
                    colors,
                    main, 
                    ylab,
                    legend,
                    legendItemsPerRow,
                    width,
                    height,
                    opts,
                    colorScaleOpts,
                    group){
  
  
  data <- .getData(ifelse(length(path) == 1 , path , path[.id]), table, mcYear, variable, elements, dateRange, timeStep)
  if(nrow(data) == 0){return(NULL)}
  f <- .getGraphFunction(type)

  f(
    data, 
    timeStep = timeStep, 
    variable = variable, 
    confInt = 0, 
    minValue = minValue,
    maxValue = maxValue,
    colors = colors,
    main = if(length(main) <= 1) main else main[.id],
    ylab = if(length(ylab) <= 1) ylab else ylab[.id],
    legend = legend,
    legendItemsPerRow = legendItemsPerRow,
    width = width, 
    height = height,
    opts = opts,
    colorScaleOpts = colorScaleOpts,
    group = group
  )
  
}


.getstructure <- function(fid, strgp){
  gid <- rhdf5::H5Gopen(fid,  strgp)
  data <- rhdf5::h5dump(gid)
  rhdf5::H5Gclose(gid)
  if(length(which(data$reCalcVar!="")) > 0)
  {
    data$reCalcVar <- data$reCalcVar[which(data$reCalcVar!="")]
    data$variable <- c(data$variable, data$reCalcVar)
    data$reCalcVar <- NULL
  }
  data
}

.tryCloseH5 <- function(){
  try(rhdf5::H5close(), silent = TRUE)
}

