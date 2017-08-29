.getTableInH5 <- function(fid, timeStep){
  dataExist <- NULL
  if(H5Lexists(fid, paste0(timeStep, "/areas")))
  {
    dataExist <- c(dataExist, "areas")
  }
  if(H5Lexists(fid, paste0(timeStep, "/links")))
  {
    dataExist <- c(dataExist, "links")
  }
  if(H5Lexists(fid, paste0(timeStep, "/clusters")))
  {
    dataExist <- c(dataExist, "clusters")
  }
  if(H5Lexists(fid, paste0(timeStep, "/districts")))
  {
    dataExist <- c(dataExist, "districts")
  }
  dataExist
}

.getVariablesH5 <- function(fid, timeStep, tables){
  sapply(tables, function(X){
    struct <- antaresHdf5:::.getstructure(fid, paste0(timeStep, "/", X, "/mcInd/", "/structure"))$variable
    if("timeId"%in%struct){
      struct <- struct[struct!="timeId"]
    }
    struct
  }, simplify = FALSE)
}

.getClustersNames <- function(fid, timeStep){
  antaresHdf5:::.getstructure(fid, paste0(timeStep, "/clusters/mcInd/structure"))$cluster
}

.getElements <- function(opts, tables, fid, timeStep){
  elements <- list()
  if("areas" %in% tables) elements$areas <- opts$areaList
  if("links" %in% tables) elements$links <- opts$linkList
  if("districts" %in% tables) elements$districts <- opts$districtList
  if("clusters" %in% tables){
    elements$clusters  <- .getClustersNames(fid, timeStep)
  }
  elements
}

.getDateRange <- function(opts, timeStep){
  tim <- .timeIdToDate(sort(unique(h5ReadAntares(opts$h5path, timeStep = timeStep, select = "timeId", areas = opts$areaList[1],
                                     mcYears = opts$mcYears[1], perf = FALSE)$timeId)),
                         timeStep = timeStep,opts = opts)
  dt <- as.Date(range(tim))
  
  dt
}


.getGraphFunction <- function(type){
  f <- switch(type,
              "ts" = .plotTS,
              "barplot" = .barplot,
              "monotone" = .plotMonotone,
              "density" = .density,
              "cdf" = .cdf,
              "heatmap" = .heatmap,
              stop("Invalid type")
  )
  f
}

.getTimStep <- function(fid){
  timeSteps <- sapply(c("hourly", "daily", "weekly", "monthly", "annual"), function(X){
    H5Lexists(fid, X)
  })
  names(timeSteps[which(timeSteps == TRUE)])
}
