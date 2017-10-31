.convertH5Filtering <- function(h5requestFiltering, x)
{
  if(length(h5requestFiltering)>0)
  {
    if(!is.list(h5requestFiltering[[1]])){
      if(!any(c("simOptions", "antaresDataTable") %in%class(x)))
      {
        h5requestFiltering <- rep(list(h5requestFiltering), length(x))
      }else{
        h5requestFiltering <- list(h5requestFiltering)
      }
    }else{
      if(class(x) == "list"){
        if(length(h5requestFiltering) != length(x)){
          h5requestFiltering <- h5requestFiltering[1:length(x)%%length(h5requestFiltering) + 1]
        }
      }
    }
  }else{
    if(!any(c("simOptions", "antaresDataTable") %in%class(x)))
    {
      h5requestFiltering <- replicate(length(x), list())
    }else{
      h5requestFiltering <- replicate(1, list())
    }
  }
  h5requestFiltering
}



.getTableInH5 <- function(fid, timeStep){
  dataExist <- NULL
  if(rhdf5::H5Lexists(fid, paste0(timeStep, "/areas")))
  {
    dataExist <- c(dataExist, "areas")
  }
  if(rhdf5::H5Lexists(fid, paste0(timeStep, "/links")))
  {
    dataExist <- c(dataExist, "links")
  }
  if(rhdf5::H5Lexists(fid, paste0(timeStep, "/clusters")))
  {
    dataExist <- c(dataExist, "clusters")
  }
  if(rhdf5::H5Lexists(fid, paste0(timeStep, "/districts")))
  {
    dataExist <- c(dataExist, "districts")
  }
  dataExist
}

.getVariablesH5 <- function(fid, timeStep, tables){
  sapply(tables, function(X){
    struct <- .getstructure(fid, paste0(timeStep, "/", X, "/mcInd/", "/structure"))$variable
    if("timeId"%in%struct){
      struct <- struct[struct!="timeId"]
    }
    struct
  }, simplify = FALSE)
}

.getClustersNames <- function(fid, timeStep){
  unique(unlist(lapply(strsplit(.getstructure(fid, paste0(timeStep, "/clusters/mcInd/structure"))$cluster, "/"), function(X)X[1])))
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
  tim <- .timeIdToDate(sort(
    unique(
      antaresRead::.h5ReadAntares(opts$h5path, timeStep = timeStep, select = "timeId", 
                                 areas = opts$areaList[1], mcYears = opts$mcYears[1], perf = FALSE)$timeId)
  ), timeStep = timeStep, opts = opts)
  dt <- as.Date(range(tim))
  dt
}


.getGraphFunction <- function(type){
  switch(type,
         "ts" = .plotTS,
         "barplot" = .barplot,
         "monotone" = .plotMonotone,
         "density" = .density,
         "cdf" = .cdf,
         "heatmap" = .heatmap,
         stop("Invalid type")
  )
}

.getTimeStep <- function(fid){
  timeSteps <- sapply(c("hourly", "daily", "weekly", "monthly", "annual"), function(X){
    rhdf5::H5Lexists(fid, X)
  })
  names(timeSteps[which(timeSteps == TRUE)])
}

.compareOperation <- function(a, opType){
  if(length(a) == 1) return(unlist(unique(a)))
  if(opType == "union") return(Reduce(union, a))
  if(opType == "intersect") return(Reduce(intersect, a))
}
