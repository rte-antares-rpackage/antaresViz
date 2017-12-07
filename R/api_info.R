.giveH5DataToApi <- function(sharerequest, infos, areas = NULL, links = NULL, clusters = NULL, districts = NULL){
  if(infos$isH5){
    gc()
    if(length(sharerequest$mcYearh_l)==0) {mcYearh2 <- NULL}else{
      if("all"%in%sharerequest$mcYearh_l){
        mcYearh2 <- "all"
      }else{
        mcYearh2 <- as.numeric(sharerequest$mcYearh_l)
      }
    }
    if(!is.null(sharerequest$tables_l))
    {
    if("areas" %in% sharerequest$tables_l){
      areas <- "all"
    }
    if("links" %in% sharerequest$tables_l){
      links <- "all"
    }
    if("clusters" %in% sharerequest$tables_l){
      clusters <- "all"
    }
    if("districts" %in% sharerequest$tables_l){
      districts <- "all"
    }
    }
    readAntares(areas = areas, links = links, clusters = clusters,districts = districts , mcYears = mcYearh2,
                timeStep = sharerequest$timeSteph5_l, opts = infos$dataInput)
  }else{
    infos$dataInput
  }
}








.giveDateInfos <- function(yD, params, xyCompare, minMax, tabl = NULL){
  use <- NULL
  nulTab <- is.null(tabl)
  if(!is.null(params))
  {
    if(minMax == "min")
    {
      if(is.null(yD)){
        if(!nulTab)
        {
          use <- params$x[[1]][[tabl]]$dataDateRange[1]
        }
        else{
        use <- params$x[[1]]$dataDateRange[1]
        }
      }else if(xyCompare == "union"){
        use <- min(
          do.call("c",(lapply(params$x, function(vv){
            if(nulTab){
            unique(vv$dataDateRange[1])
            }else{
              vv[[tabl]]$dataDateRange[1]
            }
            }
            ))))
      } else if(xyCompare == "intersect"){
        use <- max(
          do.call("c",(lapply(params$x, function(vv){
            if(nulTab){
              unique(vv$dataDateRange[1])
            }else{
              vv[[tabl]]$dataDateRange[1]
            }
          }
          ))))
      }
    }
    if(minMax == "max")
    {
      if(is.null(yD)){
        if(!nulTab)
        {
          use <- params$x[[1]][[tabl]]$dataDateRange[2]
        }
        else{
          use <- params$x[[1]]$dataDateRange[2]
        }
      }else if(xyCompare == "union"){
        use <- max(
          do.call("c",(lapply(params$x, function(vv){
            if(nulTab){
              unique(vv$dataDateRange[2])
            }else{
              vv[[tabl]]$dataDateRange[2]
            }
          }
          ))))
      } else if(xyCompare == "intersect"){
        use <- min(
          do.call("c",(lapply(params$x, function(vv){
            if(nulTab){
              unique(vv$dataDateRange[2])
            }else{
              vv[[tabl]]$dataDateRange[2]
            }
          }
          ))))
      }
    }}
  use
}

.giveParamH5 <- function(X_I, Y_I, xyCompare){
  if(X_I$isH5){
    opts <- X_I$dataInput
    fid <- rhdf5::H5Fopen(opts$h5path)
    timeStepS <- .getTimeStep(fid)
    timeStepS <- as.character(timeStepS)
    mcYearS <- opts$mcYears
    tabl <- .getTableInH5(fid, timeStepS[1])
    rhdf5::H5Fclose(fid)
    xPart = list(
      timeStepS = timeStepS,
      mcYearS = mcYearS,
      tabl = tabl
    )
    
  }else{
    xPart = NULL
  }
  if(Y_I$isH5){
    opts <- Y_I$dataInput
    fid <- rhdf5::H5Fopen(opts$h5path)
    timeStepS <- .getTimeStep(fid)
    timeStepS <- as.character(timeStepS)
    mcYearS <- opts$mcYears
    tabl <- .getTableInH5(fid, timeStepS[1])
    tabl <- tabl[tabl%in%c("areas", "districts")]
    rhdf5::H5Fclose(fid)
    yPart = list(
      timeStepS = timeStepS,
      mcYearS = mcYearS,
      tabl = tabl
    )
  }else{
    yPart = NULL
  }
  if(is.null(xPart) & is.null(yPart)){
    ret <- NULL
  }else if(is.null(xPart)){
    ret <- yPart
  }else if(is.null(yPart)){
    ret <- xPart
  }else{
    ret <- list() 
    ret$timeStepS <- .compareOperation(list(xPart$timeStepS, yPart$timeStepS), xyCompare)
    ret$mcYearS <- sort(.compareOperation(list(xPart$mcYearS, yPart$mcYearS), xyCompare))
    ret$tabl <- .compareOperation(list(xPart$tabl, yPart$tabl), xyCompare)
  }
  rhdf5::H5close()
  ret
}