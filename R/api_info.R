
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
    fid <- H5Fopen(opts$h5path)
    timeStepS <- .getTimStep(fid)
    timeStepS <- as.character(timeStepS)
    mcYearS <- opts$mcYears
    tabl <- .getTableInH5(fid, timeStepS[1])
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
    fid <- H5Fopen(opts$h5path)
    timeStepS <- .getTimStep(fid)
    timeStepS <- as.character(timeStepS)
    mcYearS <- opts$mcYears
    tabl <- .getTableInH5(fid, timeStepS[1])
    tabl <- tabl[tabl%in%c("areas", "districts")]
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
    ret$timeStepS <- .compareopetation(list(xPart$timeStepS, yPart$timeStepS), xyCompare)
    ret$mcYearS <- sort(.compareopetation(list(xPart$mcYearS, yPart$mcYearS), xyCompare))
    ret$tabl <- .compareopetation(list(xPart$tabl, yPart$tabl), xyCompare)
  }
  ret
}