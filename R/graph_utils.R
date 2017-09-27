#' Compare options from length of data
#' 
#' @param x list of data
#' @param compare character
#' 
#' @noRd
.compOpts <- function(x, compare){
  len <- 1
  if(!is.null(compare)){
    return(
      list(ncharts = 2,
           nrow = 2,
           ncol = 1)
    )
  }
  if("list" == class(x)[1]){
    len <- length(x)
  }
  ncol = ifelse(len > 2, 2 ,1)
  nrow = floor((len-1)/2) + 1 + ifelse(len == 2, 1, 0)
  list(ncharts = len, nrow = nrow, ncol = ncol)
}

#' Join date range
#' 
#' @param params list of data
#' @param xyCompare character
#' @param minMax character
#' @param tabl character
#' 
#' @noRd
.dateRangeJoin <- function(params, xyCompare, minMax, tabl = NULL){
  if(minMax == "min" & xyCompare == "union"){
    return(min(do.call("c",(lapply(params$x, function(X){
      X[[tabl]]$dataDateRange[1]})))))
  }
  if(minMax == "min" & xyCompare == "intersect"){
    return(max(do.call("c",(lapply(params$x, function(X){
      X[[tabl]]$dataDateRange[1]})))))
  }
  if(minMax == "max" & xyCompare == "union"){
    return(max(do.call("c",(lapply(params$x, function(X){
      X[[tabl]]$dataDateRange[2]})))))
  }
  if(minMax == "max" & xyCompare == "intersect"){
    return(min(do.call("c",(lapply(params$x, function(X){
      X[[tabl]]$dataDateRange[2]})))))
  }
}



#' Join date range
#' 
#' @param x list of data
#' @param compare character
#' @param compareOpts list
#' @param processFun function
#' @param ...
#' 
#' @noRd
.transformDataForComp <- function(x, compare = NULL,
                                  compareOpts = NULL,
                                  processFun = as.antaresDataList, ...) {
  if(!is.list(x)){return(NULL)}
  if (is.null(compareOpts)) compareOpts <- list()
  assert_that(is.function(processFun))
  assert_that(is.list(x))
  assert_that(all(sapply(x, inherits, what = "antaresData")), 
              msg = "'x' is not an antaresData or a list of antaresData objects")
  print(x)
  x <- lapply(x, processFun, ...)
  compareOpts$ncharts <- length(x)
  if (is.null(compare)) compare <- list()
  compareOpts <- do.call(compareOptions, compareOpts)
  list(
    x = x,
    compare = compare,
    compareOpts = compareOpts
  )
}

#' Transform x in homogeneous format, list of antaresdatalist / opts
#' 
#' @param x list of data
#' 
#' @noRd
.giveListFormat <- function(x){
  if(.isSimOpts(x) | "antaresData" %in% class(x)){
    list(.rescoverFormat(x))
  }else{
    if("list" %in% class(x)){
      lapply(x, .rescoverFormat)
    }else{
      stop("class x must be antaresData or simOptions or list")
    }
  }
}

#' Transform x in antaresdatalist / opts
#' 
#' @param x data
#' 
#' @noRd
.rescoverFormat <- function(x){
  if("antaresData" %in% class(x))
  {
    re <- as.antaresDataList(x)
  }else{
    if(.isSimOpts(x)){
      re <- x
    }else{
      stop("class x must be antaresData or simOptions")
    }
  }
  re
}


#' Test opst
#' 
#' @param test if x is simOptions class
#' 
#' @noRd
.isSimOpts <- function(x){
  "simOptions" %in% class(x)
}


#' Load h5 data
#' 
#' @param sharerequest, list of mcYearh_l, tables_l and timeSteph5_l
#' @param dta, antaresdatalist or opts, if antaresdatalist do nothing, if opts load data
#' @param areas character
#' @param links character
#' @param clusters character
#' @param districts character
#' 
#' @noRd
.loadH5Data <- function(sharerequest, dta, areas = NULL, links = NULL, clusters = NULL, districts = NULL){
  if(.isSimOpts(dta)){
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
    as.antaresDataList(readAntares(areas = areas, links = links, clusters = clusters,districts = districts , mcYears = mcYearh2,
                                   timeStep = sharerequest$timeSteph5_l, opts = dta))
  }else{
    dta
  }
}




#' List of h5 params
#' 
#' @param X_I, list
#' @param xyCompare, character
#' 
#' @noRd
.h5ParamList <- function(X_I, xyCompare){
  listParam <- lapply(X_I, function(x){
    if(.isSimOpts(x)){
      .h5Inf(x)
    }else{
      mcY <- unique(unlist(lapply(x, function(y){unique(y$mcYears)})))
      timeStepS <- attributes(x)$timeStep
      tabl <- names(x)
      list(
        timeStepS = timeStepS,
        mcYearS = mcY,
        tabl = tabl
      )
    }
  })
  lapply(transpose(listParam), function(x){
    .compareopetation(x, xyCompare)
  })
}

#' Load information from h5 file
#' 
#' @param x, opts
#' 
#' @noRd
.h5Inf <- function(x){
  fid <- H5Fopen(x$h5path)
  timeStepS <- .getTimStep(fid)
  timeStepS <- as.character(timeStepS)
  mcYearS <- opts$mcYears
  tabl <- .getTableInH5(fid, timeStepS[1])
  xPart = list(
    timeStepS = timeStepS,
    mcYearS = mcYearS,
    tabl = tabl
  )
}


