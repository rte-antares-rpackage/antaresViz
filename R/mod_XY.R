# Copyright © 2016 RTE Réseau de transport d’électricité

#' Make X-Y bockey plot, interactive version
#' 
#' @param x optsH5 or list of optsH5
#' @param xyCompare
#'   Use when you compare studies, can be "union" or "intersect". If union, all
#'   of mcYears in one of studies will be selectable. If intersect, only mcYears in all
#'   studies will be selectable.
#'   
#' 
#' @examples 
#' \dontrun{
#' opts <- setSimulationPath("h5File")
#' modXY(opts)
#' modXY(list(opts, opts))
#' 
#' }
#' 
#' @export
modXY <- function(x, xyCompare = c("union","intersect"))
{
  #remove notes
  x_in <- timeSteph5 <- allVar <- NULL
  transformFunction <- x_tranform <- dateRange <- variableX <- variableY <- paramsH5 <- mcYearh <- sharerequest <- NULL
  compareOptions <- .compOpts(x, NULL)
  compare <- NULL
  if(compareOptions$ncharts>1)compare<-""
  xyCompare <- match.arg(xyCompare)
  manipulateWidget(
    {
      transform <- NULL
      if(transformFunction == "log"){
        transform <- log
      }
      dt <- list()
      bock <- list()
      if(!is.null(x_tranform[[.id]]))
      {
     try(plotXY(.selectByRange(x_tranform[[.id]], dateRange),
                               x = variableX,y = variableY, transform = transform), silent = TRUE)
      }
    },
    x = mwSharedValue({x}),
    x_in = mwSharedValue({
      .giveListFormat(x)
    }),
    paramsH5 = mwSharedValue({
      paramsH5List <- .h5ParamList(X_I = x_in, xyCompare = xyCompare)
      rhdf5::H5close()
      paramsH5List
    }),
    H5request = mwGroup(
      timeSteph5 = mwSelect(choices = paramsH5$timeStepS,
                            value =  paramsH5$timeStepS[1],
                            label = "timeStep",
                            multiple = FALSE),
      tables = mwSelect(choices = paramsH5[["tabl"]],
                        value = {
                          if(.initial) {paramsH5[["tabl"]]} else {NULL}
                        },
                        label = "table", multiple = TRUE),
      mcYearh = mwSelect(choices = c(paramsH5[["mcYearS"]]),
                         value = {
                           if(.initial){paramsH5[["mcYearS"]][1]}else{NULL}
                         },
                         label = "mcYear", multiple = TRUE),
      .display = {any(unlist(lapply(x_in, .isSimOpts)))}
    ),
    sharerequest = mwSharedValue({
      list(timeSteph5_l = timeSteph5, mcYearh_l = mcYearh, tables_l = tables)
    }),
    x_tranform = mwSharedValue({
      sapply(1:length(x_in),function(zz){
        dta <- mergeAllAntaresData(.loadH5Data(sharerequest, x_in[[zz]]))
        dta$timeId <- .timeIdToDate(dta$timeId, timeSteph5,  x_in[[zz]])
        dta
      }, simplify = FALSE)
   
    }),
    transformFunction = mwSelect(c("None", "log")),
    allVar = mwSharedValue({.compareOperation(
      {var = lapply(x_tranform, function(X){names(X)[unlist(lapply(X, class)%in%c("numeric", "integer"))]})
      var}
      , xyCompare)}),
    variableX = mwSelect(choices = {
      variableY
      allVar
      }),
    variableY = mwSelect(choices = allVar[!allVar%in%variableX]),
    dateRange = mwDateRange(
      value = {
        if(.initial & !is.null(x_tranform)) range(x_tranform[[1]]$timeId)
        else NULL
      },
      min = if(!is.null(x_tranform))range(x_tranform[[1]]$timeId)[1], 
      max = if(!is.null(x_tranform))range(x_tranform[[1]]$timeId)[2],label = "Daterange"
    ),
    .compare = {
      compare
    },
    .compareOpts = {
      compareOptions
    }
  )
}

.selectByRange <- function(X, dateRange){
  X[timeId>=dateRange[1] & timeId<=dateRange[2]]
}
