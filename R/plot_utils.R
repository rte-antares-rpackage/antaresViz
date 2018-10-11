#' Return a table representing a time series.
#' 
#' @param x a table of class antaresDataTable
#' @param tpl template of a time series. It must contain columns element, timeId,
#'   time, value and eventually column mcYear
#' @param variable name of one column in x 
#' @param uniqueElement a vector containing the unique elements present in x.
#' @param mcYear Monte-Carlo year to keep
#' @param dateRange a vector of two dates
#' @param aggregate type of aggregation to perform
#' 
#' @return A table containing the same columns as tpl: element, timeId,
#'   time, value and eventually column mcYear
#' 
#' @noRd
.getTSData <- function(x, tpl, variable, elements, 
                       uniqueElement = unique(tpl$element), 
                       mcYear = NULL, 
                       dateRange = NULL, aggregate = c("none", "mean", "sum", "mean by areas", "sum by areas"), 
                       typeConfInt = FALSE) {
  
  if (length(variable) == 0){return(tpl[0])}
  if ("all" %in% elements) elements <- uniqueElement
  aggregate <- match.arg(aggregate)
  assert_that(inherits(x, "data.table"))
  assert_that(inherits(tpl, "data.table"))
  assert_that(are_equal(nrow(x), nrow(tpl)))
  assert_that(all(sapply(variable, is.string)))
  
  variable <- variable[variable %in% names(x)]
  if (!is.null(dateRange)) assert_that(are_equal(length(dateRange), 2))
  
  listVar <- sapply(variable, function(V){
    tpl$value <- x[, .SD, .SDcols = V]
    tpl
  }, simplify = FALSE)
  if (length(listVar) > 1){
    sapply(names(listVar), function(N){
      listVar[[N]][, element := paste(element, "__", N)]
    })
    tpl <- rbindlist(listVar)
    elements <- as.vector(sapply(elements, function(X){paste(X, "__", variable)}))
  }else{
    if (aggregate %in% c("mean by areas", "sum by areas")){
      tpl <- listVar[[1]][, element := paste(element, "__", names(listVar)[1])]
      elements <- paste(elements, "__", variable)
    } else {
      tpl <- listVar[[1]]
    }
  }
  

  # Filtering data if required
  if (!is.null(mcYear) && length(mcYear) > 0 && mcYear != "average") {
    mcy <- mcYear # Just to avoid name confusion in the next line
    tpl <- tpl[mcYear %in% mcy]
  }else{
    if (!"mcYear" %in% names(tpl))
      if (!is.null(mcYear) && length(mcYear) > 0 && mcYear != "average"){
      .printWarningMcYear()
      }
  }

  # if (length(elements) == 0) elements <- uniqueElement[1:5]
  if (!"all" %in% elements) tpl <- tpl[element %in% elements]
  if (!is.null(dateRange)) tpl <- tpl[as.Date(time) %between% dateRange]
  
  # Aggregating values
  if (aggregate != "none" && length(uniqueElement) > 1) {
    if (aggregate == "mean") {
      if (length(variable) == 1){
      tpl <- tpl[, .(element = as.factor(variable), value = mean(value)), 
                 by = c(.idCols(tpl))]
      }else{
        tpl <- tpl[, .(element = as.factor("Mean"), value = mean(value)), 
                   by = c(.idCols(tpl))]
      }
      
    } else if (aggregate == "sum") {
      
      if (length(variable) == 1){
      tpl <- tpl[, .(element = as.factor(variable), value = sum(value)), 
                 by = c(.idCols(tpl))]
      }else{
        tpl <- tpl[, .(element = as.factor("Sum"), value = sum(value)), 
                   by = c(.idCols(tpl))]
      }
    } else if (aggregate == "mean by areas"){

      tpl$areas <- unlist(lapply(strsplit(tpl$element, "__"), function(X) X[1]))
      tpl$element <- unlist(lapply(strsplit(tpl$element, "__"), function(X) X[2]))
      
      tpl <- tpl[, .(value = mean(value)), 
                 by = c(.idCols(tpl), "element")]
    } else if (aggregate == "sum by areas"){

      tpl$areas <- unlist(lapply(strsplit(tpl$element, "__"), function(X) X[1]))
      tpl$element <- unlist(lapply(strsplit(tpl$element, "__"), function(X) X[2]))
      
      tpl <- tpl[, .(value = sum(value)), 
                        by = c(.idCols(tpl), "element")]
    }
  }

  if (!typeConfInt){
    if ("mcYear" %in% names(tpl)){
      if (length(unique(tpl$mcYear)) > 1){
        tpl[, element := paste0(element, " __ mcY", mcYear)]
      }
    }
  }

  tpl
}

.printWarningMcYear <- function(){
  warning("You have mc-all data and you specify mcYear, it will be ignored")
}


.cleanH5 <- function(x, timeSteph5, mcYearh5, tablesh5, h5requestFiltering)
{
  share <- list(timeSteph5_l = timeSteph5, mcYearh_l = mcYearh5, tables_l = tablesh5)
  x <- .giveListFormat(x)
  x <- sapply(1:length(x), function(zz){
    .loadH5Data(share, x[[zz]], h5requestFilter = h5requestFiltering[[zz]])
  }, simplify = FALSE)
  x
}


.validCompare <- function(compare, values){
  if (!is.null(compare)){
    if (is.list(compare)){
      compare_values <- names(compare)
    } else if (is.vector(compare)){
      compare_values <- compare
    } else {
      stop("'compare' must be a vector or a named list")
    }
    if (!all(compare_values %in% values)){
      invalid <- compare_values[!compare_values %in% values]
      stop(paste0("Invalid arguments for 'compare' : '", paste0(invalid, collapse = "', '"),
                  "'. Possible values : '", paste0(values, collapse = "', '"), "'."))
    }
  }
  invisible(TRUE)
}

.validHidden <- function(hidden, values){
  if (!is.null(hidden)){
    if (!is.vector(hidden)){
      stop("'hidden' must be a vector")
    } else {
      if (!all(hidden %in% values)){
        invalid <- hidden[!hidden %in% values]
          stop(paste0("Invalid arguments for 'hidden' : '", paste0(invalid, collapse = "', '"),
                  "'. Possible values : '", paste0(values, collapse = "', '"), "'."))
      }
    }
  }
  invisible(TRUE)
}

#' Compare cannot work when interactive is FALSE
#' 
#' @param compareP List of option to compare
#' @param interactiveP if TRUE then return a shiny gadget and FALSE a htmlwidget  
#' 
#' @noRd
.check_compare_interactive <- function(compareP = NULL, interactiveP = NULL){
  if (!is.null(compareP) && !interactiveP){
    stop("You can't use compare in no interactive mode")
  }
}

.check_h5_param <- function(x = NULL, mcYear = NULL, interactive = NULL){
  if (!interactive && !is.null(mcYear) & !(mcYear == "average") && (.isSimOpts(x) | .isListSimOpts(x))){
    stop("You can't use mcYear for h5 file when interactive is set to FALSE. You can use mcYearh5.",
         call. = FALSE)
  }  
}

#' Check app language
#' 
#' @param language app language
#' @noRd
.check_languages <- function(language){
  # Check language
  if (!language %in% availableLanguages_labels){
    stop("Invalid 'language' argument. Must be in : ", paste(availableLanguages_labels, collapse = ", "))  
  }
}

#' Check parameters for stack
#' 
#' @param listParamsCheck a list of param to check and correct 
#' @noRd
.check_params_A_get_cor_val <- function(listParamsCheck = NULL){
  .check_compare_interactive(listParamsCheck$compare, listParamsCheck$interactive)
  .check_languages(listParamsCheck$language)
  # Check hidden
  .validHidden(listParamsCheck$hidden, listParamsCheck$valHidden)
  #Check compare
  .validCompare(listParamsCheck$compare,  listParamsCheck$valCompare)
  if (is.null(listParamsCheck$mcYear)) listParamsCheck$mcYear <- "average"
  
  if (!is.null(listParamsCheck$compare) && "list" %in% class(listParamsCheck$x)){
    if (length(listParamsCheck$x) == 1) listParamsCheck$x <- list(listParamsCheck$x[[1]], listParamsCheck$x[[1]])
  }
  if (!is.null(listParamsCheck$compare) && ("antaresData" %in% class(listParamsCheck$x)  | "simOptions" %in% class(listParamsCheck$x))){
    listParamsCheck$x <- list(listParamsCheck$x, listParamsCheck$x)
  }
  
  listParamsCheck$h5requestFiltering <- .convertH5Filtering(h5requestFiltering = listParamsCheck$h5requestFiltering, x = listParamsCheck$x)
  
  listParamsCheck$compareOptions <- .compOpts(listParamsCheck$x, listParamsCheck$compare)
  if (is.null(listParamsCheck$compare)){
    if (listParamsCheck$compareOptions$ncharts > 1){
      listParamsCheck$compare <- list()
    }
  }
  
  listParamsCheck
}

.check_x <- function(x = NULL){
  if ("list" %in% class(x)){
    for (elementI in x){
      .check_x(elementI)
    }
  }else{
    if (!(!.isSimOpts(x) | !.isAntaresData(x))){
      stop("'x' should be an object of class 'antaresData' (or 'simOptions') created with 'readAntares()' (or 'setSimulationPath()')")
    }else{
      return(TRUE)
    } 
  }
}


.check_x_simOptions <- function(x = NULL){
  if (!.isSimOpts(x)){
    stop("'x' should be an object of class 'simOptions' created with 'setSimulationPath()'")
  }else {
    return(TRUE)
  }
}

.check_x_antaresData <- function(x = NULL){
  if (!.isAntaresData(x)){
    stop("'x' should be an object of class 'antaresData created with readAntares()' or an opts")
  }else {
    return(TRUE)
  }
}

.getParamsNoInt <- function(x = NULL, 
                            refStudy = NULL, 
                            listParamH5NoInt = NULL, 
                            compare = NULL, 
                            compareOptions = NULL, 
                            processFun = NULL,
                            ...){
  
  timeSteph5 <- listParamH5NoInt$timeSteph5
  mcYearh5 <- listParamH5NoInt$mcYearh5
  tablesh5 <- listParamH5NoInt$tablesh5
  h5requestFiltering <- listParamH5NoInt$h5requestFiltering
  
  x <- .cleanH5(x, timeSteph5, mcYearh5, tablesh5, h5requestFiltering)
  
  if (!is.null(refStudy)){
    refStudy <- .cleanH5(refStudy, timeSteph5, mcYearh5, tablesh5, h5requestFiltering)
    x <- .compare_with_ref_study(x = x, refStudy = refStudy)
  }
  
  params <- .getDataForComp(x = .giveListFormat(x),
                            y = NULL,
                            compare = compare, 
                            compareOpts = compareOptions, 
                            processFun = processFun,
                            ...)
  
  params
}

#' Get x for manipulateWidget
#' 
#' @noRd
.get_x_transform <- function(x_in = NULL, sharerequest = NULL, refStudy = NULL, h5requestFilter = NULL, ...){
  dots <- list(...)
  areas <- dots$areas
  links <- dots$links
  if (!is.null(refStudy)){
    refStudyTrans <- .loadH5Data(sharerequest, 
                                 refStudy, 
                                 h5requestFilter = h5requestFilter[[1]], 
                                 areas = areas,
                                 links = links)
  }
  
  dataInApp <- sapply(1:length(x_in), function(zz){
    x_in[[zz]] <- .loadH5Data(sharerequest,
                              x_in[[zz]], 
                              h5requestFilter = h5requestFilter[[zz]],
                              areas = areas,
                              links = links)
    
    if (!is.null(refStudy)){
      if (!is(x_in[[zz]], "simOptions")){
        x_in[[zz]] <- .compare_with_ref_study(x = as.antaresDataList(x_in[[zz]]), refStudy = as.antaresDataList(refStudyTrans))
      }else{
        x_in[[zz]] <- .compare_with_ref_study(x = x_in[[zz]], refStudy = refStudyTrans)
      }
    }
    x_in[[zz]]
  }, simplify = FALSE)
  
  if (is.null(dataInApp)){
    stop("no data")
  }
  dataInApp
}


#' Get Data from result returned by plotMap
#' 
#' @noRd
.getDataFromPlotMap <- function(area = NULL, time = NULL, variable = NULL, htmlPlotMap = NULL, idWidget = NULL){

  widgetTest <- .get_chart_or_widget(htmlPlotMap , idWidget)
  dates <- .getAllDates(htmlPlotMap, idWidget)
  indexTime <- .getIndexOneDate(time, dates)
  indexArea <- .getIndexOneArea(area, htmlPlotMap, idWidget)
  indexVar <- .getIndexOneVar(variable, htmlPlotMap, idWidget)
  res <- widgetTest$x$calls[[13]]$args[[2]][[indexArea]][[indexTime, indexVar]]
  
  return(res)
}

.get_chart_or_widget <- function(htmlPlotMap = NULL, idWidget = NULL){
  .check_if_is_html_cont(htmlPlotMap)
  
  if("htmlwidget" %in% class(htmlPlotMap)){
    typeChart <- htmlPlotMap$widgets
  }else{
    typeChart <- htmlPlotMap$charts
  }
  
  if(is.null(idWidget)){
    if(length(typeChart) > 1){
      stop("you must specify the param 'idWidget'")
    }else{
      idWidget <- 1
    }
  }
  #get charts or plots to check 
  chartToCheck <- typeChart[[idWidget]]
  
  chartToCheck
}

.getIndexOneVar <- function(variable = NULL, htmlPlotMap = NULL, idWidget = NULL){
  labelVarS <- .getLabelPlotMap(htmlPlotMap, idWidget)
  grep(paste0("^",variable,"$"), labelVarS)
}

.getIndexOneArea <- function(area = NULL, htmlPlotMap = NULL, idWidget = NULL){
  listAreas <- .getAreaNamePlotMap(htmlPlotMap, idWidget)
  grep(paste0("^",area,"$"), listAreas)
}

.getIndexOneDate <- function( date = NULL, dates = NULL){
  res <- grep(date, dates)
  if(length(res) < 1){
    dataT <- .translateToEn(date)
    res <- grep(dataT, dates)
  }
  if(length(res) < 1){
    stop("no date in dates")
  }
  res
}

#translate date into english date for dygraph on appveyour and travis
# localy, R works on french and not in english
.translateToEn <- function(date = NULL){
  frenchDay <- c("lun.", "mar.", "mer.", "jeu.", "ven.", "sam.", "dim.")
  enDay <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  #only two months now...
  frenchMonth <- c("avr.", "mai")
  enMonth <- c("Apr", "May")
  
  for(indexDay in 1:length(frenchDay)){
    if(grepl(pattern = frenchDay[indexDay], x = date)){
      date <- gsub(pattern = frenchDay[indexDay], 
                   replacement = enDay[indexDay], 
                   x = date)
    }
  }
  for(indexMonth in 1:length(frenchMonth)){
    if(grepl(pattern = frenchMonth[indexMonth], x = date)){
      date <- gsub(pattern = frenchMonth[indexMonth], 
                   replacement = enMonth[indexMonth], 
                   x = date)
    }
  }
  date
}

.getAllDates <- function(htmlPlotMap = NULL, idWidget = NULL){
  widgetTest <- .get_chart_or_widget(htmlPlotMap , idWidget)
  return(widgetTest$x$calls[[9]]$args[[2]])
}

.getLabelPlotMap <- function(htmlPlotMap = NULL, idWidget = NULL){
  widgetTest <- .get_chart_or_widget(htmlPlotMap , idWidget)
  return(widgetTest$x$calls[[13]]$args[[7]]$labels)
}

.getAreaNamePlotMap <- function(htmlPlotMap = NULL, idWidget = NULL){
  widgetTest <- .get_chart_or_widget(htmlPlotMap , idWidget)
  return(widgetTest$x$calls[[2]]$args[[2]])
}
