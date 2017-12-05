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
                       dateRange = NULL, aggregate = c("none", "mean", "sum", "mean by areas", "sum by areas")) {
  
  if(length(variable) == 0){return(tpl[0])}
  if("all" %in% elements) elements <- uniqueElement
  aggregate <- match.arg(aggregate)
  assert_that(inherits(x, "data.table"))
  assert_that(inherits(tpl, "data.table"))
  assert_that(are_equal(nrow(x), nrow(tpl)))
  assert_that(all(sapply(variable, is.string)))
  
  variable <- variable[variable%in%names(x)]
  if (!is.null(dateRange)) assert_that(are_equal(length(dateRange), 2))
  
  listVar <- sapply(variable, function(V){
    tpl$value <- x[,.SD, .SDcols = V]
    tpl
  }, simplify = FALSE)
  if(length(listVar) > 1){
    sapply(names(listVar), function(N){
      listVar[[N]][,element := paste(element, '__' , N)]
    })
    tpl <- rbindlist(listVar)
    elements <- as.vector(sapply(elements, function(X){paste(X, "__", variable)}))
  }else{
    tpl <- listVar[[1]]
  }
  
  # Filtering data if required
  if (!is.null(mcYear) && mcYear != "average") {
    mcy <- mcYear # Just to avoid name confusion in the next line
    tpl <- tpl[mcYear %in% mcy]
  }else{
    if(!"mcYear" %in% names(tpl))
      if(mcYear != "average")
      {
      .printWarningMcYear()
      }
  }
  
  # if (length(elements) == 0) elements <- uniqueElement[1:5]
  if (!"all" %in% elements) tpl <- tpl[element %in% elements]
  if (!is.null(dateRange)) tpl <- tpl[as.Date(time) %between% dateRange]
  
  # Aggregating values
  if (aggregate != "none" && length(uniqueElement) > 1) {
    if (aggregate == "mean") {
      if(length(variable) == 1){
      tpl <- tpl[, .(element = as.factor(variable), value = mean(value)), 
                 by = c(.idCols(tpl))]
      }else{
        tpl <- tpl[, .(element = as.factor("Mean"), value = mean(value)), 
                   by = c(.idCols(tpl))]
      }
      
    } else if (aggregate == "sum") {
      
      if(length(variable) == 1){
      tpl <- tpl[, .(element = as.factor(variable), value = sum(value)), 
                 by = c(.idCols(tpl))]
      }else{
        tpl <- tpl[, .(element = as.factor("Sum"), value = sum(value)), 
                   by = c(.idCols(tpl))]
      }
    } else if (aggregate == "mean by areas"){

      tpl$areas <- unlist(lapply(strsplit(tpl$element, "__"),function(X) X[1]))
      tpl$element <- unlist(lapply(strsplit(tpl$element, "__"),function(X) X[2]))
      
      tpl <- tpl[, .(value = mean(value)), 
                 by = c(.idCols(tpl), "element")]
    } else if (aggregate == "sum by areas"){

      tpl$areas <- unlist(lapply(strsplit(tpl$element, "__"),function(X) X[1]))
      tpl$element <- unlist(lapply(strsplit(tpl$element, "__"),function(X) X[2]))
      
      tpl <- tpl[, .(value = sum(value)), 
                        by = c(.idCols(tpl), "element")]
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
  x <- sapply(1:length(x),function(zz){
    .loadH5Data(share, x[[zz]], h5requestFilter = h5requestFiltering[[zz]])
  }, simplify = FALSE)
  x
}


.validCompare <- function(compare, values){
  if(!is.null(compare)){
    if(is.list(compare)){
      compare_values <- names(compare)
    } else if(is.vector(compare)){
      compare_values <- compare
    } else {
      stop("'compare' must be a vector or a named list")
    }
    if(!all(compare_values %in% values)){
      invalid <- compare_values[!compare_values %in% values]
      stop(paste0("Invalid arguments for 'compare' : '", paste0(invalid, collapse = "', '"),
                  "'. Possible values : '", paste0(values, collapse = "', '"), "'."))
    }
  }
  invisible(TRUE)
}