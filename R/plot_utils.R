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
                       dateRange = NULL, aggregate = c("none", "mean", "sum")) {
  
  if(!"mcYear" %in% names(x) && !is.null(mcYear) && mcYear != "average"){
    warning("You have average data and you specify a mcYear. This specification will be ignore.")
  }
  
  
  if(length(variable) == 0){return(tpl[0])}
  
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
      listVar[[N]][,element := paste(element, '-' , N)]
    })
    tpl <- rbindlist(listVar)
    elements <- as.vector(sapply(elements, function(X){paste(X, "-", variable)}))
  }else{
    tpl <- listVar[[1]]
  }

  # Filtering data if required
  if (!is.null(mcYear) && mcYear != "average") {
    mcy <- mcYear # Just to avoid name confusion in the next line
    tpl <- tpl[mcYear %in% mcy]
  }
  # if (length(elements) == 0) elements <- uniqueElement[1:5]
  if (!"all" %in% elements) tpl <- tpl[element %in% elements]
  if (!is.null(dateRange)) tpl <- tpl[as.Date(time) %between% dateRange]
  
  # Aggregating values
  if (aggregate != "none" && length(uniqueElement) > 1) {
    if (aggregate == "mean") {
      tpl <- tpl[, .(element = as.factor(variable), value = mean(value)), 
               by = c(.idCols(tpl))]
    } else if (aggregate == "sum") {
      tpl <- tpl[, .(element = as.factor(variable), value = sum(value)), 
               by = c(.idCols(tpl))]
    }
  }
  tpl
}