#' Get parameters for time series plots
#' 
#' @param x antaresDataList
#' 
#' @return A list with one element per table in x. Each element is itself a list
#'   of parameters that are used in the the time series plot functions:
#'   - x: original table
#'   - tpl: template table containing columns element, timeId,
#'          time, value and eventually column mcYear. The only purpose of this
#'          table is to be used in .getTSData.
#'   - uniqueElement: vector of unique elements in table 'tpl'
#'   - idCols: id columns in x
#'   - valueCols: value columns in x
#'   - showConfInt: should confidence interval parameter be visible in the UI?
#'   - dataDateRange: observed date range in x
#'   - dateRange: date range to be used in the initial plot
#'   - uniqueMcYears: unique mcYears observed in x
#'   - elements: elements to be used in the initial plot
#' @noRd
.getParsForTsPlot <- function(x, elements = NULL, dateRange = NULL) {
  assert_that(inherits(x, "antaresData"))
  x <- as.antaresDataList(x)
  
  lapply(x, function(x) {
    idCols <- .idCols(x)
    tpl <- x[, .(
      timeId = timeId,
      time = .timeIdToDate(timeId, attr(x, "timeStep"), simOptions(x)), 
      value = 0)
      ]
    
    if ("cluster" %in% idCols) {
      tpl$element <- paste(x$area, x$cluster, sep = " > ")
    } else if ("district" %in% idCols) {
      tpl$element <- x$district
    } else if ("link" %in% idCols) {
      tpl$element <- x$link
    } else if ("area" %in% idCols) {
      tpl$element <- x$area
    } else stop("No Id column")
    
    if ("mcYear" %in% names(x) && length(unique(x$mcYear)) > 1) {
      tpl$mcYear <- x$mcYear
    }
    
    dataDateRange <- as.Date(range(tpl$time))
    if (is.null(dateRange) || length(dateRange) < 2) dateRange <- dataDateRange
    
    uniqueElem <- sort(as.character(unique(tpl$element)))
    if (is.null(elements)) {
      elements <- uniqueElem
      if (length(elements) > 5) elements <- elements[1:5]
    }
    
    list(
      dt = tpl,
      x = x,
      idCols = idCols,
      valueCols = setdiff(names(x), idCols),
      showConfInt = !is.null(x$mcYear) && length(unique(x$mcYear) > 1),
      dataDateRange = dataDateRange,
      dateRange = dateRange,
      uniqueElem = uniqueElem,
      uniqueMcYears = unique(x$mcYear),
      elements = elements,
      timeStep = attr(x, "timeStep"),
      opts = simOptions(x)
    )
  })
}


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
  
  aggregate <- match.arg(aggregate)
  
  assert_that(inherits(x, "data.table"))
  assert_that(inherits(tpl, "data.table"))
  assert_that(are_equal(nrow(x), nrow(tpl)))
  assert_that(is.string(variable))
  if (!is.null(dateRange)) assert_that(are_equal(length(dateRange), 2))
  
  tpl$value <- x[[variable]]
  
  # Filtering data if required
  if (!is.null(mcYear) && mcYear != "average") {
    mcy <- mcYear # Just to avoid name confusion in the next line
    tpl <- tpl[mcYear == mcy]
  }
  
  if (length(elements) == 0) elements <- uniqueElement[1:5]
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