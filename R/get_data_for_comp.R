# Copyright © 2016 RTE Réseau de transport d’électricité

#' Private function that process antaresData objects to prepare them for comparison.
#' 
#' @param processFun Function to apply to each antaresData object.
#' @inheritParams tsPlot
#' 
#' @return A list with the following elements:
#' - x: list of processed data objects (antaresDataList by default)
#' - compare: argument for manipulateWidget
#' - compareOpts: argument for manipulateWidget
#' 
#' @noRd
#' 
.getDataForComp <- function(x, y = NULL, compare = NULL, compareOpts = NULL, processFun = as.antaresDataList, ...) {
  
  if(!is.list(x)){return(NULL)}
  if (is.null(compareOpts)) compareOpts <- list()

  assert_that(is.function(processFun))
  
  
  
  if (inherits(x, "antaresData")) {
    x <- processFun(x, ...)
    
    if (!is.null(y)) {
      assert_that(inherits(y, "antaresData"))
      
      if (is.null(compare)) compare <- list()
      y <- processFun(y, ...)
      x <- list(x, y)
      compareOpts$ncharts <- 2
    } else {
      if (is.null(compareOpts$ncharts)) {
        if (is.null(compare)) compareOpts$ncharts <- 1
        else compareOpts$ncharts <- 2
      }
      
      x <- replicate(compareOpts$ncharts, x, simplify = FALSE)
    }
  } else {
    assert_that(is.list(x))
    assert_that(all(sapply(x, inherits, what = "antaresData")), 
                msg = "'x' is not an antaresData or a list of antaresData objects")
    x <- lapply(x, processFun, ...)
    compareOpts$ncharts <- length(x)
    if (is.null(compare)) compare <- list()
  }
  
  compareOpts <- do.call(compareOptions, compareOpts)
  
  list(
    x = x,
    compare = compare,
    compareOpts = compareOpts
  )
}