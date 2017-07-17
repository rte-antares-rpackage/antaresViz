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
.getDataForComp <- function(x, y, compare, compareOpts, processFun = as.antaresDataList) {
  if (is.null(compareOpts)) compareOpts <- list()
  
  if (inherits(x, "antaresData")) {
    x <- processFun(x)
    
    if (!is.null(y)) {
      if (is.null(compare)) compare <- list()
      y <- processFun(y)
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
    x <- lapply(x, processFun)
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