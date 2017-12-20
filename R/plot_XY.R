# Copyright © 2016 RTE Réseau de transport d’électricité

#' Plot density between X et Y with rbokeh
#' 
#' This function take somes arguments from rbokeh and make plot.
#' 
#' @param data \code{data.frame} can be antaresData object
#' @param x \code{character}, x variable 
#' @param y \code{character}, y variable 
#' @param precision \code{numeric} precision for plot
#' @param sizeOnCount \code{boolean}, should addapt size of object based on count
#' @param outLine \code{boolean}, add outline on your shape
#' @param transform \code{funciton}, transform function apply on count (by cells), can be log
#' 
#' @examples 
#' \dontrun{
#' 
#' setSimulationPath("myStudy")
#' myData <- readAntares()
#' 
#' plotXY(myData, "NODU", "LOAD", precision = 50,
#'          sizeOnCount = FALSE)
#'          
#' myData <- readAntares(areas = "all", links = "all")    
#' myData <- mergeAllAntaresData(myData)  
#' plotXY(myData, "OP. COST_max_b", "OP. COST_max_c", precision = 50,
#'          sizeOnCount = FALSE)
#'          
#'          
#' }
#'    
#' @export
plotXY <- function(data, x, y, precision = 30, sizeOnCount = FALSE, outLine = TRUE,
                     transform = NULL)
{
  if(!requireNamespace("rbokeh")){
    stop("You should install 'rbokeh' library")
  }
  if(!"data.frame"%in%class(data)){
    stop("data should be a data.frame")
  }

  suppressWarnings(p <- rbokeh::figure() %>%
                     rbokeh::ly_hexbin(x, y, data, xbins = precision, 
                               style = ifelse(sizeOnCount,"lattice", "colorramp"),
                               palette = c("Spectral10"), line = !outLine, trans = transform))

  p
}
