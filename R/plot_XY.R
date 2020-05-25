# Copyright © 2016 RTE Réseau de transport d’électricité

#' Plot density between X et Y with ggplot2 and plotly
#' 
#' 
#' @param data \code{data.frame} can be antaresData object
#' @param x \code{character}, x variable 
#' @param y \code{character}, y variable 
#' @param precision Deprecated.
#' @param sizeOnCount Deprecated.
#' @param outLine Deprecated.
#' @param transform Deprecated.
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
plotXY <- function(data, x, y, 
                   precision = 30,
                   sizeOnCount = FALSE,
                   outLine = TRUE,
                   transform = NULL) {
  if(!requireNamespace("ggplot2")) {
    stop("You should install 'ggplot2' library")
  }
  if(!"data.frame" %in% class(data)) {
    stop("data should be a data.frame")
  }
  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!ggplot2::sym(x), y = !!ggplot2::sym(y))) + 
    ggplot2::geom_hex() + 
    ggplot2::scale_fill_distiller(palette = "Spectral") + 
    ggplot2::theme_minimal() + 
    ggplot2::labs(fill = "Density")
  plotly::ggplotly(p)
}
