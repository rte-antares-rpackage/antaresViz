#' Use to change limit size of graph (in Mb)
#' 
#' @param size \code{numeric} widget size autorized in modules (default 200)
#' @examples 
#' \dontrun{
#' limitSizeGraph(500)
#' }
#' 
#' @export
limitSizeGraph <- function(size){
  options(antaresVizSizeGraph = size)
}

controlWidgetSize <- function(widget){
  if(is.null(getOption("antaresVizSizeGraph"))){
    options(antaresVizSizeGraph = 200)
  }
  
  if(round(as.numeric(object.size(widget)) / 1048000, 1) > getOption("antaresVizSizeGraph")){
    return(combineWidgets(antaresVizSizeGraphError))
  } else {
    widget
  }
}
