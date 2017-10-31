#' Use to change limit size of graph
#' 
#' @param size \code{numeric} size autorize of graphs in modules (default 1)
#' @examples 
#' \dontrun{
#' limitSizeGraph(10)
#' }
#' 
#' @export
limitSizeGraph <- function(size){
  options(sizeGraph = size)
}