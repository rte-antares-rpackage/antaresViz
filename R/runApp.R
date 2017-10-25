#' Run app antaresViz
#' 
#' \code{runAppAntaresViz} run antaresViz App.
#' 
#' @return 
#' an App Shiny. 
#' 
#' @importFrom shiny runApp
#' @export
runAppAntaresViz <- function() {
  shiny::runApp(system.file("application", package = "antaresViz"))
}