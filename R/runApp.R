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
  ctrl <- shiny::runApp(system.file("application", package = "antaresViz") , launch.browser = TRUE)
  gc(reset = TRUE)
  invisible(TRUE)
}