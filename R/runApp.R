#' Run app antaresViz
#' 
#' \code{runAppAntaresViz} run antaresViz App.
#' 
#' @return 
#' an App Shiny. 
#' 
#' @importFrom shiny runApp
#' @export
runAppAntaresViz <- function(browser = FALSE) {
  ctrl <- shiny::runApp(system.file("application", package = "antaresViz") , launch.browser = browser)
  gc(reset = TRUE)
  invisible(TRUE)
}