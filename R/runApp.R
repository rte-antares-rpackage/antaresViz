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
  suppressWarnings(try(rm(list = c("directoryInput", "readDirectoryInput", 
                                   "updateDirectoryInput"), envir = .GlobalEnv), silent = TRUE))
  gc(reset = TRUE)
  invisible(TRUE)
}