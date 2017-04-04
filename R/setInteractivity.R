#' Get and set interactivity mode
#' 
#' \code{setInteractivity} globally sets the interactivity mode of plot 
#' functions. This is useful to avoid repeating \code{interactive = FALSE} or
#' \code{interactive = TRUE} in each function. \code{getInteractivity} gets the
#' interactivity mode.
#' 
#' @param interactive Should plot functions generate a UI that lets users to 
#' interactively modify input data and graphical parameters of a chart? It should
#' be TRUE or FALSE. The default behavior is to set it to TRUE if the R session
#' is interactive and to FALSE otherwise (for instance in Rmarkdown document).
#' 
#' @return 
#' \code{getInteractivity} returns a boolean indicating the interactivity mode of
#' plot functions. \code{setInteractivity} is only used for its side effects.
#' 
#' @export
setInteractivity <- function(interactive = "auto") {
  if (interactive == "auto") interactive <- interactive()
  options(antaresVizInteractive = interactive)
  invisible(TRUE)
}

#' @export
#' @rdname setInteractivity
getInteractivity <- function() {
  getOption("antaresVizInteractive")
}