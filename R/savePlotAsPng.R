#' Save interactive plot as a png image
#' 
#' This function saves an interactive plot generated with one of the functions of
#' this package as a png image. The result can then be included in documents or
#' presentations.
#' 
#' @param plot
#'   A plot generated with one of the functions of this package.
#' @param file
#'   The name of the output file
#' @param width
#'   Width of the output file
#' @param height
#'   height of the output file
#' @param ...
#'   Other parameters passed to function \code{\link[webshot]{webshot}}
#' 
#' @return 
#' The function only creates the required file. Nothing is returned
#' 
#' @examples
#' \dontrun{
#'   mydata <- readAntares()
#'   myplot <- plot(mydata, variable = "MRG. PRICE", type = "density")
#'   savePlotAsPng(myplot, file = "myplot.png")
#' }
#' 
#' @export
#' 
savePlotAsPng <- function(plot, file = "Rplot.png", width = 600, height = 480, ...) {
  tmpfile <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(plot, file = tmpfile, selfcontained = FALSE)
  webshot::webshot(tmpfile, file = file, vwidth = width, vheight = height, ...)
}
