#' @noRd
leafletDragPoints <- function(geopoints, map = NULL, width = NULL, height = NULL) {
  if (!is.null(map)) map <- geojsonio::geojson_json(map)

  geopoints$avg <- (geopoints$lat + geopoints$lon) / 2

  firstPoint <- which.min(geopoints$avg)
  secondPoint <- which.max(geopoints$avg)

  x = list(geopoints = geopoints, map = map)

  attr(x, 'TOJSON_ARGS') <- list(dataframe = "rows")
  # create widget
  htmlwidgets::createWidget(
    name = 'leafletDragPoints',
    x,
    width = width,
    height = height,
    package = 'antaresViz',
    sizingPolicy = htmlwidgets::sizingPolicy(
      browser.fill = TRUE
    )
  )
}

#' Shiny bindings for placeGeoPoints
#'
#' Output and render functions for using placeGeoPoints within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a placeGeoPoints
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name placeGeoPoints-shiny
#'
#' @noRd
leafletDragPointsOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'leafletDragPoints', width, height, package = 'antaresViz')
}

#' @rdname placeGeoPoints-shiny
#' @noRd
renderLeafletDragPoints <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, leafletDragPointsOutput, env, quoted = TRUE)
}
