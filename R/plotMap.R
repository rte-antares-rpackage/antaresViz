#Copyright © 2016 RTE Réseau de transport d’électricité

#' Display results of a simulation on a map
#' 
#' This function generates an interactive map that let the user visually explore
#' the results of an Antares simulation. By default the function starts a Shiny 
#' gadget that let the user which variables to represent.
#' 
#' @param x
#'   Object of class \code{antaresDataList} created with 
#'   \code{\link[antaresRead]{readAntares}} and containing areas and links data
#' @param mapLayout
#'   Object created with function \code{\link{mapLayout}}
#' @param colAreaVar
#'   Name of a variable present in \code{x$areas}. The values of this variable
#'   are represented by the color of the areas on the map. If \code{"none"}, then
#'   the default color is used for all areas. 
#' @param sizeAreaVars
#'   vector of variables present in \code{x$areas} to associate with the size of 
#'   areas on the map. If this parameter has length equal to 0, all areas have the
#'   same size. If it has length equal to one, then the radius of the areas change
#'   depending on the values of the variable choosen. If it has length greater than
#'   1 then areas are represented by a polar area chart where the size of each section
#'   depends on the values of each variable.
#' @param colLinkVar
#'   Name of a variable present in \code{x$links}. The values of this variable
#'   are represented by the color of the links on the map. If \code{"none"}, then
#'   the default color is used for all links  
#' @param sizeLinkVar
#'   Name of a variable present in \code{x$links}. Its values are represented by
#'   the line width of the links on the map.
#' @param timeId
#'   A single time id present in the data.
#' @param main
#'   Title of the map.
#' @param options
#'   List of parameters that override some default visual settings. See the
#'   help of \code{\link{plotMapOptions}}.
#' @inheritParams productionStack
#'   
#' @return 
#' An htmlwidget of class "leaflet". It can be modified with package 
#' \code{leaflet}. By default the function starts a shiny gadget that lets the
#' user play with most of the parameters of the function. The function returns
#' a leaflet map when the user clicks on the button \code{"done"}.
#' 
#' @examples 
#' \dontrun{
#' mydata <- readAntares(areas = "all", links = "all", timeStep = "daily",
#'                       select = "nostat")
#' 
#' # Place areas on a map. Ths has to be done once for a given study. Then the
#' # object returned by "mapLayout" may be saved and reloaded with
#' # functions save and load
#' 
#' layout <- readLayout()
#' ml <- mapLayout(layout)
#' save("ml", file = "ml.rda")
#' 
#' plotMap(mydata, ml)
#' 
#' # Specify the variables to use to control the color or size of elements.
#' plotMap(mydata, ml, 
#'         sizeAreaVars = c("WIND", "SOLAR", "H. ROR"),
#'         sizeLinkVar = "FLOW LIN.")
#' 
#' # Change default graphical properties
#' plotMap(mydata, ml, options = list(colArea="red", colLink = "orange"))
#' 
#' }
#' 
#' @export
plotMap <- function(x, mapLayout, colAreaVar = "none", sizeAreaVars = c(),
                    areaChartType = c("polar", "bar"),
                    colLinkVar = "none", sizeLinkVar = "none", 
                    timeId = NULL,
                    main = "",
                    interactive = base::interactive(),
                    options = plotMapOptions(),
                    width = NULL, height = NULL) {
  
  areaChartType <- match.arg(areaChartType)
  options <- do.call(plotMapOptions, options)
  
  # Check that parameters have the good class
  if (!is(mapLayout, "mapLayout")) stop("Argument 'mapLayout' must be an object of class 'mapLayout' created with function 'mapLayout'.")
  if (!is(x, "antaresData")) {
    stop("Argument 'x' must be an object of class 'antaresData' created with function 'readAntares'.")
  } else {
    x <- as.antaresDataList(x)
    if (is.null(x$areas) && is.null(x$links)) stop("Argument 'x' should contain at least area or link data.")
  }
  
  # Should links and/or areas be displayed ?
  areas <- !is.null(x$areas)
  links <- !is.null(x$links)
  
  
  # First and last time ids in data
  if (areas) {
    timeIdMin <- min(x$areas$timeId)
    timeIdMax <- max(x$areas$timeId)
  } else {
    timeIdMin <- min(x$links$timeId)
    timeIdMax <- max(x$links$timeId)
  }
  # Select first timeId if necessary
  if (is.null(timeId)) timeId <- timeIdMin
  
  # Keep only links and areas present in the data
  if (areas) {
    areaList <- unique(x$areas$area)
    mapLayout$coords <- mapLayout$coords[area %in% areaList]
  }
  if (links) {
    linkList <- unique(x$links$link)
    mapLayout$links <- mapLayout$links[link %in% linkList]
  }
  
  # Function that draws the final map when leaving the shiny gadget.
  plotFun <- function(t, colAreaVar, sizeAreaVars, colLinkVar, sizeLinkVar) {
    map <- .initMap(x, mapLayout, areaChartType, options) %>%
      .redrawLinks(x, mapLayout, t, colLinkVar, sizeLinkVar, options) %>% 
      .redrawCircles(x, mapLayout, t, colAreaVar, sizeAreaVars, areaChartType, options) %>% 
      addTimeLabel(t, attr(x, "timeStep"), simOptions(x))
    
    map
  }
  
  initialMap <- plotFun(timeId, colAreaVar, sizeAreaVars, colLinkVar, sizeLinkVar)
  
  if (!interactive) {
    return(initialMap %>% addTitle(main))
  }
  
  # Create the UI of the gadget
  areaValColums <- setdiff(names(x$areas), .idCols(x$areas))
  linkValColums <- setdiff(names(x$links), .idCols(x$links))
  
  ui <- mwUI(
    timeId = mwSlider(timeIdMin, timeIdMax, timeId, step = 1, animate = TRUE),
    Areas = list(
      colAreaVar = mwSelect(c("none", areaValColums), colAreaVar, label = "Color"),
      sizeAreaVars = mwSelect(areaValColums, sizeAreaVars, label = "Radius", multiple = TRUE)
    ),
    Links = list(
      colLinkVar = mwSelect(c("none", setdiff(names(x$links), .idCols(x$links))), colLinkVar, label = "Color"),
      sizeLinkVar = mwSelect(c("none", setdiff(names(x$links), .idCols(x$links))), sizeLinkVar, label = "Width")
    ),
    .content = leafletOutput("map", height = "100%"), 
    .main = main
  )
  
  args <- runGadget(ui, 
                    .plotMapServer(x, mapLayout, initialMap, areaChartType, options), 
                    viewer = browserViewer(), )
  do.call(plotFun, args) %>% addTitle(main)
}

