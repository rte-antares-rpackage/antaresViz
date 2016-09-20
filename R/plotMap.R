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
#' @param interactive
#'   Should the function start a shiny gadget that lets the user modify the
#'   parameters of the plot ?
#' @param options
#'   List of parameters that override some default visual settings. See the
#'   help of \code{\link{plotMapOptions}}.
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
                    colLinkVar = "none", sizeLinkVar = "none", 
                    timeId = min(x$areas$timeId),
                    interactive = base::interactive(),
                    options = plotMapOptions()) {
  
  options <- do.call(plotMapOptions, options)
  
  # Keep only links and areas present in the data
  areaList <- unique(x$areas$area)
  linkList <- unique(x$links$link)
  mapLayout$coords <- mapLayout$coords[area %in% areaList]
  mapLayout$links <- mapLayout$links[link %in% linkList]
  
  # Function that draws the final map when leaving the shiny gadget.
  plotFun <- function(t, colAreaVar, sizeAreaVars, colLinkVar, sizeLinkVar) {
    
    ml <- copy(mapLayout)
    
    optsArea <- .getColAndSize(x$areas, mapLayout$coords, "area", t,
                               colAreaVar, sizeAreaVars)
    
    if (is.null(optsArea$color)) optsArea$color <- options$colArea
    
    if (is.null(optsArea$size)) optsArea$size <- options$sizeArea
    else if (ncol(optsArea$size) == 1) {
      optsArea$size <- sqrt(optsArea$size) * options$maxSizeArea
    }
    
    optsLink <- .getColAndSize(x$links, mapLayout$links, "link", t,
                               colLinkVar, sizeLinkVar)
    
    if (is.null(optsLink$color)) optsLink$color <- options$colLink
    if (is.null(optsLink$size)) optsLink$size <- options$sizeLink
    else optsLink$size <- optsLink$size * options$maxSizeLink
    
    ml$coords <- optsArea$coords
    ml$links <- optsLink$coords
    
    map <- plot(ml, optsArea$color, optsArea$size, 
                optsLink$color, optsLink$size, dir = optsLink$dir)
    
    # Add legends
    if (!is.null(optsArea$pal)) 
      map <- addLegend(map, "topright", optsArea$pal, optsArea$coords[[colAreaVar]], title = colAreaVar,
                       opacity = 1)
    if (!is.null(optsLink$pal))
      map <- addLegend(map, "topright", optsLink$pal, optsLink$coords[[colLinkVar]], title = colLinkVar,
                       opacity = 1)
    
    # Add an invisible layer containing either circleMarkers or polarCharts
    if (length(sizeAreaVars) <= 1) {
      map <- addPolarChart(map, ml$coords$x, ml$coords$y, 
                           data = matrix(1, nrow = nrow(ml$coords)),
                           opacity = 0, layerId = ml$coords$area)
    } else {
      map <- addCircleMarkers(map, ml$coords$x, ml$coords$y, opacity = 0, 
                              fillOpacity = 0,
                              layerId = ml$coords$area)
    }
    
    map
  }
  
  initialMap <- plotFun(timeId, colAreaVar, sizeAreaVars, colLinkVar, sizeLinkVar)
  
  if (!interactive) {
    return(initialMap)
  }
  
  areaValColums <- setdiff(names(x$areas), .idCols(x$areas))
  linkValColums <- setdiff(names(x$links), .idCols(x$links))
  
  ui <- mwUI(
    timeId = mwSlider(min(x$areas$timeId), max(x$areas$timeId), timeId, step = 1),
    Areas = list(
      colAreaVar = mwSelect(c("none", areaValColums), colAreaVar, label = "Color"),
      sizeAreaVars = mwSelect(areaValColums, sizeAreaVars, label = "Radius", multiple = TRUE)
    ),
    Links = list(
      colLinkVar = mwSelect(c("none", setdiff(names(x$links), .idCols(x$links))), colLinkVar, label = "Color"),
      sizeLinkVar = mwSelect(c("none", setdiff(names(x$links), .idCols(x$links))), sizeLinkVar, label = "Width")
    ),
    .content = leafletOutput("map", height = "100%")
  )
  
  args <- runGadget(ui, 
                    .plotMapServer(x, mapLayout, initialMap, options), 
                    viewer = browserViewer())
  do.call(plotFun, args)
}

