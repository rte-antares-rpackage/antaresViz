#' Display results on a map
#' 
#' @param x
#'   Object of class \code{antaresDataList} created with 
#'   \code{\link[antaresRead]{readAntares}}
#' @param mapLayout
#'   Object created with function \code{\link{mapLayout}}
#' @param colAreaVar
#'   area variable to display on the map
#' @param colLinkVar
#'   link variable to display on the map
#' @param timeId
#'   timeId
#' @param interactive
#'   Should the function start a shiny gadget that lets the user modify the
#'   parameters of the plot ?
#' 
#' @export
plotMap <- function(x, mapLayout, colAreaVar = "none", sizeAreaVars = c(),
                    colLinkVar = "none", sizeLinkVar = "none", 
                    timeId = min(x$areas$timeId),
                    interactive = base::interactive(), mp = NULL) {
  
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
    optsLink <- .getColAndSize(x$links, mapLayout$links, "link", t,
                               colLinkVar, sizeLinkVar)
    
    ml$coords <- optsArea$coords
    ml$links <- optsLink$coords
    
    map <- plot(ml, optsArea$color, optsArea$size * 15, 
                optsLink$color, optsLink$size * 10, dir = optsLink$dir)
    
    if (colAreaVar != "none") 
      map <- addLegend(map, "topright", optsArea$pal, optsArea$coords[[colAreaVar]], title = colAreaVar,
                       opacity = 1)
    if (colLinkVar != "none")
      map <- addLegend(map, "topright", optsLink$pal, optsLink$coords[[colLinkVar]], title = colLinkVar,
                       opacity = 1)
    
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
      sizeAreaVars = mwSelect(areaValColums, colAreaVar, label = "Radius", multiple = TRUE)
    ),
    Links = list(
      colLinkVar = mwSelect(c("none", setdiff(names(x$links), .idCols(x$links))), colLinkVar, label = "Color"),
      sizeLinkVar = mwSelect(c("none", setdiff(names(x$links), .idCols(x$links))), colLinkVar, label = "Width")
    ),
    .content = leafletOutput("map", height = "100%")
  )
  
  args <- runGadget(ui, .plotMapServer(x, mapLayout, initialMap), viewer = browserViewer())
  do.call(plotFun, args)
}

