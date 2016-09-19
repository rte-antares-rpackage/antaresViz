#' Display results on a map
#' 
#' @param x
#'   Object of class \code{antaresDataList} created with 
#'   \code{\link[antaresRead]{readAntares}}
#' @param mapLayout
#'   Object created with function \code{\link{mapLayout}}
#' @param areaVar
#'   area variable to display on the map
#' @param linkVar
#'   link variable to display on the map
#' @param timeId
#'   timeId
#' @param interactive
#'   Should the function start a shiny gadget that lets the user modify the
#'   parameters of the plot ?
#' 
#' @export
plotMap <- function(x, mapLayout, areaVar = "none", linkVar = "none", 
                    timeId = min(x$areas$timeId),
                    interactive = base::interactive(), mp = NULL) {
  
  # Keep only links and areas present in the data
  areaList <- unique(x$areas$area)
  linkList <- unique(x$links$link)
  mapLayout$coords <- mapLayout$coords[area %in% areaList]
  mapLayout$links <- mapLayout$links[link %in% linkList]
  
  # Function that draws the final map when leaving the shiny gadget.
  plotFun <- function(t, areaVar, linkVar) {
    
    ml <- copy(mapLayout)
    
    if (areaVar != "none") {
      colAndPal <- .getColAndPal(x$areas, mapLayout$coords, areaVar, t, "area")
      ml$coords <- colAndPal$coords
      colAreas <- colAndPal$col
      areaPal <- colAndPal$pal
    } else {
      colAreas <- "#CCCCCC"
    }
    
    if (linkVar != "none") {
      colAndPal <- .getColAndPal(x$links, ml$links, linkVar, t, "link")
      ml$links <- colAndPal$coords
      colLinks <- colAndPal$col
      linkPal <- colAndPal$pal
      dir <- colAndPal$dir
    } else {
      colLinks <- "#CCCCCC"
      dir <- 0
    }
    
    map <- plot(ml, colAreas, colLinks, dir = dir)
    
    if (areaVar != "none") 
      map <- addLegend(map, "topright", areaPal, ml$coords[[areaVar]], title = areaVar,
                       opacity = 1)
    if (linkVar != "none")
      map <- addLegend(map, "topright", linkPal, ml$links[[linkVar]], title = linkVar,
                       opacity = 1)
    
    map
  }
  
  if (!interactive) {
    return(plotFun(timeId, areaVar, linkVar))
  }
  
  areaValColums <- setdiff(names(x$areas), .idCols(x$areas))
  linkValColums <- setdiff(names(x$links), .idCols(x$links))
  
  ui <- mwUI(
    timeId = mwSlider(min(x$areas$timeId), max(x$areas$timeId), timeId, step = 1),
    Areas = list(
      areaVar = mwSelect(c("none", areaValColums), areaVar, label = "Color"),
      areaRadius = mwSelect(areaValColums, areaVar, label = "Radius", multiple = TRUE)
    ),
    Links = list(
      linkVar = mwSelect(c("none", setdiff(names(x$links), .idCols(x$links))), linkVar, label = "Color"),
      width = mwSelect(c("none", setdiff(names(x$links), .idCols(x$links))), linkVar, label = "Width")
    ),
    .content = leafletOutput("map", height = "100%")
  )
  
  args <- runGadget(ui, .plotMapServer(x, mapLayout), viewer = browserViewer())
  do.call(plotFun, args)
}

