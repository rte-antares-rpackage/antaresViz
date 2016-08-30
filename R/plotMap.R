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
                    interactive = base::interactive(),mp = NULL) {
  
  plotFun <- function(t, areaVar, linkVar) {
    
    ml <- copy(mapLayout)
    
    if (areaVar != "none") {
      ml$coords <- merge(
        mapLayout$coords, 
        x$areas[timeId == t, c("area", areaVar), with = FALSE], 
        by = "area"
      )
      
      setnames(ml$coords, areaVar, "var")
      
      rangevar <- range(x$areas[[areaVar]])
      if (rangevar[1] >= 0) {
        domain <- rangevar
        areaPal <- colorBin("Blues", domain, bins = 5)
      } else {
        domain <- c(-max(abs(rangevar)), max(abs(rangevar)))
        areaPal <- colorBin("RdBu", domain, bins = 7)
      }
      
      colAreas <- areaPal(ml$coords$var)
      
    } else {
      areaList <- unique(x$areas$area)
      ml$coords <- ml$coords[area %in% areaList]
      colAreas <- "#CCCCCC"
    }
    
    if (linkVar != "none") {
      
      ml$links <- merge(
        mapLayout$links, 
        x$links[timeId == t, c("link", linkVar), with = FALSE], 
        by = "link"
      )
      
      setnames(ml$links, linkVar, "var")
      
      rangevar <- range(x$links[[linkVar]])
      if (rangevar[1] >= 0) {
        domain <- rangevar
        linkPal <- colorBin("Blues", domain, bins = 5)
      } else {
        domain <- c(-max(abs(rangevar)), max(abs(rangevar)))
        linkPal <- colorBin("RdBu", domain, bins = 7)
      }
      
      colLinks <- linkPal(ml$links$var)
    } else {
      linkList <- unique(x$links$link)
      ml$links <- ml$links[link %in% linkList]
      colLinks <- "#CCCCDD"
    }
    
    map <- plot(ml, colAreas, colLinks)
    if (areaVar != "none") 
      map <- addLegend(map, "topright", areaPal, ml$coords$var, title = areaVar,
                       opacity = 1)
    if (linkVar != "none")
      map <- addLegend(map, "topright", linkPal, ml$links$var, title = linkVar,
                       opacity = 1)
    
    map
  }
  
  if (!interactive) {
    return(plotFun(timeId, areaVar, linkVar))
  }
  
  manipulateWidget(
    plotFun(timeId, areaVar, linkVar),
    timeId = mwSlider(min(x$areas$timeId), max(x$areas$timeId), timeId),
    areaVar = mwSelect(c("none", setdiff(names(x$areas), .idCols(x$areas))), areaVar),
    linkVar = mwSelect(c("none", setdiff(names(x$links), .idCols(x$links))), linkVar)
  )
  
}