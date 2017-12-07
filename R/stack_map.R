
# Copyright © 2016 RTE Réseau de transport d’électricité

#' plot stack and map
#' 
#' 
#' @param x \code{antaresDataList} antaresDataList contian areas ans links.
#' @param mapLayout
#'   Object created with function \code{\link{mapLayout}}
#'
#' @examples 
#' \dontrun{
#' mydata <- readAntares(areas = "all", links = "all")
#' 
#' layout <- readLayout()
#' ml <- mapLayout(layout = layout)
#' 
#' stackMap(x = mydata, mapLayout = ml)
#' }
#' 
#' @export
stackMap <- function(x, mapLayout)
{
  colorArea <- colorLink <- sizeArea <- dateRange <- NULL
  manipulateWidget(.expr = {
    ColorArea2 <- colorArea
    if(is.null(ColorArea2)){
      ColorArea2 <- "none"
    }
    
    colorLink2 <- colorLink
    if(is.null(colorLink2)){
      colorLink2 <- "none"
    }
    
    
    sizeArea2 <- sizeArea
    if(is.null(sizeArea2)){
      sizeArea2 <- "none"
    }
    combineWidgets(nrow = 1, ncol = 2,
                   exchangesStack(x, area = area, interactive = FALSE, dateRange = dateRange),
                   plotMap(x, mapLayout = mapLayout,  colLinkVar = colorLink2,
                           colAreaVar = ColorArea2, interactive = FALSE,
                           dateRange = dateRange, sizeAreaVars = sizeArea2,
                           areaChartType = areaChartType, sizeMiniPlot = sizeMiniPlot))
    
  },
  area = mwSelect(label = "area", levels(x$areas$area)),
  dateRange = mwDateRange(value = {
    if(attr(x, "timeStep") != "hourly")
    {
      ra <- range(x$areas$time)
    }else{
      if(max(x$areas$time)-min(x$areas$time)>3){
        ra = c(max(x$areas$time)-3600*24*3, max(x$areas$time))
      }else{
        ra <- range(x$areas$time)
        
        }
    }
    
    ra
    }, 
                          min = min(x$areas$time), max = max(x$areas$time)),
  Area = mwGroup(
    colorArea = mwSelect(choices = {
      names(x$areas)[!names(x$areas) %in% getIdCols(x$areas)]
    },value = "LOAD", label = "Color"),
    
    
    sizeArea = mwSelect(choices = {
      names(x$areas)[!names(x$areas) %in% getIdCols(x$areas)]
    }, label = "Size", multiple = TRUE),
    
    
    miniPlot = mwGroup(
      areaChartType = mwSelect(list("bar chart" = "bar", 
                                    "pie chart" = "pie",
                                    "polar (area)" = "polar-area",
                                    "polar (radius)" = "polar-radius"),
                               value = {
                                 if(.initial) areaChartType
                                 else NULL
                               }),
      sizeMiniPlot = mwCheckbox(FALSE)
    ),
    
    .display = TRUE),
  
  Link = mwGroup(
    colorLink = mwSelect(choices = {
      names(x$links)[!names(x$links) %in% getIdCols(x$links)]
    },value = "FLOW LIN.", label = "Color"),
    
    .display = TRUE)
  )
}
