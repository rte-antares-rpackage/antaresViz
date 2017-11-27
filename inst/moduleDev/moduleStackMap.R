
setSimulationPath(path = "D:/test_case/", 1)
mydata <- readAntares(areas = "all", links = "all", timeStep = "hourly")
ml <- mapLayout(readLayout())

library(antaresViz)
library(manipulateWidget)

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
  print(ColorArea2)
  combineWidgets(nrow = 1, ncol = 2,
                 exchangesStack(mydata, area = area, interactive = FALSE, dateRange = dateRange),
                 plotMap(mydata, ml,  colLinkVar = colorLink2,
                         colAreaVar = ColorArea2, interactive = FALSE, dateRange = dateRange, sizeAreaVars = sizeArea2))
  
},
area = mwSelect(label = "area", levels(mydata$areas$area)),
dateRange = mwDateRange(value = range(mydata$areas$time), 
                        min = min(mydata$areas$time), max = max(mydata$areas$time)),
Area = mwGroup(
  colorArea = mwSelect(choices = {
    names(mydata$areas)[!names(mydata$areas) %in% getIdCols(mydata$areas)]
  },value = "LOAD", label = "Color"),
  
  
  sizeArea = mwSelect(choices = {
    names(mydata$areas)[!names(mydata$areas) %in% getIdCols(mydata$areas)]
  }, label = "Size", multiple = TRUE),
  
  .display = TRUE),

Link = mwGroup(
  colorLink = mwSelect(choices = {
    names(mydata$links)[!names(mydata$links) %in% getIdCols(mydata$links)]
  },value = "FLOW LIN.", label = "Color"),
  
  .display = TRUE)
)

