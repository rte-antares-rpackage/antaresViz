
setSimulationPath()
mydata <- readAntares(areas = "all", links = "all", timeStep = "hourly")
ml <- mapLayout(readLayout())

library(antaresViz)
library(manipulateWidget)

stackMap(mydata, ml)

