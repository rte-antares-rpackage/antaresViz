require(antaresRead)
require(antaresViz)

# classic study
setSimulationPath("C:\\Users\\Datastorm\\Desktop\\test_case_init", 1)
mydata <- readAntares(areas = "all", links = "all", timeStep = "daily",
                      select = "nostat", mcYears = "all")
# un h5
setSimulationPath("C:\\Users\\Datastorm\\Desktop\\antares\\20170315-1140eco-test.h5", 1)

opts_h5 <- simOptions()

#------------
# prodstak
#------------

c("mcYear", "main", "unit", "areas", "legend", "stack", "stepPlot", "drawPoints")

prodStack(x = mydata, 
          compare = c("mcYear", "main", "unit", "areas", "legend", "stack", "stepPlot", "drawPoints"))

prodStack(x = mydata, compare = c("mcYear"))

prodStack(x = list(mydata), compare = c("mcYear"))

prodStack(x = list(mydata, mydata), compare = c("stack"))

prodStack(x = list(mydata, mydata), compare = list(main = NULL, areas = NULL))

prodStack(x = opts_h5, compare = "main")

prodStack(x = list(opts_h5), compare = "main")

prodStack(x = list(opts_h5, opts_h5), compare = "main")

#------------
# exchangesStack
#------------

c("mcYear", "main", "unit", "area",
  "legend", "stepPlot", "drawPoints")

exchangesStack(x = mydata, 
               compare = c("mcYear", "main", "unit", "area",
                           "legend", "stepPlot", "drawPoints"))

exchangesStack(x = list(mydata, mydata), compare = "mcYear")  

exchangesStack(x = opts_h5, compare = "mcYear")

exchangesStack(x = list(opts_h5, opts_h5), compare = "mcYear")     

#------------
# tsPlot
#------------

c("mcYear", "main", "variable", "type", "confInt", "elements", "aggregate", 
  "legend", "highlight", "stepPlot", "drawPoints", "secondAxis")

tsPlot(x = mydata, 
       compare = c("mcYear", "main", "variable", "type", "confInt", "elements", "aggregate", 
                   "legend", "highlight", "stepPlot", "drawPoints", "secondAxis"))

tsPlot(x = list(mydata, mydata), 
       compare = c("mcYear"))

tsPlot(x = opts_h5, compare = "mcYear")

tsPlot(x = list(opts_h5, opts_h5), compare = "mcYear")

#------------
# plotMap
#------------

# layout <- readLayout()
# ml <- mapLayout(layout = layout)
load("ml.rda")

c("mcYear", "type", "colAreaVar", "sizeAreaVars", "areaChartType", "showLabels",
  "popupAreaVars", "labelAreaVar","colLinkVar", "sizeLinkVar", "popupLinkVars")

plotMap(x = mydata, mapLayout = ml, 
        compare =c("mcYear", "type", "colAreaVar", "sizeAreaVars", "areaChartType", "showLabels",
                   "popupAreaVars", "labelAreaVar","colLinkVar", "sizeLinkVar", "popupLinkVars"))


plotMap(x = list(mydata, mydata), mapLayout = ml, 
        compare =c("mcYear"))

plotMap(x = list(opts_h5, opts_h5, opts_h5), mapLayout = ml, 
        compare =c("mcYear", "type", "colAreaVar", "sizeAreaVars", "areaChartType", "showLabels",
        "popupAreaVars", "labelAreaVar","colLinkVar", "sizeLinkVar", "popupLinkVars"))
