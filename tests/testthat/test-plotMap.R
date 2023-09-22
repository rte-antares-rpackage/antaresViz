# context("plotMap")
# 
# test_that("plotMap, no interactive", {
#   
#   dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)
#   testClass <- function(obj){
#     class(obj)[1] == 'combineWidgets'
#   }
#   load(system.file("mapLayout/ml.rda", package = "antaresViz"))
#   
#   listArgs <- list(noarg = list(x = dta, interactive = FALSE, mapLayout = ml),
#                    colorLinks = list(x = dta, interactive = FALSE, mapLayout = ml, colLinkVar = "FLOW LIN."),
#                    colorAll = list(x = dta, interactive = FALSE, mapLayout = ml, colLinkVar = "FLOW LIN.",
#                                    colAreaVar = "OP. COST")
#   )
#   
#   lapply(listArgs, function(X){
#     re1 <- do.call(plotMap, X)
#     expect_true(testClass(re1))
#   })
#   
# })
# 
# test_that("plotMap, no interactive return error", {
#   
#   dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)
#   load(system.file("mapLayout/ml.rda", package = "antaresViz"))
#   
#   expect_error(plotMap(x = dta, mapLayout = ml , interactive = FALSE, compare = "areas"))
#   
#   
# })
# 
# test_that("plotMap, interactive", {
#   dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)
#   load(system.file("mapLayout/ml.rda", package = "antaresViz"))
#   VV <- plotMap(x = dta, mapLayout = ml, .runApp = FALSE, interactive = TRUE)
#   VV$init()
#   expect_true("MWController" %in% class(VV))
# })
# 
# test_that("plotMap, no interactive, x and refStudy are antaresDataList", {
#   dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)
#   load(system.file("mapLayout/ml.rda", package = "antaresViz"))
#   resPlotMap <- plotMap(x = dta, 
#                         mapLayout = ml, 
#                         interactive = FALSE,
#                         colAreaVar = "LOAD",
#                         sizeAreaVars = c("LOAD", "WIND", "SOLAR"))
#   expect_true("htmlwidget" %in% class(resPlotMap))
#   valToValid <- .getDataFromPlotMap(area = "c", 
#                       time = "sam. 05 mai 2018<br/>17:00",
#                       variable = "LOAD",
#                       htmlPlotMap = resPlotMap)
#   expect_gt(valToValid, 50000)
#   # with refStudy
#   resPlotMap <- plotMap(x = dta, 
#                         refStudy = dta,
#                         mapLayout = ml, 
#                         interactive = FALSE,
#                         colAreaVar = "LOAD",
#                         sizeAreaVars = c("LOAD", "WIND", "SOLAR"))
#   expect_true("htmlwidget" %in% class(resPlotMap))
#   valToValid <- .getDataFromPlotMap(area = "c", 
#                                     time = "sam. 05 mai 2018<br/>17:00",
#                                     variable = "LOAD",
#                                     htmlPlotMap = resPlotMap)
#   expect_equal(valToValid, 0)
#   # edit myData
#   data2 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
#   data2$areas[ , LOAD := as.double(LOAD)][area=="c", LOAD := as.double(LOAD +2500.0)]
#   resPlotMap2 <- plotMap(x = data2, 
#                         refStudy = dta,
#                         mapLayout = ml, 
#                         interactive = FALSE,
#                         colAreaVar = "LOAD",
#                         sizeAreaVars = c("LOAD", "WIND", "SOLAR"))
#   expect_true("htmlwidget" %in% class(resPlotMap2))
#   valToValid <- .getDataFromPlotMap(area = "c", 
#                                     time = "sam. 05 mai 2018<br/>17:00",
#                                     variable = "LOAD",
#                                     htmlPlotMap = resPlotMap2)
#   expect_equal(valToValid, 2500)
# })
# 
# test_that("plotMap, no interactive, x is a list of antaresDataList and refStudy an antaresDataList", {
#   data1 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
#   dataList <- list(data1, data1, data1)
#   load(system.file("mapLayout/ml.rda", package = "antaresViz"))
#   resPlotMap <- plotMap(x = dataList, 
#                         mapLayout = ml, 
#                         interactive = FALSE,
#                         colAreaVar = "LOAD",
#                         sizeAreaVars = c("LOAD", "WIND", "SOLAR"))
#   expect_true("htmlwidget" %in% class(resPlotMap))
#   valToValid <- .getDataFromPlotMap(area = "c", 
#                                     time = "sam. 05 mai 2018<br/>17:00",
#                                     variable = "LOAD",
#                                     htmlPlotMap = resPlotMap,
#                                     idWidget = 2)
#   expect_gt(valToValid, 50000)
#   # with refStudy
#   resPlotMap <- plotMap(x = dataList, 
#                         refStudy = data1,
#                         mapLayout = ml, 
#                         interactive = FALSE,
#                         colAreaVar = "LOAD",
#                         sizeAreaVars = c("LOAD", "WIND", "SOLAR"))
#   expect_true("htmlwidget" %in% class(resPlotMap))
#   valToValid <- .getDataFromPlotMap(area = "c", 
#                                     time = "sam. 05 mai 2018<br/>17:00",
#                                     variable = "LOAD",
#                                     htmlPlotMap = resPlotMap,
#                                     idWidget = 2)
#   expect_equal(valToValid, 0)
#   # edit myData
#   data2 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
#   data1 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
#   data2$areas[ , LOAD := as.double(LOAD)][area=="c", LOAD := as.double(LOAD +2500.0)]
#   dataList2 <- list(data1, data2, data1)
#   expect_equal(dataList2[[2]]$areas[area=="c", LOAD], dataList2[[1]]$areas[area=="c", LOAD] + 2500)
#   resPlotMap2 <- plotMap(x = dataList2, 
#                          refStudy = data1,
#                          mapLayout = ml, 
#                          interactive = FALSE,
#                          colAreaVar = "LOAD",
#                          sizeAreaVars = c("LOAD", "WIND", "SOLAR"))
#   expect_true("htmlwidget" %in% class(resPlotMap2))
#   valToValid <- .getDataFromPlotMap(area = "c", 
#                                     time = "sam. 05 mai 2018<br/>17:00",
#                                     variable = "LOAD",
#                                     htmlPlotMap = resPlotMap2,
#                                     idWidget = 2)
#   expect_equal(valToValid, 2500)
# })

# test_that("plotMap, interactive, x and refStudy are antaresDataList", {
#   dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)
#   load(system.file("mapLayout/ml.rda", package = "antaresViz"))
#   #interactive
#   resPlotMap <- plotMap(x = dta, 
#                         mapLayout = ml, 
#                         interactive = TRUE,
#                         .runApp = FALSE,
#                         colAreaVar = "LOAD",
#                         sizeAreaVars = c("LOAD", "WIND", "SOLAR"))
#   resPlotMap$init()
#   expect_true("MWController" %in% class(resPlotMap))
#   valToValid <- .getDataFromPlotMap(area = "c", 
#                                     time = "sam. 05 mai 2018<br/>17:00",
#                                     variable = "LOAD",
#                                     htmlPlotMap = resPlotMap)
#   expect_gt(valToValid, 50000)
#   # interactive, with refStudy
#   resPlotMap <- plotMap(x = dta, 
#                         refStudy = dta,
#                         mapLayout = ml, 
#                         interactive = TRUE,
#                         .runApp = FALSE,
#                         colAreaVar = "LOAD",
#                         areaChartType = "bar",
#                         sizeAreaVars = c("LOAD", "WIND", "SOLAR"))
#   resPlotMap$init()
#   expect_true("MWController" %in% class(resPlotMap))
#   valToValid <- .getDataFromPlotMap(area = "c", 
#                                     time = "sam. 05 mai 2018<br/>17:00",
#                                     variable = "LOAD",
#                                     htmlPlotMap = resPlotMap)
#   expect_equal(valToValid, 0)
#   # edit myData
#   data2 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
#   data2$areas[ , LOAD := as.double(LOAD)][area=="c", LOAD := as.double(LOAD +2500.0)]
#   resPlotMap2 <- plotMap(x = data2, 
#                          refStudy = dta,
#                          mapLayout = ml, 
#                          interactive = TRUE,
#                          .runApp = FALSE,
#                          colAreaVar = "LOAD",
#                          sizeAreaVars = c("LOAD", "WIND", "SOLAR"))
#   resPlotMap2$init()
#   expect_true("MWController" %in% class(resPlotMap2))
#   valToValid <- .getDataFromPlotMap(area = "c", 
#                                     time = "sam. 05 mai 2018<br/>17:00",
#                                     variable = "LOAD",
#                                     htmlPlotMap = resPlotMap2)
#   expect_equal(valToValid, 2500)
# })
# 
# test_that("plotMap, interactive, x is a list of antaresDataList and refStudy an antaresDataList", {
#   data1 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
#   dataList <- list(data1, data1, data1)
#   load(system.file("mapLayout/ml.rda", package = "antaresViz"))
#   #interactive
#   resPlotMap <- plotMap(x = dataList, 
#                         mapLayout = ml, 
#                         interactive = TRUE,
#                         .runApp = FALSE,
#                         colAreaVar = "LOAD",
#                         sizeAreaVars = c("LOAD", "WIND", "SOLAR"))
#   resPlotMap$init()
#   expect_true("MWController" %in% class(resPlotMap))
#   valToValid <- .getDataFromPlotMap(area = "c", 
#                                     time = "sam. 05 mai 2018<br/>17:00",
#                                     variable = "LOAD",
#                                     htmlPlotMap = resPlotMap,
#                                     idWidget = 2)
#   expect_gt(valToValid, 50000)
#   # interactive, with refStudy
#   resPlotMap <- plotMap(x = dataList, 
#                         refStudy = data1,
#                         mapLayout = ml, 
#                         interactive = TRUE,
#                         .runApp = FALSE,
#                         colAreaVar = "LOAD",
#                         sizeAreaVars = c("LOAD", "WIND", "SOLAR"))
#   resPlotMap$init()
#   expect_true("MWController" %in% class(resPlotMap))
#   valToValid <- .getDataFromPlotMap(area = "c", 
#                                     time = "sam. 05 mai 2018<br/>17:00",
#                                     variable = "LOAD",
#                                     htmlPlotMap = resPlotMap,
#                                     idWidget = 2)
#   expect_equal(valToValid, 0)
#   # edit myData
#   data2 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
#   data2$areas[ , LOAD := as.double(LOAD)][area=="c", LOAD := as.double(LOAD +2500.0)]
#   dataList <- list(data1, data2, data1)
#   resPlotMap2 <- plotMap(x = dataList, 
#                          refStudy = data1,
#                          mapLayout = ml, 
#                          interactive = TRUE,
#                          .runApp = FALSE,
#                          colAreaVar = "LOAD",
#                          sizeAreaVars = c("LOAD", "WIND", "SOLAR"))
#   resPlotMap2$init()
#   expect_true("MWController" %in% class(resPlotMap2))
#   valToValid <- .getDataFromPlotMap(area = "c", 
#                                     time = "sam. 05 mai 2018<br/>17:00",
#                                     variable = "LOAD",
#                                     htmlPlotMap = resPlotMap2,
#                                     idWidget = 2)
#   expect_equal(valToValid, 2500)
# })
# 
# test_that("plotMap, interactive, x and refStudy are optsH5", {
#   if (.requireRhdf5_Antares(stopP = FALSE)){
#     skip_if_not(.runPlotMapTest)
#     suppressMessages(writeAntaresH5(pathtemp, opts = opts, overwrite = TRUE))
#     optsH5 <- setSimulationPath(pathtemp)
#     listOptsH5 <- list(optsH5, optsH5, optsH5)
#     load(system.file("mapLayout/ml.rda", package = "antaresViz"))
#     #interactive
#     resPlotMap <- plotMap(x = listOptsH5, 
#                           mapLayout = ml, 
#                           interactive = TRUE,
#                           .runApp = FALSE,
#                           colAreaVar = "LOAD",
#                           sizeAreaVars = c("LOAD", "WIND", "SOLAR"))
#     resPlotMap$init()
#     expect_true("MWController" %in% class(resPlotMap))
#     #Problem TimeZone, to get 17pm in the plot we must put 19pm for h5request
#     valToValid <- .getDataFromPlotMap(area = "c", 
#                                       time = "sam. 05 mai 2018<br/>17:00",
#                                       variable = "LOAD",
#                                       htmlPlotMap = resPlotMap,
#                                       idWidget = 2)
#     expect_gt(valToValid, 50000)
#     # interactive, with refStudy
#     resPlotMap <- plotMap(x = listOptsH5, 
#                           refStudy = optsH5,
#                           mapLayout = ml, 
#                           interactive = TRUE,
#                           .runApp = FALSE,
#                           colAreaVar = "LOAD",
#                           sizeAreaVars = c("LOAD", "WIND", "SOLAR"))
#     resPlotMap$init()
#     expect_true("MWController" %in% class(resPlotMap))
#     valToValid <- .getDataFromPlotMap(area = "c", 
#                                       time = "sam. 05 mai 2018<br/>17:00",
#                                       variable = "LOAD",
#                                       htmlPlotMap = resPlotMap,
#                                       idWidget = 2)
#     expect_equal(valToValid, 0)
#     # with a new Study H5 test if compare plotMap works
#     ## create a new folder h5
#     pathInitial <- file.path(dirname(pathtemp), basename(pathtemp))
#     pathNewH5 <- file.path(pathInitial, "testH5")
#     if (!dir.exists(pathNewH5)){
#       dir.create(pathNewH5)
#     }
#     #write the study
#     #windows pb ? pathNewH5 <- gsub("/", "\\", pathNewH5, fixed = TRUE)
#     optsData <- antaresRead::setSimulationPath(path = studyPath)
#     suppressWarnings(writeAntaresH5(path = pathNewH5, opts = optsData, 
#                                     overwrite = TRUE, supressMessages = TRUE))
#     pathNewH5File <- file.path(pathNewH5, list.files(pathNewH5))
#     myArea <- "c"
#     newValue <- 15000
#     .h5Antares_edit_variable(
#       pathH5 = pathNewH5File, 
#       area = myArea, 
#       timeId = 1:40, 
#       antVar = "LOAD",
#       newValue = newValue
#     )
#     optsH5New <- setSimulationPath(path = pathNewH5File)
#     myDataValid <- readAntares(areas = "c", select = "LOAD", opts = optsH5New)
#     expect_equal(myDataValid[timeId == 2691, LOAD], newValue)
#     expect_equal(myDataValid[time == "2018-04-23 02:00:00 UTC", LOAD], newValue)
#     resPlotMapOpts <- plotMap(x = listOptsH5, 
#                           refStudy = optsH5New,
#                           mapLayout = ml, 
#                           interactive = TRUE,
#                           .runApp = FALSE,
#                           colAreaVar = "LOAD",
#                           sizeAreaVars = c("LOAD", "WIND", "SOLAR"),
#                           h5requestFiltering = list(
#                             mcYears = NULL))
#     resPlotMapOpts$init()
#     expect_true("MWController" %in% class(resPlotMapOpts))
#     valToValid <- .getDataFromPlotMap(area = "c", 
#                                       time = "lun. 23 avr. 2018<br/>02:00",
#                                       variable = "LOAD",
#                                       htmlPlotMap = resPlotMapOpts,
#                                       idWidget = 2)
#     
#     oldValue <- readAntares(areas = "c", select = "LOAD", opts = optsH5)
#     oldValue <- oldValue[timeId == 2691, LOAD]
#     expect_equal(valToValid, oldValue - newValue)
#     
#   }
# })
