context("tsPlot")

test_that("tsPlot, no interactive", {
  dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  testClass <- function(obj){
    class(obj)[1] == 'combineWidgets'
  }
  listArgs <- list(noarg = list(x = dta, interactive = FALSE),
                   elem = list(x = dta, interactive = FALSE, elements = "a"),
                   elemS = list(x = dta, interactive = FALSE, elements = c("a", "b")),
                   linkS = list(x = dta, table = "links", interactive = FALSE, elements = c("a - a_offshore")),
                   linkSVarSel = list(x = dta, table = "links", interactive = FALSE,
                                      elements = c("a - a_offshore"),
                                      variable = "FLOW LIN._std"),
                   bar = list(x = dta, interactive = FALSE, elements = "all", type = "barplot"),
                   monotone = list(x = dta, interactive = FALSE, elements = "all", type = "monotone"),
                   density = list(x = dta, interactive = FALSE, elements = "all", type = "density"),
                   cdf = list(x = dta, interactive = FALSE, elements = "all", type = "cdf")
  )

  lapply(listArgs, function(X){
    re1 <- do.call(tsPlot, X)
    expect_true(testClass(re1))
  })

})



test_that("tsPlot, no interactive return error", {

  dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)

  expect_error(tsPlot(dta, interactive = FALSE, compare = "areas"))

})

# test_that("tsPlot, work with compare", {
# 
#   dta <- readAntares(areas = "all", links = "all", showProgress = FALSE, mcYears = "all")
#   exList <-  tsPlot(x = dta, .runApp = FALSE, interactive = TRUE, compare = "mcYear")
#   exList <- exList$init()
#   #to get a param exList$getParams("tables")
#   # exList$getValue("mcYear")
#   exList$setValue("mcYear", 1, chartId = 1, reactive = FALSE)
#   exList$setValue("mcYear", 2, chartId = 2, reactive = FALSE)
#   exList$updateCharts()
#   expect_equal(exList$getValue("tables"), "areas")
#   expect_equal(exList$getValue("main"), "")
#   expect_true(is(exList, "MWController"))
#   expect_equal(exList$ncharts, 2)
#   expect_equal(exList$ncol, 1)
#   expect_equal(exList$nrow, 2)
#   dataTsCompare <- .get_data_from_htmlwidget(exList, widgetsNumber = 1)
#   timeEditValue <- "2018-05-06T18:00:00.000Z"
#   indexHour <- grep(timeEditValue, dataTsCompare$hour)
#   expect_gt(indexHour, 180)
#   expect_equal(dataTsCompare$a[indexHour], 1627275)
# 
#   dataTsCompareMcYear2 <- .get_data_from_htmlwidget(exList, widgetsNumber = 2)
#   expect_equal(dataTsCompareMcYear2$a[indexHour], 1432100)
# 
# })

test_that("tsPlot, no interactive, x and refStudy are antaresDataTable", {
  myData1 <- readAntares(links = "all", showProgress = FALSE)
  myData2 <- readAntares(links = "all", showProgress = FALSE)
  myLink <- "a - a_offshore"
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  tsDa1 <- tsPlot(x = myData1,
                  table = "links",
                  elements = myLink,
                  type = "ts",
                  interactive = FALSE)
  dataTsDa1 <- .get_data_from_htmlwidget(tsDa1)
  timeEditValue <- "2018-04-25T00:00:00.000Z"
  indexHour <- grep(timeEditValue, dataTsDa1$hour)
  expect_gt(indexHour, 2)
  expect_equal(dataTsDa1$`a - a_offshore`[indexHour], -9)
  tsDa2 <- tsPlot(x = myData1,
                  refStudy = myData2,
                  table = "links",
                  elements = myLink,
                  type = "ts",
                  interactive = FALSE)
  dataTsDa2 <- .get_data_from_htmlwidget(tsDa2)
  expect_equal(dataTsDa2$`a - a_offshore`[indexHour], 0)

  table <- "areas"
  myArea <- "b"
  myData1 <- readAntares(areas = myArea, showProgress = FALSE)
  myData2 <- readAntares(areas = myArea, showProgress = FALSE)
  tsDa1 <- tsPlot(x = myData1,
                  table = table,
                  elements = myArea,
                  type = "ts",
                  interactive = FALSE)
  dataTsDa1 <- .get_data_from_htmlwidget(tsDa1)
  expect_equal(dataTsDa1[[myArea]][[indexHour]], 2427150)
  tsDa2 <- tsPlot(x = myData1,
                  refStudy = myData2,
                  table = table,
                  elements = myArea,
                  type = "ts",
                  interactive = FALSE)
  dataTsDa2 <- .get_data_from_htmlwidget(tsDa2)
  expect_equal(dataTsDa2[[myArea]][[indexHour]], 0)
})

test_that("tsPlot, no interactive, x and refStudy are antaresDataList", {
  myArea <- "b"
  myLink <- "a - a_offshore"
  myData1 <- readAntares(links = myLink, areas = myArea, showProgress = FALSE, mcYears = 1)
  myData2 <- readAntares(links = myLink, areas = myArea, showProgress = FALSE, mcYears = 1)
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  tsDa1 <- tsPlot(x = myData1,
                  table = "links",
                  elements = myLink,
                  type = "ts",
                  interactive = FALSE)
  dataTsDa1 <- .get_data_from_htmlwidget(tsDa1)
  timeEditValue <- "2018-04-25T00:00:00.000Z"
  indexHour <- grep(timeEditValue, dataTsDa1$hour)
  expect_gt(indexHour, 2)
  expect_equal(dataTsDa1$`a - a_offshore`[indexHour], -10)
  tsDa2 <- tsPlot(x = myData1,
                  refStudy = myData2,
                  table = "links",
                  elements = myLink,
                  type = "ts",
                  interactive = FALSE)
  dataTsDa2 <- .get_data_from_htmlwidget(tsDa2)
  expect_equal(dataTsDa2$`a - a_offshore`[indexHour], 0)

  #pb timeZone local (PC, Travis, etc)
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData2$links[ (time == timeEditMinus | time == timeEditPlus) & link == "a - a_offshore", `FLOW LIN.` := as.integer(`FLOW LIN.` + 2500)]
  }
  tsDa2 <- tsPlot(x = myData1,
                  refStudy = myData2,
                  table = "links",
                  elements = myLink,
                  type = "ts",
                  interactive = FALSE)
  dataTsDa2 <- .get_data_from_htmlwidget(tsDa2)
  expect_equal(dataTsDa2$`a - a_offshore`[indexHour], -2500)

})

# describe("tsPlot, no interactive, x is a list of antaresDataList and refStudy an antaresDataList", {
#   myData1 <- readAntares(links = "all", areas = "all", showProgress = FALSE)
#   myData2 <- readAntares(links = "all", areas = "all", showProgress = FALSE)
#   myData3 <- readAntares(links = "all", areas = "all", showProgress = FALSE)
#   myData4 <- readAntares(links = "all", areas = "all", showProgress = FALSE)
# 
#   myDataList <- list(myData2, myData3, myData4)
#   myLink <- "a - a_offshore"
#   mytables <- "links"
#   myVariable <- "FLOW LIN."
#   DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
#   TsDaList <- tsPlot(x = myDataList,
#                   table = mytables,
#                   elements = myLink,
#                   type = "ts",
#                   interactive = FALSE,
#                   variable = myVariable)
#   # compare with myData3
#   idWidget <- 2
#   dataTsDaList <- .get_data_from_htmlwidget(TsDaList, widgetsNumber = idWidget)
#   timeEditValue <- "2018-04-25T00:00:00.000Z"
#   indexHour <- grep(timeEditValue, dataTsDaList$hour)
#   expect_gt(indexHour, 2)
#   expect_equal(dataTsDaList$`a - a_offshore`[indexHour], -9)
#   #with a refStudy
#   TsDaList <- tsPlot(x = myDataList,
#                      refStudy = myData1,
#                      table = mytables,
#                      elements = myLink,
#                      type = "ts",
#                      interactive = FALSE,
#                      variable = myVariable)
#   dataTsDaList <- .get_data_from_htmlwidget(TsDaList, widgetsNumber = idWidget)
#   expect_equal(dataTsDaList$`a - a_offshore`[indexHour], 0)
#   # edit myData3 to have a diff != 0
#   #pb timeZone local (PC, Travis, etc)
#   for (i in 0:5){
#     timeEditShift <- lubridate::hours(i)
#     timeEditMinus <- as.Date(timeEditValue) - timeEditShift
#     timeEditPlus <- as.Date(timeEditValue) + timeEditShift
#     myData3$links[ (time == timeEditMinus | time == timeEditPlus) & link == "a - a_offshore", `FLOW LIN.` := as.integer(`FLOW LIN.` + 2500)]
#   }
#   TsDaList <- tsPlot(x = myDataList,
#                      refStudy = myData1,
#                      table = mytables,
#                      elements = myLink,
#                      type = "ts",
#                      interactive = FALSE,
#                      variable = myVariable)
#   dataTsDaList <- .get_data_from_htmlwidget(TsDaList, widgetsNumber = idWidget)
#   expect_equal(dataTsDaList$`a - a_offshore`[indexHour], 2500)
# })
# 
# describe("tsPlot, interactive, x and refStudy are antaresDataTable", {
#   myData1 <- readAntares(links = "all", showProgress = FALSE)
#   myData2 <- readAntares(links = "all", showProgress = FALSE)
#   myLink <- "a - a_offshore"
#   DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
#   # no interactive
#   tsDa1 <- tsPlot(x = myData1,
#                   table = "links",
#                   elements = myLink,
#                   type = "ts",
#                   interactive = FALSE)
#   dataTsDa1 <- .get_data_from_htmlwidget(tsDa1)
#   timeEditValue <- "2018-04-25T00:00:00.000Z"
#   indexHour <- grep(timeEditValue, dataTsDa1$hour)
#   expect_gt(indexHour, 2)
#   expect_equal(dataTsDa1$`a - a_offshore`[indexHour], -9)
#   # interactive
#   tsDa1Int <- tsPlot(x = myData1,
#                   table = "links",
#                   elements = myLink,
#                   type = "ts",
#                   interactive = TRUE,
#                   .runApp = FALSE,
#                   dateRange = DR)
#   tsDa1Int <- tsDa1Int$init()
#   expect_true(is(tsDa1Int, "MWController"))
#   expect_equal(tsDa1Int$ncharts, 1)
#   expect_equal(tsDa1Int$ncol, 1)
#   expect_equal(tsDa1Int$nrow, 1)
#   tsDa1Int$setValue("mcYear", "average", reactive = FALSE)
#   tsDa1Int$updateCharts()
#   expect_equal(tsDa1Int$getValue("tables"), "links")
#   expect_equal(tsDa1Int$getValue("mcYear"), "average")
#   dataTsDAInt <- .get_data_from_htmlwidget(tsDa1Int)
#   indexHour <- grep(timeEditValue, dataTsDa1$hour)
#   expect_gt(indexHour, 2)
#   expect_lt(indexHour, 50)
#   # BUG with interactive
#   ## we must remove 24 hours ?
#   expect_equal(dataTsDAInt$`a - a_offshore`[indexHour - 24], -9)
#   # interactive with refStudy
#   tsDa1Int <- tsPlot(x = myData1,
#                      refStudy = myData2,
#                      table = "links",
#                      elements = myLink,
#                      type = "ts",
#                      interactive = TRUE,
#                      .runApp = FALSE,
#                      dateRange = DR)
#   tsDa1Int <- tsDa1Int$init()
#   expect_true(is(tsDa1Int, "MWController"))
#   tsDa1Int$setValue("mcYear", "average", reactive = FALSE)
#   dataTsDAInt <- .get_data_from_htmlwidget(tsDa1Int)
#   expect_equal(dataTsDAInt$`a - a_offshore`[indexHour - 24], 0)
# 
#   # edit myData2 to have a diff != 0
#   ## pb timeZone local (PC, Travis, etc)
#   for (i in 0:5){
#     timeEditShift <- lubridate::hours(i)
#     timeEditMinus <- as.Date(timeEditValue) - timeEditShift
#     timeEditPlus <- as.Date(timeEditValue) + timeEditShift
#     myData2[ (time == timeEditMinus | time == timeEditPlus) & link == myLink, `FLOW LIN.` := as.integer(`FLOW LIN.` + 2500)]
#   }
#   tsDa1Int <- tsPlot(x = myData2,
#                      refStudy = myData1,
#                      table = "links",
#                      elements = myLink,
#                      type = "ts",
#                      interactive = TRUE,
#                      .runApp = FALSE,
#                      dateRange = DR)
#   tsDa1Int <- tsDa1Int$init()
#   expect_true(is(tsDa1Int, "MWController"))
#   tsDa1Int$setValue("mcYear", "average", reactive = FALSE)
#   dataTsDAInt <- .get_data_from_htmlwidget(tsDa1Int)
#   expect_equal(dataTsDAInt$`a - a_offshore`[indexHour - 24], 2500)
# })
# 
# describe("tsPlot, interactive, x and refStudy are antaresDataList", {
#   myData1 <- readAntares(links = "all", areas = "all", showProgress = FALSE)
#   myData2 <- readAntares(links = "all", areas = "all", showProgress = FALSE)
#   myArea <- "b"
#   DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
#   # no interactive
#   tsDa1 <- tsPlot(x = myData1,
#                   table = "areas",
#                   elements = myArea,
#                   type = "ts",
#                   interactive = FALSE)
#   dataTsDa1 <- .get_data_from_htmlwidget(tsDa1)
#   timeEditValue <- "2018-04-25T00:00:00.000Z"
#   indexHour <- grep(timeEditValue, dataTsDa1$hour)
#   expect_gt(indexHour, 2)
#   expect_equal(dataTsDa1$b[indexHour], 2427150)
#   # interactive
#   tsDa1Int <- tsPlot(x = myData1,
#                      table = "areas",
#                      elements = myArea,
#                      type = "ts",
#                      interactive = TRUE,
#                      .runApp = FALSE,
#                      dateRange = DR)
#   tsDa1Int <- tsDa1Int$init()
#   expect_true(is(tsDa1Int, "MWController"))
#   expect_equal(tsDa1Int$ncharts, 1)
#   expect_equal(tsDa1Int$ncol, 1)
#   expect_equal(tsDa1Int$nrow, 1)
#   tsDa1Int$setValue("mcYear", "average", reactive = TRUE)
#   tsDa1Int$setValue("tables", "areas", reactive = TRUE)
#   tsDa1Int$setValue("elements", "b", reactive = TRUE)
#   tsDa1Int$updateCharts()
#   expect_equal(tsDa1Int$getValue("tables"), "areas")
#   expect_equal(tsDa1Int$getValue("mcYear"), "average")
#   expect_equal(tsDa1Int$getValue("elements"), "b")
#   dataTsDAInt <- .get_data_from_htmlwidget(tsDa1Int)
#   indexHour <- grep(timeEditValue, dataTsDa1$hour)
#   expect_gt(indexHour, 2)
#   expect_lt(indexHour, 50)
#   # BUG with interactive
#   ## we must remove 24 hours ?
#   expect_equal(dataTsDAInt$b[indexHour - 24], 2427150)
#   # interactive with refStudy
#   tsDa1Int <- tsPlot(x = myData1,
#                      refStudy = myData2,
#                      table = "areas",
#                      elements = myArea,
#                      type = "ts",
#                      interactive = TRUE,
#                      .runApp = FALSE,
#                      dateRange = DR)
#   tsDa1Int <- tsDa1Int$init()
#   expect_true(is(tsDa1Int, "MWController"))
#   tsDa1Int$setValue("mcYear", "average", reactive = FALSE)
#   dataTsDAInt <- .get_data_from_htmlwidget(tsDa1Int)
#   expect_equal(dataTsDAInt$b[indexHour - 24], 0)
# 
#   # edit myData2 to have a diff != 0
#   ## pb timeZone local (PC, Travis, etc)
#   for (i in 0:5){
#     timeEditShift <- lubridate::hours(i)
#     timeEditMinus <- as.Date(timeEditValue) - timeEditShift
#     timeEditPlus <- as.Date(timeEditValue) + timeEditShift
#     myData2$areas[ (time == timeEditMinus | time == timeEditPlus) & area == myArea, `OV. COST` := as.integer(`OV. COST` + 2500)]
#   }
#   tsDa1Int <- tsPlot(x = myData2,
#                      refStudy = myData1,
#                      table = "areas",
#                      elements = myArea,
#                      type = "ts",
#                      interactive = TRUE,
#                      .runApp = FALSE,
#                      dateRange = DR)
#   tsDa1Int <- tsDa1Int$init()
#   expect_true(is(tsDa1Int, "MWController"))
#   tsDa1Int$setValue("mcYear", "average", reactive = TRUE)
#   tsDa1Int$setValue("tables", "areas", reactive = TRUE)
#   tsDa1Int$setValue("elements", "b", reactive = TRUE)
#   tsDa1Int$updateCharts()
#   dataTsDAInt <- .get_data_from_htmlwidget(tsDa1Int)
#   expect_equal(dataTsDAInt$b[indexHour - 24], 2500)
# })
# 
# describe("tsPlot, no interactive, x and refStudy are optsH5 ", {
#   testthat::skip("Reason: .runTsPlotTest is not TRUE")
#   if (.requireRhdf5_Antares(stopP = FALSE)){
#     skip_if_not(.runTsPlotTest)
#     suppressMessages(writeAntaresH5(pathtemp, opts = opts, overwrite = TRUE))
#     optsH5 <- setSimulationPath(pathtemp)
#     myLink <- "a - a_offshore"
#     DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
#     # no interactive
#     tsDa1 <- tsPlot(x = optsH5,
#                     table = "links",
#                     elements = myLink,
#                     type = "ts",
#                     interactive = FALSE)
#     dataTsDa1 <- .get_data_from_htmlwidget(tsDa1)
#     timeEditValue <- "2018-04-25T00:00:00.000Z"
#     indexHour <- grep(timeEditValue, dataTsDa1$hour)
#     expect_gt(indexHour, 2)
#     expect_equal(dataTsDa1$`a - a_offshore`[indexHour], -9)
#     #ref Study
#     tsDa1 <- tsPlot(x = optsH5,
#                     refStudy = optsH5,
#                     table = "links",
#                     elements = myLink,
#                     type = "ts",
#                     interactive = FALSE)
#     dataTsDa1 <- .get_data_from_htmlwidget(tsDa1)
#     indexHour <- grep(timeEditValue, dataTsDa1$hour)
#     expect_gt(indexHour, 2)
#     expect_equal(dataTsDa1$`a - a_offshore`[indexHour], 0)
#     # Edit H5 file
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
# 
#     pathNewH5File <- file.path(pathNewH5, list.files(pathNewH5))
#     .h5Antares_edit_variable(
#       pathH5 = pathNewH5File,
#       link = myLink,
#       timeId = 1:100,
#       antVar = "FLOW LIN.",
#       newValue = 15000
#     )
# 
#     optsH5New <- setSimulationPath(path = pathNewH5File)
#     tsDa1 <- tsPlot(x = optsH5New,
#                     refStudy = optsH5,
#                     table = "links",
#                     elements = myLink,
#                     type = "ts",
#                     interactive = FALSE)
#     dataTsDa1 <- .get_data_from_htmlwidget(tsDa1)
#     indexHour <- grep(timeEditValue, dataTsDa1$hour)
#     expect_equal(dataTsDa1$`a - a_offshore`[indexHour], 15009)
#   }
# })
# 
# describe("tsPlot, no interactive, x is a list of optH5 and refStudy are optsH5 ", {
#   if (.requireRhdf5_Antares(stopP = FALSE)){
#     skip_if_not(.runTsPlotTest)
#     suppressMessages(writeAntaresH5(pathtemp, opts = opts, overwrite = TRUE))
#     optsH5 <- setSimulationPath(pathtemp)
# 
#     # with new Studies H5 test if compare prodStack works
#     ## create new folders h5
#     pathInitial <- file.path(dirname(pathtemp), basename(pathtemp))
# 
#     listFolderToCreate <- c("testH5v2", "testH5v3", "testH5v4")
#     for (folder in listFolderToCreate){
#       pathNewH5 <- file.path(pathInitial, folder)
#       if (!dir.exists(pathNewH5)){
#         dir.create(pathNewH5)
#       }
#       #write the study
#       #windows pb ? pathNewH5 <- gsub("/", "\\", pathNewH5, fixed = TRUE)
#       optsData <- antaresRead::setSimulationPath(path = studyPath)
#       suppressWarnings(
#         writeAntaresH5(
#           path = pathNewH5,
#           opts = optsData,
#           overwrite = TRUE,
#           supressMessages = TRUE)
#       )
#     }
#     idWidgetToEdit <- 2
#     pathH5FolderToEdit <- file.path(pathInitial, listFolderToCreate[[idWidgetToEdit]])
#     pathH5FileToEdit <- file.path(pathH5FolderToEdit, list.files(pathH5FolderToEdit))
#     newValueFlow <- 15000
#     myLink <- getLinks()[1]
#     .h5Antares_edit_variable(
#       pathH5 = pathH5FileToEdit,
#       link = myLink,
#       timeId = 1:100,
#       antVar = "FLOW LIN.",
#       newValue = newValueFlow
#     )
#     optsList <- list()
#     antaresDataListH5 <- list()
#     for (i in 1:length(listFolderToCreate)){
#       pathOptsI <- file.path(pathInitial, listFolderToCreate[[i]])
#       optsList[[i]] <- setSimulationPath(path = pathOptsI)
#       antaresDataListH5[[i]] <- readAntares(links = myLink)
#     }
#     #test the data from h5
#     #get the data from the h5 file
#     antaresDataRef <- readAntares(opts = optsH5, links = myLink)
#     expect_equal(max(antaresDataListH5[[idWidgetToEdit]]$`FLOW LIN.`), newValueFlow)
#     expect_equal(max(antaresDataListH5[[1]]$`FLOW LIN.`), max(antaresDataRef$`FLOW LIN.`))
#     expect_equal(max(antaresDataListH5[[3]]$`FLOW LIN.`), max(antaresDataRef$`FLOW LIN.`))
#     # get the data from htmlwidget
#     myArea <- "a"
#     DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
#     tsDa1 <- tsPlot(x = optsList,
#                     refStudy = optsH5,
#                     table = "links",
#                     elements = myLink,
#                     type = "ts",
#                     interactive = FALSE)
#     dataTsDa3 <- .get_data_from_htmlwidget(tsDa1, widgetsNumber = idWidgetToEdit)
#     timeEditValue <- "2018-04-25T00:00:00.000Z"
#     indexHour <- grep(timeEditValue, dataTsDa3$hour)
#     expect_equal(dataTsDa3$`a - a_offshore`[indexHour], 15009)
#     dataTsDa2 <- .get_data_from_htmlwidget(tsDa1, widgetsNumber = 1)
#     expect_equal(dataTsDa2$`a - a_offshore`[indexHour], 0)
#   }
# })
# 
# describe("tsPlot, interactive, x and refStudy are optsH5 ", {
#   if (.requireRhdf5_Antares(stopP = FALSE)){
#     skip_if_not(.runTsPlotTest)
#     suppressMessages(writeAntaresH5(pathtemp, opts = opts, overwrite = TRUE))
#     optsH5 <- setSimulationPath(pathtemp)
#     myLink <- "a - a_offshore"
#     DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
#     # no interactive
#     tsDa1 <- tsPlot(x = optsH5,
#                     table = "links",
#                     elements = myLink,
#                     type = "ts",
#                     interactive = FALSE)
#     dataTsDa1 <- .get_data_from_htmlwidget(tsDa1)
#     timeEditValue <- "2018-04-25T00:00:00.000Z"
#     indexHour <- grep(timeEditValue, dataTsDa1$hour)
#     expect_gt(indexHour, 2)
#     expect_equal(dataTsDa1$`a - a_offshore`[indexHour], -9)
#     #interactive
#     tsDa1Int <- tsPlot(x = optsH5,
#                     type = "ts",
#                     interactive = TRUE,
#                     .runApp = FALSE,
#                     h5requestFiltering = list(
#                       mcYears = 1
#                     ))
#     tsDa1Int <- tsDa1Int$init()
#     expect_true(is(tsDa1Int, "MWController"))
#     expect_equal(tsDa1Int$ncharts, 1)
#     expect_equal(tsDa1Int$ncol, 1)
#     expect_equal(tsDa1Int$nrow, 1)
#     expect_true(is((tsDa1Int$getValue("x_tranform")[[1]]), "antaresData"))
#     tsDa1Int$setValue("mcYear", 1, reactive = TRUE)
#     tsDa1Int$setValue("tables", "areas", reactive = TRUE)
#     tsDa1Int$setValue("elements", "b", reactive = TRUE)
#     tsDa1Int$setValue("variable", "LOAD", reactive = TRUE)
#     tsDa1Int$setValue("dateRange", DR, reactive = TRUE)
#     tsDa1Int$updateCharts()
#     tsDa1Int$setValue("mcYear", 1, reactive = TRUE)
#     tsDa1Int$setValue("meanYearH5", FALSE, reactive = TRUE)
#     tsDa1Int$updateCharts()
#     tsDa1Int$setValue("meanYearH5", FALSE, reactive = TRUE)
#     tsDa1Int$setValue("mcYear", 1, reactive = TRUE)
#     expect_equal(tsDa1Int$getValue("tables"), "areas")
#     expect_equal(tsDa1Int$getValue("mcYear"), 1)
#     expect_equal(tsDa1Int$getValue("elements"), "b")
#     expect_equal(tsDa1Int$getValue("variable"), "LOAD")
#     dataTsDa1 <- .get_data_from_htmlwidget(tsDa1Int)
#     indexHour <- grep(timeEditValue, dataTsDa1$hour)
#     expect_gt(indexHour, 2)
#     expect_equal(dataTsDa1$b[indexHour], 60262)
#     # Edit H5 file
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
#     .h5Antares_edit_variable(
#       pathH5 = pathNewH5File,
#       area = "b",
#       timeId = 1:100,
#       antVar = "LOAD",
#       newValue = 15000,
#       mcYear = 1
#     )
#     optsH5New <- setSimulationPath(path = pathNewH5File)
#     myData <- readAntares(areas = "b",
#                           select = "LOAD",
#                           opts = optsH5New,
#                           mcYears = 1)
#     myDataRef <- readAntares(areas = "b",
#                              select = "LOAD",
#                              opts = optsH5,
#                              mcYears = 1)
#     expect_equal(myData[area == "b" & timeId == 2737, LOAD], 15000)
#     expect_gt(myDataRef[area == "b" & timeId == 2737, LOAD], 16000)
# 
#     diffValue <- myData[area == "b" & timeId == 2737, LOAD] -
#       myDataRef[area == "b" & timeId == 2737, LOAD]
#     tsDa1Int <- tsPlot(x = optsH5New,
#                     refStudy = optsH5,
#                     type = "ts",
#                     interactive = TRUE,
#                     .runApp = FALSE,
#                     h5requestFiltering = list(
#                       mcYears = 1
#                     ))
#     tsDa1Int <- tsDa1Int$init()
#     expect_true(is(tsDa1Int, "MWController"))
#     expect_equal(tsDa1Int$ncharts, 1)
#     expect_equal(tsDa1Int$ncol, 1)
#     expect_equal(tsDa1Int$nrow, 1)
#     tsDa1Int$setValue("mcYear", 1, reactive = TRUE)
#     tsDa1Int$setValue("tables", "areas", reactive = TRUE)
#     tsDa1Int$setValue("elements", "b", reactive = FALSE)
#     tsDa1Int$setValue("variable", "LOAD", reactive = FALSE)
#     tsDa1Int$setValue("dateRange", DR, reactive = FALSE)
#     tsDa1Int$updateCharts()
#     expect_equal(tsDa1Int$getValue("tables"), "areas")
#     expect_equal(tsDa1Int$getValue("mcYear"), 1)
#     expect_equal(tsDa1Int$getValue("elements"), "b")
#     expect_equal(tsDa1Int$getValue("variable"), "LOAD")
#     dataTsDa1 <- .get_data_from_htmlwidget(tsDa1Int)
#     indexHour <- grep(timeEditValue, dataTsDa1$hour)
#     expect_equal(dataTsDa1$b[indexHour], diffValue)
#     # for links, no refStudy
#     tsDa1Int <- tsPlot(x = optsH5New,
#                        type = "ts",
#                        interactive = TRUE,
#                        .runApp = FALSE,
#                        h5requestFiltering = list(
#                          mcYears = 1
#                        ))
#     tsDa1Int <- tsDa1Int$init()
#     expect_true(is(tsDa1Int, "MWController"))
#     expect_equal(tsDa1Int$ncharts, 1)
#     expect_equal(tsDa1Int$ncol, 1)
#     expect_equal(tsDa1Int$nrow, 1)
#     tsDa1Int$setValue("mcYear", 1, reactive = TRUE)
#     tsDa1Int$setValue("tables", "links", reactive = TRUE)
#     tsDa1Int$setValue("elements", myLink, reactive = FALSE)
#     tsDa1Int$setValue("variable", "FLOW LIN.", reactive = FALSE)
#     tsDa1Int$setValue("dateRange", DR, reactive = FALSE)
#     tsDa1Int$updateCharts()
#     tsDa1Int$setValue("mcYear", 1, reactive = TRUE)
#     expect_true(is((tsDa1Int$getValue("x_tranform")[[1]]), "antaresData"))
#     expect_equal(tsDa1Int$getValue("tables"), "links")
#     expect_equal(tsDa1Int$getValue("mcYear"), 1)
#     expect_equal(tsDa1Int$getValue("elements"), myLink)
#     expect_equal(tsDa1Int$getValue("variable"), "FLOW LIN.")
#     dataTsDa1 <- .get_data_from_htmlwidget(tsDa1Int)
#     indexHour <- grep(timeEditValue, dataTsDa1$hour)
#     expect_equal(dataTsDa1$`a - a_offshore`[indexHour], -10)
#     # for links, with refStudy
#     tsDa1IntRef <- tsPlot(x = optsH5New,
#                        refStudy = optsH5,
#                        type = "ts",
#                        interactive = TRUE,
#                        .runApp = FALSE,
#                        h5requestFiltering = list(
#                          mcYears = 1
#                        ))
#     tsDa1IntRef <- tsDa1IntRef$init()
#     expect_true(is(tsDa1IntRef, "MWController"))
#     expect_equal(tsDa1IntRef$ncharts, 1)
#     expect_equal(tsDa1IntRef$ncol, 1)
#     expect_equal(tsDa1IntRef$nrow, 1)
#     expect_true(is((tsDa1IntRef$getValue("x_tranform")[[1]]), "antaresData"))
#     tsDa1IntRef$setValue("mcYear", 1, reactive = TRUE)
#     expect_equal(tsDa1IntRef$getValue("mcYear"), 1)
#     expect_equal(tsDa1IntRef$getValue("tables"), "areas")
#     tsDa1IntRef$setValue("tables", "links", reactive = TRUE)
#     tsDa1IntRef$setValue("elements", myLink, reactive = FALSE)
#     tsDa1IntRef$setValue("variable", "FLOW LIN.", reactive = FALSE)
#     tsDa1IntRef$setValue("dateRange", DR, reactive = FALSE)
#     tsDa1IntRef$setValue("meanYearH5", FALSE, reactive = TRUE)
#     tsDa1IntRef$updateCharts()
#     tsDa1IntRef$setValue("meanYearH5", TRUE, reactive = TRUE)
#     tsDa1IntRef$setValue("mcYear", 1, reactive = TRUE)
#     expect_true(is((tsDa1IntRef$getValue("x_tranform")[[1]]), "antaresData"))
#     expect_equal(tsDa1IntRef$getValue("tables"), "links")
#     expect_true(is((tsDa1IntRef$getValue("x_tranform")[[1]]), "antaresData"))
#     expect_equal(tsDa1IntRef$getValue("elements"), myLink)
#     expect_equal(tsDa1IntRef$getValue("variable"), "FLOW LIN.")
#     expect_equal(tsDa1IntRef$getValue("mcYear"), 1)
#     dataTsDa1Ref <- .get_data_from_htmlwidget(tsDa1IntRef)
#     indexHour <- grep(timeEditValue, dataTsDa1Ref$hour)
#     expect_equal(dataTsDa1Ref$`a - a_offshore`[indexHour], 0)
#   }
# })
# 
# describe("tsPlot, interactive, x is a list of optsH5 and refStudy optsH5", {
#   if (.requireRhdf5_Antares(stopP = FALSE)){
#     skip_if_not(.runTsPlotTest)
#     suppressMessages(writeAntaresH5(pathtemp, opts = opts, overwrite = TRUE))
#     optsH5 <- setSimulationPath(pathtemp)
# 
#     # with new Studies H5 test if compare prodStack works
#     ## create new folders h5
#     pathInitial <- file.path(dirname(pathtemp), basename(pathtemp))
#     listFolderToCreate <- c("testH5v2", "testH5v3", "testH5v4")
#     for (folder in listFolderToCreate){
#       pathNewH5 <- file.path(pathInitial, folder)
#       if (!dir.exists(pathNewH5)){
#         dir.create(pathNewH5)
#       }
# 
#       #write the study
#       #windows pb ? pathNewH5 <- gsub("/", "\\", pathNewH5, fixed = TRUE)
#       optsData <- antaresRead::setSimulationPath(path = studyPath)
#       suppressWarnings(
#         writeAntaresH5(
#           path = pathNewH5,
#           opts = optsData,
#           overwrite = TRUE,
#           supressMessages = TRUE)
#       )
#     }
#     pathH5FolderToEdit <- file.path(pathInitial, listFolderToCreate[[2]])
#     pathH5FileToEdit <- file.path(pathH5FolderToEdit, list.files(pathH5FolderToEdit))
#     myLink <- getLinks()[1]
#     newValueFlow <- 50000
#     mcYearToTestList <- c(2, NULL)
#     myVar <- "FLOW LIN."
#     for (mcYearToTest in mcYearToTestList){
#       .h5Antares_edit_variable(
#         pathH5 = pathH5FileToEdit,
#         link = myLink,
#         timeId = 1:100,
#         antVar = myVar,
#         newValue = newValueFlow,
#         mcYear = mcYearToTest
#       )
#       #stock the data
#       optsList <- list()
#       antaresDataListH5 <- list()
#       for (i in 1:length(listFolderToCreate)){
#         pathOptsI <- file.path(pathInitial, listFolderToCreate[[i]])
#         optsList[[i]] <- setSimulationPath(path = pathOptsI)
#         antaresDataListH5[[i]] <- readAntares(links = myLink,
#                                               mcYear = mcYearToTest,
#                                               opts = optsList[[i]])
#       }
#       DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
#       #try without refStudy and interactive == FALSE
#       indexHour <- 49
#       expect_equal(antaresDataListH5[[2]]$`FLOW LIN.`[[indexHour]], 50000)
#       if(is.null(mcYearToTest)){
#         valFlow <- (-9)
#       }else{
#         valFlow <- (-7)
#       }
#       expect_equal(antaresDataListH5[[1]]$`FLOW LIN.`[[indexHour]], valFlow)
#       expect_equal(antaresDataListH5[[3]]$`FLOW LIN.`[[indexHour]], valFlow)
# 
#       if(!is.null(mcYearToTest)){
#         expect_error(tsPlot(x = optsList,
#                             table = "links",
#                             elements = myLink,
#                             type = "ts",
#                             interactive = FALSE,
#                             mcYear = mcYearToTest),
#                      "You can't use mcYear for h5 file when interactive is set to FALSE. You can use mcYearh5.")
#       }
#       tsPlotNoInt <-  tsPlot(x = optsList,
#                              table = "links",
#                              elements = myLink,
#                              type = "ts",
#                              interactive = FALSE,
#                              mcYearh5  = mcYearToTest)
#       datatsPlotNoInt <- .get_data_from_htmlwidget(tsPlotNoInt, widgetsNumber = 2)
#       timeEditValue <- "2018-04-25T00:00:00.000Z"
#       indexHour <- grep(timeEditValue, datatsPlotNoInt$hour)
#       expect_gt(indexHour, 2)
#       expect_equal(datatsPlotNoInt$`a - a_offshore`[[indexHour]], 50000)
#       #interactive
#       tsPlotInt <-  tsPlot(x = optsList,
#                          type = "ts",
#                          dateRange = DR,
#                          .runApp = FALSE,
#                          interactive = TRUE,
#                          h5requestFiltering = list(
#                            mcYears = mcYearToTest
#                          ))
#       tsPlotInt <- tsPlotInt$init()
#       expect_true(is(tsPlotInt, "MWController"))
#       expect_equal(tsPlotInt$ncharts, 3)
#       expect_equal(tsPlotInt$ncol, 2)
#       expect_equal(tsPlotInt$nrow, 2)
#       tsPlotInt$setValue("mcYear", mcYearToTest, reactive = TRUE)
#       tsPlotInt$setValue("tables", "links", reactive = TRUE)
#       tsPlotInt$setValue("elements", myLink, reactive = TRUE)
#       tsPlotInt$setValue("variable", myVar, reactive = TRUE)
#       tsPlotInt$setValue("dateRange", DR, reactive = TRUE)
#       if(is.null(mcYearToTest)){
#         tsPlotInt$setValue("meanYearH5", FALSE, reactive = TRUE)
#         expect_equal(tsPlotInt$getValue("meanYearH5"), FALSE)
#         tsPlotInt$setValue("mcYear", "average", reactive = TRUE)
#       }else{
#         tsPlotInt$setValue("meanYearH5", TRUE, reactive = TRUE)
#         expect_equal(tsPlotInt$getValue("meanYearH5"), TRUE)
#       }
#       tsPlotInt$updateCharts()
#       expect_equal(tsPlotInt$getValue("tables"), "links")
#       tsPlotInt$setValue("mcYear", mcYearToTest, reactive = TRUE)
#       if(!is.null(mcYearToTest)){
#         expect_equal(tsPlotInt$getValue("mcYear"), mcYearToTest)
#       }else{
#         tsPlotInt$setValue("mcYear", "average", reactive = TRUE)
#         expect_equal(tsPlotInt$getValue("mcYear"), "average")
#       }
#       expect_equal(tsPlotInt$getValue("mcYearH5"),"1")
#       expect_equal(tsPlotInt$getValue("elements"), myLink)
#       expect_equal(tsPlotInt$getValue("variable"), myVar)
#       datatsPlotInt <- .get_data_from_htmlwidget(tsPlotInt, widgetsNumber = 2)
#       indexHour <- grep(timeEditValue, datatsPlotInt$hour)
#       expect_equal(datatsPlotInt$`a - a_offshore`[[indexHour]], 50000)
#       datatsPlotInt2 <- .get_data_from_htmlwidget(tsPlotInt, widgetsNumber = 1)
#       indexHour <- grep(timeEditValue, datatsPlotInt2$hour)
#       expect_equal(datatsPlotInt2$`a - a_offshore`[[indexHour]], valFlow)
#       #interactive with refStudy
#       tsPlotIntRef <-  tsPlot(x = optsList,
#                               refStudy = optsH5,
#                               type = "ts",
#                               dateRange = DR,
#                               .runApp = FALSE,
#                               interactive = TRUE,
#                               h5requestFiltering = list(
#                                 mcYears = mcYearToTest
#                              ))
#       tsPlotIntRef <- tsPlotIntRef$init()
#       expect_true(is(tsPlotIntRef, "MWController"))
#       expect_equal(tsPlotIntRef$ncharts, 3)
#       expect_equal(tsPlotIntRef$ncol, 2)
#       expect_equal(tsPlotIntRef$nrow, 2)
#       tsPlotIntRef$setValue("mcYear", mcYearToTest, reactive = TRUE)
#       tsPlotIntRef$setValue("tables", "links", reactive = TRUE)
#       tsPlotIntRef$setValue("elements", myLink, reactive = TRUE)
#       tsPlotIntRef$setValue("variable", myVar, reactive = TRUE)
#       tsPlotIntRef$setValue("dateRange", DR, reactive = TRUE)
#       if(is.null(mcYearToTest)){
#         tsPlotIntRef$setValue("meanYearH5", FALSE, reactive = TRUE)
#         expect_equal(tsPlotIntRef$getValue("meanYearH5"), FALSE)
#       }else{
#         tsPlotIntRef$setValue("meanYearH5", TRUE, reactive = TRUE)
#         expect_equal(tsPlotIntRef$getValue("meanYearH5"), TRUE)
#       }
#       tsPlotIntRef$updateCharts()
#       expect_equal(tsPlotIntRef$getValue("tables"), "links")
#       tsPlotIntRef$setValue("mcYear", mcYearToTest, reactive = TRUE)
#       if(!is.null(mcYearToTest)){
#         expect_equal(tsPlotInt$getValue("mcYear"), mcYearToTest)
#         expect_equal(tsPlotIntRef$getValue("mcYearH5"),"1")
#       }else{
#         tsPlotIntRef$setValue("mcYear", "average", reactive = TRUE)
#         tsPlotIntRef$setValue("mcYearH5", "", reactive = TRUE)
#         expect_equal(tsPlotIntRef$getValue("mcYearH5"),"")
#         expect_equal(tsPlotIntRef$getValue("mcYear"), "average")
#       }
#       expect_equal(tsPlotIntRef$getValue("elements"), myLink)
#       expect_equal(tsPlotIntRef$getValue("variable"), myVar)
#       datatsPlotIntRef <- .get_data_from_htmlwidget(tsPlotIntRef, widgetsNumber = 2)
#       indexHour <- grep(timeEditValue, datatsPlotIntRef$hour)
#       expect_equal(datatsPlotIntRef$`a - a_offshore`[[indexHour]], 50000-(valFlow))
#       datatsPlotIntRef2 <- .get_data_from_htmlwidget(tsPlotIntRef, widgetsNumber = 1)
#       indexHour <- grep(timeEditValue, datatsPlotIntRef2$hour)
#       expect_equal(datatsPlotIntRef2$`a - a_offshore`[[indexHour]], 0)
#     }
#   }
# })
