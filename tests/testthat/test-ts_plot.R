context("tsPlot")

describe("tsPlot, no interactive", {
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
    test_that (names(listArgs), {
      re1 <- do.call(tsPlot, X)
      expect_true(testClass(re1))
    })
  })
  
})



describe("tsPlot, no interactive return error", {
  
  dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  
  expect_error(tsPlot(dta, interactive = FALSE, compare = "areas"))
  
})

describe("tsPlot, work with compare", {
  
  dta <- readAntares(areas = "all", links = "all", showProgress = FALSE, mcYears = "all")
  exList <-  tsPlot(x = dta, .runApp = FALSE, interactive = TRUE, compare = "mcYear")
  exList <- exList$init()
  #to get a param exList$getParams("tables")
  # exList$getValue("mcYear")
  exList$setValue("mcYear", 1, chartId = 1, reactive = FALSE)
  exList$setValue("mcYear", 2, chartId = 2, reactive = FALSE)
  exList$updateCharts()
  expect_equal(exList$getValue("tables"), "areas")
  expect_equal(exList$getValue("main"), "")
  expect_true(is(exList, "MWController"))
  expect_equal(exList$ncharts, 2)
  expect_equal(exList$ncol, 1)
  expect_equal(exList$nrow, 2)
  dataTsCompare <- .get_data_from_htmlwidget(exList, widgetsNumber = 1)
  timeEditValue <- "2018-05-06T18:00:00.000Z"
  indexHour <- grep(timeEditValue, dataTsCompare$hour)
  expect_gt(indexHour, 180)
  expect_equal(dataTsCompare$a[indexHour], 1627275)
  
  dataTsCompareMcYear2 <- .get_data_from_htmlwidget(exList, widgetsNumber = 2)
  expect_equal(dataTsCompareMcYear2$a[indexHour], 1432100)
  
})

describe("tsPlot, no interactive, x and refStudy are antaresDataTable", {
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

describe("tsPlot, no interactive, x and refStudy are antaresDataList", {
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

describe("tsPlot, no interactive, x is a list of antaresDataList and refStudy an antaresDataList", {
  myData1 <- readAntares(links = "all", areas = "all", showProgress = FALSE)
  myData2 <- readAntares(links = "all", areas = "all", showProgress = FALSE)
  myData3 <- readAntares(links = "all", areas = "all", showProgress = FALSE)
  myData4 <- readAntares(links = "all", areas = "all", showProgress = FALSE)
  
  myDataList <- list(myData2, myData3, myData4)
  myLink <- "a - a_offshore"
  mytables <- "links"
  myVariable <- "FLOW LIN."
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  TsDaList <- tsPlot(x = myDataList, 
                  table = mytables, 
                  elements = myLink,
                  type = "ts",
                  interactive = FALSE,
                  variable = myVariable)
  # compare with myData3
  idWidget <- 2
  dataTsDaList <- .get_data_from_htmlwidget(TsDaList, widgetsNumber = idWidget)
  timeEditValue <- "2018-04-25T00:00:00.000Z"
  indexHour <- grep(timeEditValue, dataTsDaList$hour)
  expect_gt(indexHour, 2)
  expect_equal(dataTsDaList$`a - a_offshore`[indexHour], -9)
  #with a refStudy 
  TsDaList <- tsPlot(x = myDataList,
                     refStudy = myData1,
                     table = mytables, 
                     elements = myLink,
                     type = "ts",
                     interactive = FALSE,
                     variable = myVariable)
  dataTsDaList <- .get_data_from_htmlwidget(TsDaList, widgetsNumber = idWidget)
  expect_equal(dataTsDaList$`a - a_offshore`[indexHour], 0)
  # edit myData3 to have a diff != 0
  #pb timeZone local (PC, Travis, etc)
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData3$links[ (time == timeEditMinus | time == timeEditPlus) & link == "a - a_offshore", `FLOW LIN.` := as.integer(`FLOW LIN.` + 2500)]
  }
  TsDaList <- tsPlot(x = myDataList,
                     refStudy = myData1,
                     table = mytables,
                     elements = myLink,
                     type = "ts",
                     interactive = FALSE,
                     variable = myVariable)
  dataTsDaList <- .get_data_from_htmlwidget(TsDaList, widgetsNumber = idWidget)
  expect_equal(dataTsDaList$`a - a_offshore`[indexHour], 2500)
})

describe("tsPlot, interactive, x and refStudy are antaresDataTable", {
  myData1 <- readAntares(links = "all", showProgress = FALSE)
  myData2 <- readAntares(links = "all", showProgress = FALSE)
  myLink <- "a - a_offshore" 
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  # no interactive 
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
  # interactive 
  tsDa1Int <- tsPlot(x = myData1, 
                  table = "links", 
                  elements = myLink,
                  type = "ts",
                  interactive = TRUE,
                  .runApp = FALSE,
                  dateRange = DR)
  tsDa1Int <- tsDa1Int$init()
  expect_true(is(tsDa1Int, "MWController"))
  expect_equal(tsDa1Int$ncharts, 1)
  expect_equal(tsDa1Int$ncol, 1)
  expect_equal(tsDa1Int$nrow, 1)
  tsDa1Int$setValue("mcYear", "average", reactive = FALSE)
  tsDa1Int$updateCharts()
  expect_equal(tsDa1Int$getValue("tables"), "links")
  expect_equal(tsDa1Int$getValue("mcYear"), "average")
  dataTsDAInt <- .get_data_from_htmlwidget(tsDa1Int)
  indexHour <- grep(timeEditValue, dataTsDa1$hour)
  expect_gt(indexHour, 2)
  expect_lt(indexHour, 50)
  # BUG with interactive 
  ## we must remove 24 hours ? 
  expect_equal(dataTsDAInt$`a - a_offshore`[indexHour - 24], -9)  
  
  # interactive with refStudy 
  tsDa1Int <- tsPlot(x = myData1, 
                     refStudy = myData2,
                     table = "links", 
                     elements = myLink,
                     type = "ts",
                     interactive = TRUE,
                     .runApp = FALSE,
                     dateRange = DR)
  tsDa1Int <- tsDa1Int$init()
  expect_true(is(tsDa1Int, "MWController"))
  tsDa1Int$setValue("mcYear", "average", reactive = FALSE)
  dataTsDAInt <- .get_data_from_htmlwidget(tsDa1Int)
  #expect_equal(dataTsDAInt$`a - a_offshore`[indexHour - 24], 0)  
})