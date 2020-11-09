context("exchangesStack")

test_that("no interactive", {
  
  mydata <- readAntares(links = "all", timeStep = "daily", showProgress = FALSE)
  # default parameters
  default_params <- exchangesStack(mydata, interactive = FALSE)
  expect_is(default_params, "htmlwidget")
  # TO DO : passer les arguments
  # passer plusieurs data
  # .compare
  # suivant les cas : 
  # - tester les retours d'erreurs
})

test_that("exchangesStack, no interactive", {
  dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  testClass <- function(obj){
    class(obj)[1] == "combineWidgets"
  }
  listArgs <- list(noarg = list(x = dta, interactive = FALSE, areas = "a"),
                   allAreas = list(x = dta, interactive = FALSE, areas = "all"),
                   main = list(x = dta, interactive = FALSE, areas = "all", main = "Title"),
                   ylab = list(x = dta, interactive = FALSE, areas = "all", main = "Title", ylab = "Subt")
  )
  lapply(listArgs, function(X){
    re1 <- do.call(exchangesStack, X)
    expect_true(testClass(re1))
  })
  
})

test_that("exchangesStack, no interactive return error", {
  dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  expect_error(exchangesStack(dta, interactive = FALSE, compare = "areas"))
})

test_that("exchangesStack, no interactive, x and refStudy are antaresDataTable", {
  myData1 <- readAntares(links = "all", showProgress = FALSE)
  myData2 <- readAntares(links = "all", showProgress = FALSE)
  myArea <- "a"
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  exS1 <-  exchangesStack(x = myData1, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExS1 <- .get_data_from_htmlwidget(exS1)
  timeEditValue <- "2018-04-25T00:00:00.000Z"
  indexHour <- grep(timeEditValue, dataExS1$hour)
  expect_gt(indexHour, 2)
  expect_equal(dataExS1$nega_offshore[indexHour], 9)
  #identical myData, diff == 0 always
  exS21V0 <-  exchangesStack(x = myData1, refStudy = myData2, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExS21V0 <- .get_data_from_htmlwidget(exS21V0)
  expect_equal(dataExS21V0$nega_offshore[indexHour], 0)
  # edit myData2 to have a diff != 0
  #pb timeZone local (PC, Travis, etc)
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData2[ (time == timeEditMinus | time == timeEditPlus) & link == "a - a_offshore", `FLOW LIN.` := as.integer(`FLOW LIN.` + 2500)]
  }
  exS21V1 <-  exchangesStack(x = myData1, refStudy = myData2, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExS21V1 <- .get_data_from_htmlwidget(exS21V1)
  expect_equal(dataExS21V1$nega_offshore[indexHour], 2500)
})

test_that("exchangesStack, no interactive, x and refStudy are antaresDataList", {
  myData1 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData2 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myArea <- "a"
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  exS1 <-  exchangesStack(x = myData1, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExS1 <- .get_data_from_htmlwidget(exS1)
  timeEditValue <- "2018-04-25T00:00:00.000Z"
  indexHour <- grep(timeEditValue, dataExS1$hour)
  expect_gt(indexHour, 2)
  expect_equal(dataExS1$nega_offshore[indexHour], 9)
  #identical myData, diff == 0 always
  exS21V0 <-  exchangesStack(x = myData1, refStudy = myData2, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExS21V0 <- .get_data_from_htmlwidget(exS21V0)
  expect_equal(dataExS21V0$nega_offshore[indexHour], 0)
  # edit myData2 to have a diff != 0
  #pb timeZone local (PC, Travis, etc)
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData2$links[ (time == timeEditMinus | time == timeEditPlus) & link == "a - a_offshore", `FLOW LIN.` := as.integer(`FLOW LIN.` + 2500)]
  }
  exS21V1 <-  exchangesStack(x = myData1, refStudy = myData2, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExS21V1 <- .get_data_from_htmlwidget(exS21V1)
  expect_equal(dataExS21V1$nega_offshore[indexHour], 2500)
  #ROW not null in myData1
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData1$areas[ (time == timeEditMinus | time == timeEditPlus) & area == myArea, `ROW BAL.` := as.integer(`ROW BAL.` - 1500)]
  }
  #test if there is row
  exS1V2 <-  exchangesStack(x = myData1, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExS1V2 <- .get_data_from_htmlwidget(exS1V2)
  expect_equal(dataExS1V2$ROW[indexHour], 1500)
  exS21V2 <-  exchangesStack(x = myData1, refStudy = myData2, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExS21V2 <- .get_data_from_htmlwidget(exS21V2)
  expect_equal(dataExS21V2$nega_offshore[indexHour], 2500)
  expect_equal(dataExS21V2$ROW[indexHour], 1500)
  #ROW not null in myData2
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData2$areas[ (time == timeEditMinus | time == timeEditPlus) & area == myArea, `ROW BAL.` := as.integer(`ROW BAL.` - 1000)]
  }
  exS21V3 <-  exchangesStack(x = myData1, refStudy = myData2, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExS21V3 <- .get_data_from_htmlwidget(exS21V3)
  expect_equal(dataExS21V3$nega_offshore[indexHour], 2500)
  expect_equal(dataExS21V3$ROW[indexHour], 500)
})

test_that("exchangesStack, no interactive, x is a list of antaresDataTable and refStudy an antaresDataTable", {
  myData1 <- readAntares(links = "all", showProgress = FALSE)
  myData2 <- readAntares(links = "all", showProgress = FALSE)
  myData3 <- readAntares(links = "all", showProgress = FALSE)
  myData4 <- readAntares(links = "all", showProgress = FALSE)
  
  myDataList <- list(myData2, myData3, myData4)
  myArea <- "a"
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  exS2 <-  exchangesStack(x = myDataList, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  # compare with myData3
  idWidget <- 2
  dataExS2 <- .get_data_from_htmlwidget(exS2, widgetsNumber = idWidget)
  timeEditValue <- "2018-04-25T00:00:00.000Z"
  indexHour <- grep(timeEditValue, dataExS2$hour)
  expect_gt(indexHour, 2)
  expect_equal(dataExS2$nega_offshore[indexHour], 9)
  #identical myData, diff == 0 always
  exS21V0 <-  exchangesStack(x = myDataList, refStudy = myData1, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExS21V0 <- .get_data_from_htmlwidget(exS21V0, widgetsNumber = idWidget)
  expect_equal(dataExS21V0$nega_offshore[indexHour], 0)
  # edit myData3 to have a diff != 0
  #pb timeZone local (PC, Travis, etc)
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData3[ (time == timeEditMinus | time == timeEditPlus) & link == "a - a_offshore", `FLOW LIN.` := as.integer(`FLOW LIN.` + 2500)]
  }
  exS21V1 <-  exchangesStack(x = myDataList, refStudy = myData1, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExS21V1 <- .get_data_from_htmlwidget(exS21V1, widgetsNumber = idWidget)
  expect_equal(dataExS21V1$a_offshore[indexHour], 2500)
})

test_that("exchangesStack, no interactive, x is a list of antaresDataList and refStudy an antaresDataList", {
  myData1 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData2 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData3 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData4 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  
  myDataList <- list(myData2, myData3, myData4)
  myArea <- "a"
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  exS2 <-  exchangesStack(x = myDataList, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  # compare with myData3
  idWidget <- 2
  dataExS2 <- .get_data_from_htmlwidget(exS2, widgetsNumber = idWidget)
  timeEditValue <- "2018-04-25T00:00:00.000Z"
  indexHour <- grep(timeEditValue, dataExS2$hour)
  expect_gt(indexHour, 2)
  expect_equal(dataExS2$nega_offshore[indexHour], 9)
  #identical myData, diff == 0 always
  exS21V0 <-  exchangesStack(x = myDataList, refStudy = myData1, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExS21V0 <- .get_data_from_htmlwidget(exS21V0, widgetsNumber = idWidget)
  expect_equal(dataExS21V0$nega_offshore[indexHour], 0)
  # edit myData3 to have a diff != 0
  #pb timeZone local (PC, Travis, etc)
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData3$links[ (time == timeEditMinus | time == timeEditPlus) & link == "a - a_offshore", `FLOW LIN.` := as.integer(`FLOW LIN.` + 2500)]
  }
  exS21V1 <-  exchangesStack(x = myDataList, refStudy = myData1, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExS21V1 <- .get_data_from_htmlwidget(exS21V1, widgetsNumber = idWidget)
  expect_equal(dataExS21V1$a_offshore[indexHour], 2500)
  #ROW not null in myData4
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData4$areas[ (time == timeEditMinus | time == timeEditPlus) & area == myArea, `ROW BAL.` := as.integer(`ROW BAL.` - 1500)]
  }
  #test if there is row
  exList <-  exchangesStack(x = myDataList, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExList <- .get_data_from_htmlwidget(exList, widgetsNumber = idWidget)
  expect_equal(dataExList$a_offshore[indexHour], 2500 - 9)
  idRowNotNull <- 3
  dataExListRow <- .get_data_from_htmlwidget(exList, widgetsNumber = idRowNotNull)
  expect_equal(dataExListRow$ROW[indexHour], 1500)
  #with a refStudy
  exListListV2 <-  exchangesStack(x = myDataList, refStudy = myData1, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExListV2 <- .get_data_from_htmlwidget(exListListV2, widgetsNumber = idWidget)
  expect_equal(dataExListV2$a_offshore[indexHour], 2500)
  expect_equal(dataExListV2$ROW[indexHour], 0)
  dataExListV2Row <- .get_data_from_htmlwidget(exListListV2, widgetsNumber = idRowNotNull)
  expect_equal(dataExListV2Row$a_offshore[indexHour], 0)
  expect_equal(dataExListV2Row$ROW[indexHour], 1500)
  #ROW not null in refStudy myData1
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData1$areas[ (time == timeEditMinus | time == timeEditPlus) & area == myArea, `ROW BAL.` := as.integer(`ROW BAL.` - 1000)]
  }
  exListListV3 <-  exchangesStack(x = myDataList, refStudy = myData1, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExListV3 <- .get_data_from_htmlwidget(exListListV3, widgetsNumber = idRowNotNull)
  expect_equal(dataExListV3$nega_offshore[indexHour], 0)
  expect_equal(dataExListV3$ROW[indexHour], 500)
  dataExListV3g2 <- .get_data_from_htmlwidget(exListListV3, widgetsNumber = idWidget)
  expect_equal(dataExListV3g2$a_offshore[indexHour], 2500)
  expect_equal(dataExListV3g2$negROW[indexHour], 1000)
})

test_that("exchangesStack, interactive, x and refStudy are antaresDataTable", {
  skip_if_not(.runExchangesStackTest)
  myData1 <- readAntares(links = "all", showProgress = FALSE)
  myData2 <- readAntares(links = "all", showProgress = FALSE)
  myArea <- "a"
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  #no interactive
  exS1 <-  exchangesStack(x = myData1, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExS1 <- .get_data_from_htmlwidget(exS1)
  timeEditValue <- "2018-04-25T00:00:00.000Z"
  indexHour <- grep(timeEditValue, dataExS1$hour)
  expect_gt(indexHour, 2)
  expect_equal(dataExS1$nega_offshore[indexHour], 9)
  # interactive 
  exS1 <-  exchangesStack(x = myData1, .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  exS1 <- exS1$init()
  expect_true(is(exS1, "MWController"))
  expect_equal(exS1$ncharts, 1)
  expect_equal(exS1$ncol, 1)
  expect_equal(exS1$nrow, 1)
  dataExS1 <- .get_data_from_htmlwidget(exS1)
  expect_equal(dataExS1$nega_offshore[indexHour], 9)  
  
  #identical myData, diff == 0 always
  exS21V0 <-  exchangesStack(x = myData1, refStudy = myData2, .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  resExS1V0 <- exS21V0$init()
  expect_true(is(exS21V0, "MWController"))
  expect_equal(exS21V0$ncharts, 1)
  expect_equal(exS21V0$ncol, 1)
  expect_equal(exS21V0$nrow, 1)
  #get the data
  dataExS21V0 <- .get_data_from_htmlwidget(exS21V0)
  expect_equal(dataExS21V0$nega_offshore[indexHour], 0)
  # edit myData2 to have a diff != 0
  #pb timeZone local (PC, Travis, etc)
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData2[ (time == timeEditMinus | time == timeEditPlus) & link == "a - a_offshore", `FLOW LIN.` := as.integer(`FLOW LIN.` + 2500)]
  }
  exS21V1 <-  exchangesStack(x = myData1, refStudy = myData2, .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  resExS1V0 <- exS21V1$init()
  expect_true(is(exS21V1, "MWController"))
  expect_equal(exS21V1$ncharts, 1)
  expect_equal(exS21V1$ncol, 1)
  expect_equal(exS21V1$nrow, 1)
  dataExS21V1 <- .get_data_from_htmlwidget(exS21V1)
  expect_equal(dataExS21V1$nega_offshore[indexHour], 2500)
})

test_that("exchangesStack, interactive, x and refStudy are antaresDataList", {
  skip_if_not(.runExchangesStackTest)
  myData1 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData2 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myArea <- "a"
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  #no interactive
  exS1 <-  exchangesStack(x = myData1, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  dataExS1 <- .get_data_from_htmlwidget(exS1)
  timeEditValue <- "2018-04-25T00:00:00.000Z"
  indexHour <- grep(timeEditValue, dataExS1$hour)
  expect_gt(indexHour, 2)
  expect_equal(dataExS1$nega_offshore[indexHour], 9)
  # interactive no interactive
  exS1I <-  exchangesStack(x = myData1, .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  resExS1 <- exS1I$init()
  expect_true(is(exS1I, "MWController"))
  expect_equal(exS1I$ncharts, 1)
  expect_equal(exS1I$ncol, 1)
  expect_equal(exS1I$nrow, 1)
  dataExS1I <- .get_data_from_htmlwidget(exS1I)
  expect_equal(dataExS1I$nega_offshore[indexHour], 9)
  
  # interactive with refStudy but myData1 =myData2
  exS21V0 <- exchangesStack(x = myData1, refStudy = myData2, .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  exS21V0 <- exS21V0$init()
  expect_true(is(exS21V0, "MWController"))
  expect_equal(exS21V0$ncharts, 1)
  expect_equal(exS21V0$ncol, 1)
  expect_equal(exS21V0$nrow, 1)
  dataExS21V0 <- .get_data_from_htmlwidget(exS21V0)
  expect_equal(dataExS21V0$nega_offshore[indexHour], 0)
  # edit myData2 to have a diff != 0
  #pb timeZone local (PC, Travis, etc)
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData2$links[ (time == timeEditMinus | time == timeEditPlus) & link == "a - a_offshore", `FLOW LIN.` := as.integer(`FLOW LIN.` + 2500)]
  }
  exS21V1 <-  exchangesStack(x = myData1, refStudy = myData2, .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  exS21V1 <- exS21V1$init()
  expect_true(is(exS21V1, "MWController"))
  expect_equal(exS21V1$ncharts, 1)
  expect_equal(exS21V1$ncol, 1)
  expect_equal(exS21V1$nrow, 1)
  dataExS21V1 <- .get_data_from_htmlwidget(exS21V1)
  expect_equal(dataExS21V1$nega_offshore[indexHour], 2500)
  #ROW not null in myData1
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData1$areas[ (time == timeEditMinus | time == timeEditPlus) & area == myArea, `ROW BAL.` := as.integer(`ROW BAL.` - 1500)]
  }
  #test if there is row
  exS1V2 <-  exchangesStack(x = myData1,  .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  exS1V2 <- exS1V2$init()
  expect_true(is(exS1V2, "MWController"))
  expect_equal(exS1V2$ncharts, 1)
  expect_equal(exS1V2$ncol, 1)
  expect_equal(exS1V2$nrow, 1)
  dataExS1V2 <- .get_data_from_htmlwidget(exS1V2)
  expect_equal(dataExS1V2$ROW[indexHour], 1500)
  exS21V2 <-  exchangesStack(x = myData1, refStudy = myData2, .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  exS21V2 <- exS21V2$init()
  expect_true(is(exS21V2, "MWController"))
  expect_equal(exS21V2$ncharts, 1)
  expect_equal(exS21V2$ncol, 1)
  expect_equal(exS21V2$nrow, 1)
  dataExS21V2 <- .get_data_from_htmlwidget(exS21V2)
  expect_equal(dataExS21V2$nega_offshore[indexHour], 2500)
  expect_equal(dataExS21V2$ROW[indexHour], 1500)
  #ROW not null in myData2
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData2$areas[ (time == timeEditMinus | time == timeEditPlus) & area == myArea, `ROW BAL.` := as.integer(`ROW BAL.` - 1000)]
  }
  exS21V3 <-  exchangesStack(x = myData1, refStudy = myData2,  .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  exS21V3 <- exS21V3$init()
  expect_true(is(exS21V3, "MWController"))
  expect_equal(exS21V3$ncharts, 1)
  expect_equal(exS21V3$ncol, 1)
  expect_equal(exS21V3$nrow, 1)
  dataExS21V3 <- .get_data_from_htmlwidget(exS21V3)
  expect_equal(dataExS21V3$nega_offshore[indexHour], 2500)
  expect_equal(dataExS21V3$ROW[indexHour], 500)
})

test_that("exchangesStack, interactive, x is a list of antaresDataTable and refStudy an antaresDataTable", {
  skip_if_not(.runExchangesStackTest)
  myData1 <- readAntares(links = "all", showProgress = FALSE)
  myData2 <- readAntares(links = "all", showProgress = FALSE)
  myData3 <- readAntares(links = "all", showProgress = FALSE)
  myData4 <- readAntares(links = "all", showProgress = FALSE)
  myDataList <- list(myData2, myData3, myData4)
  myArea <- "a"
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  # no interactive
  exS2 <-  exchangesStack(x = myDataList, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  # compare with myData3
  idWidget <- 2
  dataExS2 <- .get_data_from_htmlwidget(exS2, widgetsNumber = idWidget)
  timeEditValue <- "2018-04-25T00:00:00.000Z"
  indexHour <- grep(timeEditValue, dataExS2$hour)
  expect_gt(indexHour, 2)
  expect_equal(dataExS2$nega_offshore[indexHour], 9)
  # interactive
  exSList1 <-  exchangesStack(x = myDataList, .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  exSList1 <- exSList1$init()
  expect_true(is(exSList1, "MWController"))
  expect_equal(exSList1$ncharts, 3)
  expect_equal(exSList1$ncol, 2)
  expect_equal(exSList1$nrow, 2)
  dataExS1I <- .get_data_from_htmlwidget(exSList1, widgetsNumber = idWidget)
  expect_equal(dataExS1I$nega_offshore[indexHour], 9)
  #identical myData, diff == 0 always
  exS21V0 <-  exchangesStack(x = myDataList, refStudy = myData1, .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  exS21V0 <- exS21V0$init()
  expect_true(is(exS21V0, "MWController"))
  expect_equal(exS21V0$ncharts, 3)
  expect_equal(exS21V0$ncol, 2)
  expect_equal(exS21V0$nrow, 2)
  dataExS21V0 <- .get_data_from_htmlwidget(exS21V0, widgetsNumber = idWidget)
  expect_equal(dataExS21V0$nega_offshore[indexHour], 0)
  # edit myData3 to have a diff != 0
  #pb timeZone local (PC, Travis, etc)
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData3[ (time == timeEditMinus | time == timeEditPlus) & link == "a - a_offshore", `FLOW LIN.` := as.integer(`FLOW LIN.` + 2500)]
  }
  exS21V1 <-  exchangesStack(x = myDataList, refStudy = myData1, .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  exS21V1 <- exS21V1$init()
  expect_true(is(exS21V1, "MWController"))
  expect_equal(exS21V1$ncharts, 3)
  expect_equal(exS21V1$ncol, 2)
  expect_equal(exS21V1$nrow, 2)  
  dataExS21V1 <- .get_data_from_htmlwidget(exS21V1, widgetsNumber = idWidget)
  expect_equal(dataExS21V1$a_offshore[indexHour], 2500)
})

test_that("exchangesStack, interactive, x is a list of antaresDataList and refStudy an antaresDataList", {
  skip_if_not(.runExchangesStackTest)
  myData1 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData2 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData3 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData4 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  
  myDataList <- list(myData2, myData3, myData4)
  myArea <- "a"
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  # no interactive
  exS2 <-  exchangesStack(x = myDataList, interactive = FALSE, area = myArea, dateRange = DR, stepPlot = TRUE)
  # compare with myData3
  idWidget <- 2
  dataExS2 <- .get_data_from_htmlwidget(exS2, widgetsNumber = idWidget)
  timeEditValue <- "2018-04-25T00:00:00.000Z"
  indexHour <- grep(timeEditValue, dataExS2$hour)
  expect_gt(indexHour, 2)
  expect_equal(dataExS2$nega_offshore[indexHour], 9)
  # interactive 
  exSList1 <-  exchangesStack(x = myDataList, .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  exSList1 <- exSList1$init()
  expect_true(is(exSList1, "MWController"))
  expect_equal(exSList1$ncharts, 3)
  expect_equal(exSList1$ncol, 2)
  expect_equal(exSList1$nrow, 2)
  dataExS1I <- .get_data_from_htmlwidget(exSList1, widgetsNumber = idWidget)
  expect_equal(dataExS1I$nega_offshore[indexHour], 9)
  
  #identical myData, diff == 0 always
  exSList1Ref <-  exchangesStack(x = myDataList, refStudy = myData1, .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  exSList1Ref <- exSList1Ref$init()
  expect_true(is(exSList1Ref, "MWController"))
  expect_equal(exSList1Ref$ncharts, 3)
  expect_equal(exSList1Ref$ncol, 2)
  expect_equal(exSList1Ref$nrow, 2)
  dataExS21V0Ref <- .get_data_from_htmlwidget(exSList1Ref, widgetsNumber = idWidget)
  expect_equal(dataExS21V0Ref$nega_offshore[indexHour], 0)
  # edit myData3 to have a diff != 0
  #pb timeZone local (PC, Travis, etc)
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData3$links[ (time == timeEditMinus | time == timeEditPlus) & link == "a - a_offshore", `FLOW LIN.` := as.integer(`FLOW LIN.` + 2500)]
  }
  exS21V1 <-  exchangesStack(x = myDataList, refStudy = myData1, .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  exS21V1 <- exS21V1$init()
  expect_true(is(exS21V1, "MWController"))
  expect_equal(exS21V1$ncharts, 3)
  expect_equal(exS21V1$ncol, 2)
  expect_equal(exS21V1$nrow, 2)
  dataExS21V1 <- .get_data_from_htmlwidget(exS21V1, widgetsNumber = idWidget)
  expect_equal(dataExS21V1$a_offshore[indexHour], 2500)
  #ROW not null in myData4
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData4$areas[ (time == timeEditMinus | time == timeEditPlus) & area == myArea, `ROW BAL.` := as.integer(`ROW BAL.` - 1500)]
  }
  #test if there is row
  exList <-  exchangesStack(x = myDataList, .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  exList <- exList$init()
  expect_true(is(exList, "MWController"))
  expect_equal(exList$ncharts, 3)
  expect_equal(exList$ncol, 2)
  expect_equal(exList$nrow, 2)
  dataExList <- .get_data_from_htmlwidget(exList, widgetsNumber = idWidget)
  expect_equal(dataExList$a_offshore[indexHour], 2500 - 9)
  idRowNotNull <- 3
  dataExListRow <- .get_data_from_htmlwidget(exList, widgetsNumber = idRowNotNull)
  expect_equal(dataExListRow$ROW[indexHour], 1500)
  #with a refStudy
  exListListV2 <-  exchangesStack(x = myDataList, refStudy = myData1, .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  exListListV2 <- exListListV2$init()
  expect_true(is(exListListV2, "MWController"))
  expect_equal(exListListV2$ncharts, 3)
  expect_equal(exListListV2$ncol, 2)
  expect_equal(exListListV2$nrow, 2) 
  dataExListV2 <- .get_data_from_htmlwidget(exListListV2, widgetsNumber = idWidget)
  expect_equal(dataExListV2$a_offshore[indexHour], 2500)
  expect_equal(dataExListV2$ROW[indexHour], 0)
  dataExListV2Row <- .get_data_from_htmlwidget(exListListV2, widgetsNumber = idRowNotNull)
  expect_equal(dataExListV2Row$a_offshore[indexHour], 0)
  expect_equal(dataExListV2Row$ROW[indexHour], 1500)
  #ROW not null in refStudy myData1
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(timeEditValue) - timeEditShift
    timeEditPlus <- as.Date(timeEditValue) + timeEditShift
    myData1$areas[ (time == timeEditMinus | time == timeEditPlus) & area == myArea, `ROW BAL.` := as.integer(`ROW BAL.` - 1000)]
  }
  exListListV3 <-  exchangesStack(x = myDataList, refStudy = myData1, .runApp = FALSE, interactive = TRUE, area = myArea, dateRange = DR, stepPlot = TRUE)
  exListListV3 <- exListListV3$init()
  expect_true(is(exListListV3, "MWController"))
  expect_equal(exListListV3$ncharts, 3)
  expect_equal(exListListV3$ncol, 2)
  expect_equal(exListListV3$nrow, 2) 
  dataExListV3 <- .get_data_from_htmlwidget(exListListV3, widgetsNumber = idRowNotNull)
  expect_equal(dataExListV3$nega_offshore[indexHour], 0)
  expect_equal(dataExListV3$ROW[indexHour], 500)
  dataExListV3g2 <- .get_data_from_htmlwidget(exListListV3, widgetsNumber = idWidget)
  expect_equal(dataExListV3g2$a_offshore[indexHour], 2500)
  expect_equal(dataExListV3g2$negROW[indexHour], 1000)
})

test_that("exchangesStack, no interactive, x and refStudy are optsH5 ", {
  if (.requireRhdf5_Antares(stopP = FALSE)){
    skip_if_not(.runExchangesStackTest)
    suppressMessages(writeAntaresH5(pathtemp, opts = opts, overwrite = TRUE))
    optsH5 <- setSimulationPath(pathtemp)
    myArea <- "a"
    DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
    ES1 <-  exchangesStack(x = optsH5, interactive = FALSE, area = myArea, dateRange = DR)
    dataHtmlWidgetES1 <- .get_data_from_htmlwidget(ES1)
    timeEditValue <- "2018-04-25T00:00:00.000Z"
    indexHour <- grep(timeEditValue, dataHtmlWidgetES1$hour)
    expect_gt(indexHour, 2)
    expect_equal(dataHtmlWidgetES1$nega_offshore[indexHour], 9)
    # with refStudy
    ESRef <-  exchangesStack(x = optsH5, refStudy = optsH5, interactive = FALSE, area = myArea, dateRange = DR)
    dataHtmlWidgetES1 <- .get_data_from_htmlwidget(ESRef)
    expect_equal(dataHtmlWidgetES1$nega_offshore[indexHour], 0)
    # with a new Study H5 test if compare prodStack works
    ## create a new folder h5
    pathInitial <- file.path(dirname(pathtemp), basename(pathtemp))
    pathNewH5 <- file.path(pathInitial, "testH5")
    if (!dir.exists(pathNewH5)){
      dir.create(pathNewH5)
    }
    #write the study
    #windows pb ? pathNewH5 <- gsub("/", "\\", pathNewH5, fixed = TRUE)
    optsData <- antaresRead::setSimulationPath(path = studyPath)
    suppressWarnings(writeAntaresH5(path = pathNewH5, opts = optsData, 
                                    overwrite = TRUE, supressMessages = TRUE))
    
    
    pathNewH5File <- file.path(pathNewH5, list.files(pathNewH5))
    myLink <- getLinks()[1]
    .h5Antares_edit_variable(
      pathH5 = pathNewH5File, 
      link = myLink, 
      timeId = 1:40, 
      antVar = "FLOW LIN.", 
      newValue = 15000
    )
    
    optsH5New <- setSimulationPath(path = pathNewH5File)
    ES1New <-  exchangesStack(x = optsH5New, interactive = FALSE, area = myArea, dateRange = DR)
    dataHtmlWidgetES1 <- .get_data_from_htmlwidget(ES1New)
    expect_equal(dataHtmlWidgetES1$nega_offshore[indexHour], 9)
    expect_equal(dataHtmlWidgetES1$a_offshore[2], 15000)
    ES1NewRef <-  exchangesStack(x = optsH5New, refStudy = optsH5, interactive = FALSE, area = myArea, dateRange = DR)
    dataHtmlWidgetES1Ref <- .get_data_from_htmlwidget(ES1NewRef)
    expect_equal(dataHtmlWidgetES1Ref$nega_offshore[indexHour], 0)
    expect_gt(dataHtmlWidgetES1Ref$a_offshore[2], 15000)
  }
})

test_that("exchangesStack, no interactive, x is a list of optH5 and refStudy are optsH5 ", {
  if (.requireRhdf5_Antares(stopP = FALSE)){
    skip_if_not(.runExchangesStackTest)
    suppressMessages(writeAntaresH5(pathtemp, opts = opts, overwrite = TRUE))
    optsH5 <- setSimulationPath(pathtemp)
    
    # with new Studies H5 test if compare prodStack works
    ## create new folders h5
    pathInitial <- file.path(dirname(pathtemp), basename(pathtemp))
    
    listFolderToCreate <- c("testH5v2", "testH5v3", "testH5v4")
    for (folder in listFolderToCreate){
      pathNewH5 <- file.path(pathInitial, folder)
      if (!dir.exists(pathNewH5)){
        dir.create(pathNewH5)
      }
      
      #write the study
      #windows pb ? pathNewH5 <- gsub("/", "\\", pathNewH5, fixed = TRUE)
      optsData <- antaresRead::setSimulationPath(path = studyPath)
      suppressWarnings(
        writeAntaresH5(
          path = pathNewH5, 
          opts = optsData, 
          overwrite = TRUE,
          supressMessages = TRUE)
      )
    }
    idWidgetToEdit <- 2
    pathH5FolderToEdit <- file.path(pathInitial, listFolderToCreate[[idWidgetToEdit]])
    pathH5FileToEdit <- file.path(pathH5FolderToEdit, list.files(pathH5FolderToEdit))
    newValueFlow <- 15000
    myLink <- getLinks()[1]
    .h5Antares_edit_variable(
      pathH5 = pathH5FileToEdit, 
      link = myLink, 
      timeId = 1:40, 
      antVar = "FLOW LIN.", 
      newValue = newValueFlow
    )
    
    optsList <- list()
    antaresDataListH5 <- list()
    for (i in 1:length(listFolderToCreate)){
      pathOptsI <- file.path(pathInitial, listFolderToCreate[[i]])
      optsList[[i]] <- setSimulationPath(path = pathOptsI)
      antaresDataListH5[[i]] <- readAntares(links = myLink)
    }
    #test the data from h5
    #get the data from the h5 file
    antaresDataRef <- readAntares(opts = optsH5, links = myLink)
    expect_equal(max(antaresDataListH5[[idWidgetToEdit]]$`FLOW LIN.`), newValueFlow) 
    expect_equal(max(antaresDataListH5[[1]]$`FLOW LIN.`), max(antaresDataRef$`FLOW LIN.`)) 
    expect_equal(max(antaresDataListH5[[3]]$`FLOW LIN.`), max(antaresDataRef$`FLOW LIN.`)) 
    # get the data from htmlwidget 
    myArea <- "a"
    DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
    ESList <-  exchangesStack(x = optsList, interactive = FALSE, area = myArea, dateRange = DR)
    dataHtmlWidgetES2 <- .get_data_from_htmlwidget(ESList, widgetsNumber = idWidgetToEdit)
    expect_equal(dataHtmlWidgetES2$a_offshore[3], newValueFlow)
    dataHtmlWidgetES1 <- .get_data_from_htmlwidget(ESList, widgetsNumber = 1)
    expect_equal(dataHtmlWidgetES1$a_offshore[3], 0)
    expect_equal(dataHtmlWidgetES1$nega_offshore[3], 6)
    # with refStudy
    ESListRef <-  exchangesStack(x = optsList, refStudy = optsH5, interactive = FALSE, area = myArea, dateRange = DR)
    dataHtmlWidgetES2Ref <- .get_data_from_htmlwidget(ESListRef, widgetsNumber = idWidgetToEdit)
    expect_equal(dataHtmlWidgetES2Ref$a_offshore[3] - dataHtmlWidgetES1$nega_offshore[3], newValueFlow)
    dataHtmlWidgetES1Ref <- .get_data_from_htmlwidget(ESListRef, widgetsNumber = 1)
    expect_equal(dataHtmlWidgetES1Ref$a_offshore[3], 0)
    expect_equal(dataHtmlWidgetES1Ref$nega_offshore[3], 0)
  }
})

test_that("exchangesStack, interactive, x and refStudy are optsH5 ", {
  if (.requireRhdf5_Antares(stopP = FALSE)){
    skip_if_not(.runExchangesStackTest)
    suppressMessages(writeAntaresH5(pathtemp, opts = opts, overwrite = TRUE))
    optsH5 <- setSimulationPath(pathtemp)
    myArea <- "a"
    DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
    # no interactive 
    ES1 <-  exchangesStack(x = optsH5, 
                           interactive = FALSE, 
                           area = myArea, 
                           dateRange = DR,
                           mcYearh5 = 1)
    dataHtmlWidgetES1 <- .get_data_from_htmlwidget(ES1)
    timeEditValue <- "2018-04-24T23:00:00.000Z"
    indexHour <- grep(timeEditValue, dataHtmlWidgetES1$hour)
    expect_gt(indexHour, 2)
    expect_equal(dataHtmlWidgetES1$nega_offshore[indexHour], 10)
    # with interactive
    #FOR DEBUG 
    #ES1I <-  exchangesStack(x = optsH5, 
    #                       interactive = TRUE)
    
    ES1I <-  exchangesStack(x = optsH5, 
                            .runApp = FALSE, 
                            interactive = TRUE, 
                            dateRange = DR)
    ES1I <- ES1I$init()
    expect_true(is(ES1I, "MWController"))
    expect_equal(ES1I$ncharts, 1)
    expect_equal(ES1I$ncol, 1)
    expect_equal(ES1I$nrow, 1) 
    dataHtmlWidgetES1I <- .get_data_from_htmlwidget(ES1I)
    expect_equal(dataHtmlWidgetES1I$nega_offshore[indexHour], 10)
    # with refStudy no interactive
    ESRef <-  exchangesStack(x = optsH5, 
                             refStudy = optsH5, 
                             interactive = FALSE, 
                             area = myArea, 
                             dateRange = DR,
                             mcYearh5 = 1)
    dataHtmlWidgetES1 <- .get_data_from_htmlwidget(ESRef)
    expect_equal(dataHtmlWidgetES1$nega_offshore[indexHour], 0)
    # refStudy with interactive
    ESRefI <-  exchangesStack(x = optsH5, 
                              refStudy = optsH5, 
                              interactive = TRUE,
                              .runApp = FALSE, 
                              area = myArea, 
                              dateRange = DR)
    ESRefI <- ESRefI$init()
    expect_true(is(ESRefI, "MWController"))
    expect_equal(ESRefI$ncharts, 1)
    expect_equal(ESRefI$ncol, 1)
    expect_equal(ESRefI$nrow, 1) 
    dataHtmlWidgetESRefI <- .get_data_from_htmlwidget(ESRefI)
    expect_equal(dataHtmlWidgetESRefI$nega_offshore[indexHour], 0)
    # with a new Study H5 test if compare prodStack works
    ## create a new folder h5
    pathInitial <- file.path(dirname(pathtemp), basename(pathtemp))
    pathNewH5 <- file.path(pathInitial, "testH5")
    if (!dir.exists(pathNewH5)){
      dir.create(pathNewH5)
    }
    #write the study
    #windows pb ? pathNewH5 <- gsub("/", "\\", pathNewH5, fixed = TRUE)
    optsData <- antaresRead::setSimulationPath(path = studyPath)
    suppressWarnings(writeAntaresH5(path = pathNewH5, opts = optsData, 
                                    overwrite = TRUE, supressMessages = TRUE))
    
    
    pathNewH5File <- file.path(pathNewH5, list.files(pathNewH5))
    myLink <- getLinks()[1]
    .h5Antares_edit_variable(
      pathH5 = pathNewH5File, 
      link = myLink, 
      timeId = 1:40, 
      antVar = "FLOW LIN.", 
      newValue = 15000,
      mcYear = 1
    )
    
    optsH5New <- setSimulationPath(path = pathNewH5File)
    #no interactive
    ES1New <-  exchangesStack(x = optsH5New, 
                              interactive = FALSE, 
                              area = myArea, 
                              dateRange = DR,
                              mcYearh5 = 1)
    dataHtmlWidgetES1New <- .get_data_from_htmlwidget(ES1New)
    expect_equal(dataHtmlWidgetES1New$nega_offshore[indexHour], 10)
    expect_equal(dataHtmlWidgetES1New$a_offshore[2], 15000)
    # with interactive
    ES1NewI <-  exchangesStack(x = optsH5New, 
                               interactive = TRUE,
                               .runApp = FALSE,
                               area = myArea, 
                               dateRange = DR)
    ES1NewI <- ES1NewI$init()
    expect_true(is(ES1NewI, "MWController"))
    expect_equal(ES1NewI$ncharts, 1)
    expect_equal(ES1NewI$ncol, 1)
    expect_equal(ES1NewI$nrow, 1) 
    dataHtmlWidgetES1New <- .get_data_from_htmlwidget(ES1NewI)
    expect_equal(dataHtmlWidgetES1New$nega_offshore[indexHour], 10)
    expect_equal(dataHtmlWidgetES1New$a_offshore[2], 15000)
    # no interactive, refStudy,  
    ES1NewRef <-  exchangesStack(x = optsH5New, 
                                 refStudy = optsH5, 
                                 interactive = FALSE, 
                                 area = myArea, 
                                 dateRange = DR,
                                 mcYearh5 = 1)
    dataHtmlWidgetES1Ref <- .get_data_from_htmlwidget(ES1NewRef)
    expect_equal(dataHtmlWidgetES1Ref$nega_offshore[indexHour], 0)
    expect_gt(dataHtmlWidgetES1Ref$a_offshore[2], 15000)
    # interactive, refStudy,  
    ES1NewRefI <-  exchangesStack(x = optsH5New, 
                                  refStudy = optsH5, 
                                  interactive = TRUE,
                                  .runApp = FALSE,
                                  area = myArea, 
                                  dateRange = DR,
                                  mcYearh5 = 1)
    ES1NewRefI <- ES1NewRefI$init()
    expect_true(is(ES1NewRefI, "MWController"))
    expect_equal(ES1NewRefI$ncharts, 1)
    expect_equal(ES1NewRefI$ncol, 1)
    expect_equal(ES1NewRefI$nrow, 1) 
    dataHtmlWidgetES1RefI <- .get_data_from_htmlwidget(ES1NewRefI)
    expect_equal(dataHtmlWidgetES1RefI$nega_offshore[indexHour], 0)
    expect_gt(dataHtmlWidgetES1RefI$a_offshore[2], 15000)
  }
})

test_that("exchangesStack, interactive, x is a list of optsH5 and refStudy optsH5  , ", {
  if (.requireRhdf5_Antares(stopP = FALSE)){
    skip_if_not(.runExchangesStackTest)
    suppressMessages(writeAntaresH5(pathtemp, opts = opts, overwrite = TRUE))
    optsH5 <- setSimulationPath(pathtemp)
    
    # with new Studies H5 test if compare prodStack works
    ## create new folders h5
    pathInitial <- file.path(dirname(pathtemp), basename(pathtemp))
    
    listFolderToCreate <- c("testH5v2", "testH5v3", "testH5v4")
    for (folder in listFolderToCreate){
      pathNewH5 <- file.path(pathInitial, folder)
      if (!dir.exists(pathNewH5)){
        dir.create(pathNewH5)
      }
      
      #write the study
      #windows pb ? pathNewH5 <- gsub("/", "\\", pathNewH5, fixed = TRUE)
      optsData <- antaresRead::setSimulationPath(path = studyPath)
      suppressWarnings(
        writeAntaresH5(
          path = pathNewH5, 
          opts = optsData, 
          overwrite = TRUE,
          supressMessages = TRUE)
      )
    }
    pathH5FolderToEdit <- file.path(pathInitial, listFolderToCreate[[2]])
    pathH5FileToEdit <- file.path(pathH5FolderToEdit, list.files(pathH5FolderToEdit))
    myLink <- getLinks()[1]
    newValueFlow <- 50000
    mcYearToTestList <- c(2, NULL)
    for (mcYearToTest in mcYearToTestList){
      .h5Antares_edit_variable(
        pathH5 = pathH5FileToEdit, 
        link = myLink,
        timeId = 1:40, 
        antVar = "FLOW LIN.", 
        newValue = newValueFlow,
        mcYear = mcYearToTest
      )
      
      optsList <- list()
      antaresDataListH5 <- list()
      for (i in 1:length(listFolderToCreate)){
        pathOptsI <- file.path(pathInitial, listFolderToCreate[[i]])
        optsList[[i]] <- setSimulationPath(path = pathOptsI)
        antaresDataListH5[[i]] <- readAntares(links = myLink, mcYear = mcYearToTest)
      }
      
      DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
      #try without refStudy and interactive == FALSE
      myArea <- "a"
      ESListNoInt <-  exchangesStack(x = optsList,
                                     dateRange = DR,
                                     area = myArea,
                                     interactive = FALSE,
                                     mcYearh5 = mcYearToTest)
      dataHtmlWidgetESNoInt <- .get_data_from_htmlwidget(ESListNoInt, widgetsNumber = 2)
      expect_equal(max(dataHtmlWidgetESNoInt$a_offshore, na.rm = TRUE), 50000)
      
      # try with refStudy 
      ESListNoInt <-  exchangesStack(x = optsList, 
                                     refStudy = optsH5, 
                                     interactive = FALSE, 
                                     areas = myArea, 
                                     dateRange = DR,
                                     mcYearh5 = mcYearToTest)
      ## get the data from htmlwidget
      dataHtmlWidgetES1 <- .get_data_from_htmlwidget(ESListNoInt, widgetsNumber = 1)
      dataHtmlWidgetES2 <- .get_data_from_htmlwidget(ESListNoInt, widgetsNumber = 2)
      
      ## get the data from the h5 file
      antaresDataRef <- readAntares(opts = optsH5, links = myLink, mcYears = mcYearToTest)
      expect_equal(max(antaresDataListH5[[2]]$`FLOW LIN.`), newValueFlow) 
      expect_equal(max(antaresDataListH5[[1]]$`FLOW LIN.`), max(antaresDataRef$`FLOW LIN.`)) 
      expect_equal(max(antaresDataListH5[[3]]$`FLOW LIN.`), max(antaresDataRef$`FLOW LIN.`)) 
      expect_equal(antaresDataListH5[[2]]$`OV. COST`, antaresDataRef$`OV. COST`)
      
      ## compare data  
      resCompareData1_ref <- antaresProcessing::compare(x = antaresDataRef, y = antaresDataListH5[[1]])
      resCompareData2_ref <- antaresProcessing::compare(x = antaresDataRef, y = antaresDataListH5[[2]])
      expect_equal(resCompareData1_ref[timeId == timeId[40], `FLOW LIN.`], -dataHtmlWidgetES1$nega_offshore[[2]])
      expect_gt(resCompareData2_ref[timeId == timeId[40], `FLOW LIN.`], newValueFlow)
      
      # interactive == TRUE
      ## DEBUG 
      # PSWORef <-  prodStack(x = optsList,
      #                       dateRange = DR,
      #                       h5requestFiltering = list(areas = myArea, 
      #                                                 mcYears = mcYearToTest),
      #                       .runApp = FALSE,
      #                       interactive = TRUE)
      # PSWORef <- PSWORef$init()
      # ESWORef <-  exchangesStack(x = antaresDataListH5[[2]])
      # ESWORef <-  exchangesStack(x = optsList)
      # ESWORef <-  exchangesStack(x = antaresDataListH5[[2]],
      #                            dateRange = DR)
      # ESWORef <-  exchangesStack(x = optsList,
      #                            dateRange = DR)
      
      ESWORef <-  exchangesStack(
        x = optsList,
        dateRange = DR,
        .runApp = FALSE,
        interactive = TRUE,
        h5requestFiltering = list(
          areas = getAreas(select = "a"), 
          links = getLinks(areas = myArea),
          mcYears = mcYearToTest))
      ESWORef <- ESWORef$init()
      expect_true(is(ESWORef, "MWController"))
      expect_equal(ESWORef$ncharts, 3)
      expect_equal(ESWORef$ncol, 2)
      expect_equal(ESWORef$nrow, 2)    
      ## get the data from htmlwidget
      dataHtmlWidgetESWORef <- .get_data_from_htmlwidget(ESWORef, widgetsNumber = 2)
      expect_equal(dataHtmlWidgetESWORef$a_offshore[[2]], 50000)
      expect_equal(dataHtmlWidgetESWORef$nega_offshore[[2]], 0)
      dataHtmlWidgetESWORef1 <- .get_data_from_htmlwidget(ESWORef, widgetsNumber = 1)
      expect_equal(dataHtmlWidgetESWORef1$a_offshore[[2]], 0)
      expect_gt(dataHtmlWidgetESWORef1$nega_offshore[[2]], 0)
      
      # fourth, MWController with refStudy and interactive == TRUE
      ESWORefListI <-  exchangesStack(
        x = optsList,
        refStudy  = optsH5,
        dateRange = DR,
        .runApp = FALSE,
        interactive = TRUE,
        h5requestFiltering = list(
          areas = getAreas(select = "a"), 
          links = getLinks(areas = myArea),
          mcYears = mcYearToTest))
      ESWORefListI <- ESWORefListI$init()
      expect_true(is(ESWORefListI, "MWController"))
      expect_equal(ESWORefListI$ncharts, 3)
      expect_equal(ESWORefListI$ncol, 2)
      expect_equal(ESWORefListI$nrow, 2)  
      #check data from htmlwidgets
      dataHtmlWidgetES31 <- .get_data_from_htmlwidget(ESWORefListI, widgetsNumber = 2)
      expect_gt(dataHtmlWidgetES31$a_offshore[[2]], 50000)
      expect_equal(dataHtmlWidgetES31$nega_offshore[[2]], 0)
      dataHtmlWidgetES21 <- .get_data_from_htmlwidget(ESWORefListI, widgetsNumber = 1)
      expect_equal(dataHtmlWidgetES21$a_offshore[[2]], 0)
      expect_equal(dataHtmlWidgetES21$nega_offshore[[2]], 0)
      
      resOptsH5Old <- readAntares(opts = optsH5, links = myLink, showProgress = FALSE, mcYears = mcYearToTest)
      resOptsH5New <- readAntares(opts = optsList[[2]], links = myLink, showProgress = FALSE, mcYears = mcYearToTest)
      #timeId for time = "2018-04-24 00:00:00 UTC" ? timeId = 2713
      timeIdVal <- 2713
      expect_equal(resOptsH5New[timeId == timeIdVal, `FLOW LIN.`], newValueFlow)
      expect_lt(resOptsH5Old[timeId == timeIdVal, `FLOW LIN.`], 0)
      
      resCompareData <- antaresProcessing::compare(x = resOptsH5Old, y = resOptsH5New)
      expect_gt(resCompareData[timeId == timeIdVal, `FLOW LIN.`], newValueFlow)
      expect_equal(resCompareData[timeId == timeIdVal, `FLOW LIN.`], dataHtmlWidgetES31$a_offshore[[1]])
      #no change after timeID > 40
      expect_equal(resCompareData[timeId == (timeIdVal + 90), `FLOW LIN.`], dataHtmlWidgetES31$a_offshore[[50]])
      expect_equal(dataHtmlWidgetES21$a_offshore[[1]], 0)
      expect_equal(dataHtmlWidgetES21$nega_offshore[[1]], 0)
    }
  }
  
})
