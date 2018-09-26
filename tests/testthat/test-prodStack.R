context("prodStack")

describe("prodStack, no interactive", {
  dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  testClass <- function(obj){
    class(obj)[1] == "combineWidgets"
  }
  listArgs <- list(noarg = list(x = dta, interactive = FALSE, areas = "a"),
                   areas2 = list(x = dta, interactive = FALSE, areas = c("a", "b"))
  )
  
  lapply(listArgs, function(X){
    test_that (names(listArgs), {
      re1 <- do.call(prodStack, X)
      expect_true(testClass(re1))
    })
  })
  
})

describe("prodStack, no interactive return error", {
  
  dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  expect_error(prodStack(dta, interactive = FALSE, compare = "areas"))
  
})

describe("prodStack, interactive", {
  dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  VV <- prodStack(dta, interactive = FALSE)
  expect_true("htmlwidget" %in% class(VV))
})

describe("prodStack must work with refStudy, if x and refStudy are antaresDataList, ", {
  myData1 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData2 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  
  myArea <- "a"
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  PS3 <-  prodStack(x = myData2, refStudy  = myData1, interactive = FALSE, areas = myArea, dateRange = DR)
  
  resCompare <- antaresProcessing::compare(myData2, myData1, method = "diff")
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3)
  
  expect_true(isTRUE(max(resCompare$areas$GAS) == max(dataHtmlWidgetPS$neggas, na.rm = TRUE)))
  
  #pb timeZine local (PC, Travis, etc)
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(DR[1]) - timeEditShift
    timeEditPlus <- as.Date(DR[1]) + timeEditShift
    myData1$areas[ (time == timeEditMinus | time == timeEditPlus) & area == myArea, GAS := as.integer(GAS + 2500)]
  }
  #check console
  #myData1$areas[ time == DR[1] & area == myArea, ]$GAS
  #myData2$areas[ time == DR[1] & area == myArea, ]$GAS
  
  expect_true(isTRUE(all.equal(myData2$areas[ time == DR[1] & area == myArea, ]$GAS + 2500, myData1$areas[ time == DR[1] & area == myArea, ]$GAS)))
  
  PS3 <-  prodStack(x = myData2, refStudy  = myData1, interactive = FALSE, areas = myArea, dateRange = DR)
  
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3)
  resCompare <- antaresProcessing::compare(myData1, myData2, method = "diff")
  expect_true(all.equal(resCompare$areas[ time == DR[1] & area == myArea, GAS ], - (dataHtmlWidgetPS$neggas[[2]])))
  #after DR + 5 hours (no change)
  expect_true(all.equal(0, dataHtmlWidgetPS$neggas[[20]]))
})

describe("prodStack must work with refStudy, if x is a list of antaresDataList and refStudy an antaresDataList, ", {
  myData1 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData2 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData3 <- readAntares(areas = c("a", "b", "c"), links = "all", showProgress = FALSE)
  myData4 <- readAntares(areas = c("a", "b"), links = "all", showProgress = FALSE)
  myArea <- "a"
  myDataList <- list(myData4, myData3, myData2)
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  PS1_list <-  prodStack(x = myDataList, refStudy = myData1, interactive = FALSE, areas = myArea, dateRange = DR)
  resCompare <- antaresProcessing::compare(myDataList[[3]], myData1, method = "diff")
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS1_list, widgetsNumber = 3)
  all.equal(max(resCompare$areas$GAS), max(dataHtmlWidgetPS$neggas, na.rm = TRUE))
  #pb timeZone local (PC, Travis, etc)
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(DR[1]) - timeEditShift
    timeEditPlus <- as.Date(DR[1]) + timeEditShift
    myData1$areas[ (time == timeEditMinus | time == timeEditPlus) & area == myArea, GAS := as.integer(GAS + 2500)]
    myData4$areas[ (time == timeEditMinus | time == timeEditPlus) & area == myArea, GAS := as.integer(GAS + 2500)]
    myData3$areas[ (time == timeEditMinus | time == timeEditPlus) & area == myArea, GAS := as.integer(GAS + 2500)]
  }
  #check console
  #myData1$areas[ time == DR[1] & area == myArea, ]$GAS
  #myData3$areas[ time == DR[1] & area == myArea, ]$GAS
  
  expect_true(isTRUE(all.equal(myData2$areas[ time == DR[1] & area == myArea, ]$GAS + 2500, myData1$areas[ time == DR[1] & area == myArea, ]$GAS)))
  
  PS1_list <-  prodStack(x = myDataList, refStudy = myData1, interactive = FALSE, areas = myArea, dateRange = DR)
  
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS1_list, widgetsNumber = 3)
  resCompare <- antaresProcessing::compare(myData1, myData2, method = "diff")
  expect_true(all.equal(resCompare$areas[ time == DR[1] & area == myArea, GAS ], - (dataHtmlWidgetPS$neggas[[2]])))
  #after DR + 5 hours (no edit)
  expect_true(all.equal(0, dataHtmlWidgetPS$neggas[[20]]))
  
  #no change for myData3
  resCompare3 <- antaresProcessing::compare(myData3, myData1, method = "diff")
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS1_list, widgetsNumber = 2)
  expect_true(all.equal(resCompare3$areas[ time == DR[1] & area == myArea, GAS], dataHtmlWidgetPS$neggas[[2]]))
  expect_true(all.equal(0, dataHtmlWidgetPS$neggas[[20]]))
  
})

describe("prodStack must work with refStudy, if x and refStudy are optsH5, ", {
  if (.requireRhdf5_Antares(stopP = FALSE)){
    skip_if_not(.runProdStackTest)
    suppressMessages(writeAntaresH5(pathtemp, opts = opts, overwrite = TRUE))
    optsH5 <- setSimulationPath(pathtemp)
    
    myArea <- "b"
    DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
    PS3 <-  prodStack(x = optsH5, 
                      refStudy = optsH5, 
                      interactive = FALSE, 
                      areas = myArea, 
                      dateRange = DR)
    
    #check that PS1 == PS2 or PS3 == 0
    dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3)
    expect_equal(0, max(dataHtmlWidgetPS$totalProduction, na.rm = TRUE))
    expect_equal(0, max(dataHtmlWidgetPS$gas, na.rm = TRUE))
    
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
    .h5Antares_edit_variable(
      pathH5 = pathNewH5File, 
      area = myArea, 
      timeId = 1:40, 
      antVar = "LIGNITE", 
      newValue = 15000
    )
    
    optsH5New <- setSimulationPath(path = pathNewH5)
    PS3 <-  prodStack(x = optsH5New, refStudy = optsH5, interactive = FALSE, areas = myArea, dateRange = DR)
    
    resOptsH5Old <- readAntares(opts = optsH5, areas = myArea, showProgress = FALSE)
    resOptsH5New <- readAntares(opts = optsH5New, areas = myArea, showProgress = FALSE)
    expect_equal(resOptsH5New[time == DR[1], LIGNITE], 15000)
    
    resCompareData <- antaresProcessing::compare(x = resOptsH5Old, y = resOptsH5New)
    expect_equal(resCompareData[timeId == timeId[40], LIGNITE], -24000)
    
    dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3)
    expect_equal(resCompareData[timeId == timeId[40], LIGNITE], -dataHtmlWidgetPS$neglignite[[2]])
    #no change after timeID > 40
    expect_equal(resCompareData[timeId == timeId[90], LIGNITE], -dataHtmlWidgetPS$neglignite[[50]])
  }
  
})

describe("prodStack must work with refStudy, if x is a list of optsH5 and refStudy an optsH5, ", {
  if (.requireRhdf5_Antares(stopP = FALSE)){
    skip_if_not(.runProdStackTest)
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
    myArea <- "b"
    pathH5FolderToEdit <- file.path(pathInitial, listFolderToCreate[[2]])
    pathH5FileToEdit <- file.path(pathH5FolderToEdit, list.files(pathH5FolderToEdit))
    newValueLignite <- 100000
    .h5Antares_edit_variable(
      pathH5 = pathH5FileToEdit, 
      area = myArea, 
      timeId = 1:40, 
      antVar = "LIGNITE", 
      newValue = newValueLignite
    )
    
    optsList <- list()
    antaresDataListH5 <- list()
    for (i in 1:length(listFolderToCreate)){
      pathOptsI <- file.path(pathInitial, listFolderToCreate[[i]])
      optsList[[i]] <- setSimulationPath(path = pathOptsI)
      antaresDataListH5[[i]] <- readAntares(areas = myArea)
    }
    DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
    PS1 <- prodStack(x = optsH5, interactive = FALSE, areas = myArea, dateRange = DR)
    PS2 <- prodStack(x = optsList, interactive = FALSE, areas = myArea, dateRange = DR)
    PS_List <-  prodStack(x = optsList, refStudy = optsH5, interactive = FALSE, areas = myArea, dateRange = DR)
    #get the data from the h5 file
    antaresDataRef <- readAntares(opts = optsH5, areas = myArea)
    expect_equal(max(antaresDataListH5[[2]]$LIGNITE), newValueLignite) 
    expect_equal(max(antaresDataListH5[[1]]$LIGNITE), max(antaresDataRef$LIGNITE)) 
    expect_equal(max(antaresDataListH5[[3]]$LIGNITE), max(antaresDataRef$LIGNITE)) 
    #get the data from htmlwidget
    dataHtmlWidgetPS1 <- .get_data_from_htmlwidget(PS_List, widgetsNumber = 1)
    dataHtmlWidgetPS2 <- .get_data_from_htmlwidget(PS_List, widgetsNumber = 2)
    #compare data  
    resCompareData1_ref <- antaresProcessing::compare(x = antaresDataRef, y = antaresDataListH5[[1]])
    resCompareData2_ref <- antaresProcessing::compare(x = antaresDataRef, y = antaresDataListH5[[2]])
    expect_equal(resCompareData1_ref[timeId == timeId[40], LIGNITE], -dataHtmlWidgetPS1$lignite[[2]])
    expect_equal(resCompareData2_ref[timeId == timeId[40], LIGNITE], dataHtmlWidgetPS2$lignite[[2]])
    #no change after timeID > 40
    expect_equal(resCompareData1_ref[timeId == timeId[90], LIGNITE], -dataHtmlWidgetPS1$lignite[[50]])
    expect_equal(resCompareData2_ref[timeId == timeId[90], LIGNITE], -dataHtmlWidgetPS2$lignite[[50]])
    
  }
})

describe("prodStack must work with refStudy, if interactive is set to TRUE and if x and refStudy are antaresData, ", {
  myData1 <- readAntares(areas = "all", showProgress = FALSE)
  myData2 <- readAntares(areas = "all", showProgress = FALSE)
  
  myArea <- "a"
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  
  #for debug, dont compare
  # PS3 <-  prodStack(
  #   x = myData2, 
  #   dateRange = DR, 
  #   .runApp = FALSE, 
  #   interactive = TRUE,
  #   h5requestFiltering = list(areas = myArea))
  # res <- PS3$init()
  # PS3 
  # #for debug, refStudy but not interactive 
  # PS3 <-  prodStack(
  #   x = myData2, 
  #   refStudy = myData1,
  #   dateRange = DR, 
  #   .runApp = FALSE, 
  #   interactive = FALSE,
  #   areas = myArea)
  # PS3
  
  #MWController 
  PS3 <-  prodStack(
    x = myData2, 
    refStudy  = myData1, 
    dateRange = DR, 
    .runApp = FALSE, 
    interactive = TRUE,
    h5requestFiltering = list(areas = myArea))
  res <- PS3$init()
  expect_true(is(PS3, "MWController"))
  expect_equal(PS3$ncharts, 1)
  expect_equal(PS3$ncol, 1)
  expect_equal(PS3$nrow, 1)
  
  #get the data from antaresData
  resCompare <- antaresProcessing::compare(myData2, myData1, method = "diff")
  
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3)
  expect_true(isTRUE(max(resCompare$GAS) == max(dataHtmlWidgetPS$neggas, na.rm = TRUE)))
  
  #pb timeZine local (PC, Travis, etc)
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(DR[1]) - timeEditShift
    timeEditPlus <- as.Date(DR[1]) + timeEditShift
    myData1[ (time == timeEditMinus | time == timeEditPlus) & area == myArea, GAS := as.integer(GAS + 2500)]
  }
  #check console
  #myData1[ time == DR[1] & area == myArea, ]$GAS
  #myData2[ time == DR[1] & area == myArea, ]$GAS
  
  expect_true(isTRUE(all.equal(myData2[ time == DR[1] & area == myArea, ]$GAS + 2500, myData1[ time == DR[1] & area == myArea, ]$GAS)))
  
  PS3 <-  prodStack(x = myData2, refStudy = myData1, areas = myArea, dateRange = DR, .runApp = FALSE, interactive = TRUE)
  expect_true(is(PS3, "MWController"))
  expect_equal(PS3$ncharts, 1)
  expect_equal(PS3$ncol, 1)
  expect_equal(PS3$nrow, 1)
  
  resCompare <- antaresProcessing::compare(myData1, myData2, method = "diff")
  
  PS3$init()
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3)
  expect_true(all.equal(resCompare[ time == DR[1] & area == myArea, GAS ], - (dataHtmlWidgetPS$neggas[[2]])))
  #after DR + 5 hours (no change)
  expect_true(all.equal(0, dataHtmlWidgetPS$neggas[[20]]))   
  
})

describe("prodStack must work with refStudy, if interactive is set to TRUE and if x a list of antaresData and refStudy an antaresData, ", {
  myData1 <- readAntares(areas = "all", showProgress = FALSE)
  myData2 <- readAntares(areas = "all", showProgress = FALSE)
  myData3 <- readAntares(areas = "all", showProgress = FALSE)
  myData4 <- readAntares(areas = "all", showProgress = FALSE)
  
  myDataList <- list(myData2, myData3, myData4)
  
  myArea <- "a"
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  #MWController 
  PS3 <-  prodStack(x = myDataList, refStudy  = myData1, areas = myArea, dateRange = DR, .runApp = FALSE, interactive = TRUE)
  #PS3 <-  prodStack(x = myData2, refStudy = myData1, areas = myArea, dateRange = DR)
  res <- PS3$init()
  expect_true(is(PS3, "MWController"))
  expect_equal(PS3$ncharts, 3)
  expect_equal(PS3$ncol, 2)
  expect_equal(PS3$nrow, 2)
  
  #get the data from antaresData
  resCompare3_1 <- antaresProcessing::compare(myDataList[[2]], myData1, method = "diff")
  
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3, widgetsNumber = 2)
  expect_true(isTRUE(max(resCompare3_1$GAS) == max(dataHtmlWidgetPS$neggas, na.rm = TRUE)))
  
  #pb timeZine local (PC, Travis, etc)
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(DR[1]) - timeEditShift
    timeEditPlus <- as.Date(DR[1]) + timeEditShift
    myData3[ (time == timeEditMinus | time == timeEditPlus) & area == myArea, GAS := as.integer(GAS + 2500)]
  }
  #check console
  #myData1[ time == DR[1] & area == myArea, ]$GAS
  #myData2[ time == DR[1] & area == myArea, ]$GAS
  
  expect_true(isTRUE(all.equal(myData3[ time == DR[1] & area == myArea, ]$GAS, myData1[ time == DR[1] & area == myArea, ]$GAS + 2500)))
  
  PS3 <-  prodStack(x = myDataList, refStudy = myData1, areas = myArea, dateRange = DR, .runApp = FALSE, interactive = TRUE)
  
  expect_true(is(PS3, "MWController"))
  expect_equal(PS3$ncharts, 3)
  expect_equal(PS3$ncol, 2)
  expect_equal(PS3$nrow, 2)
  
  resCompare3_1 <- antaresProcessing::compare(myDataList[[2]], myData2, method = "diff")
  
  PS3$init()
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3, widgetsNumber = 2)
  expect_true(all.equal(resCompare3_1[ time == DR[1] & area == myArea, GAS ], - (dataHtmlWidgetPS$gas[[2]])))
  #after DR + 5 hours (no change)
  expect_true(all.equal(0, dataHtmlWidgetPS$neggas[[20]]))   
  
})

describe("prodStack must work with refStudy, if interactive is set to TRUE and if x is an antaresDataList and refStudy an antaresDataList, ", {
  myData1 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData2 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  
  myArea <- "a"
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  #MWController 
  PS3 <-  prodStack(x = myData2, refStudy  = myData1, areas = myArea, dateRange = DR, .runApp = FALSE, interactive = TRUE)
  res <- PS3$init()
  expect_true(is(PS3, "MWController"))
  expect_equal(PS3$ncharts, 1)
  expect_equal(PS3$ncol, 1)
  expect_equal(PS3$nrow, 1)
  
  #get the data from antaresData
  resCompare2_1 <- antaresProcessing::compare(myData2, myData1, method = "diff")
  
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3, widgetsNumber = 1)
  expect_true(isTRUE(max(resCompare2_1$areas$GAS) == max(dataHtmlWidgetPS$neggas, na.rm = TRUE)))
  
  #pb timeZine local (PC, Travis, etc)
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(DR[1]) - timeEditShift
    timeEditPlus <- as.Date(DR[1]) + timeEditShift
    myData2$areas[ (time == timeEditMinus | time == timeEditPlus) & area == myArea, GAS := as.integer(GAS + 2500)]
  }
  #check console
  #myData1[ time == DR[1] & area == myArea, ]$GAS
  #myData2[ time == DR[1] & area == myArea, ]$GAS
  
  expect_true(isTRUE(all.equal(myData2$areas[ time == DR[1] & area == myArea, ]$GAS, myData1$areas[ time == DR[1] & area == myArea, ]$GAS + 2500)))
  
  PS3 <-  prodStack(x = myData2, refStudy = myData1, areas = myArea, dateRange = DR, .runApp = FALSE, interactive = TRUE)
  
  expect_true(is(PS3, "MWController"))
  expect_equal(PS3$ncharts, 1)
  expect_equal(PS3$ncol, 1)
  expect_equal(PS3$nrow, 1)
  
  resCompare2_1 <- antaresProcessing::compare(myData1, myData2, method = "diff")
  
  PS3$init()
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3, widgetsNumber = 1)
  expect_true(all.equal(resCompare2_1$areas[ time == DR[1] & area == myArea, GAS ], (dataHtmlWidgetPS$gas[[2]])))
  #after DR + 5 hours (no change)
  expect_true(all.equal(0, dataHtmlWidgetPS$neggas[[20]]))   
  
})

describe("prodStack must work with refStudy, if interactive is set to TRUE and if x is a list of antaresDataList and refStudy an antaresDataList , ", {
  myData1 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData2 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData3 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData4 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  
  myDataList <- list(myData4, myData3, myData2)
  
  myArea <- "a"
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  #MWController 
  PS3 <-  prodStack(x = myDataList, refStudy  = myData1, areas = myArea, dateRange = DR, .runApp = FALSE, interactive = TRUE)
  res <- PS3$init()
  expect_true(is(PS3, "MWController"))
  expect_equal(PS3$ncharts, 3)
  expect_equal(PS3$ncol, 2)
  expect_equal(PS3$nrow, 2)
  
  #get the data from antaresData
  resCompare2_1 <- antaresProcessing::compare(myDataList[[3]], myData1, method = "diff")
  
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3, widgetsNumber = 3)
  expect_true(isTRUE(max(resCompare2_1$areas$GAS) == max(dataHtmlWidgetPS$neggas, na.rm = TRUE)))
  
  #pb timeZine local (PC, Travis, etc)
  for (i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(DR[1]) - timeEditShift
    timeEditPlus <- as.Date(DR[1]) + timeEditShift
    myData2$areas[ (time == timeEditMinus | time == timeEditPlus) & area == myArea, GAS := as.integer(GAS + 2500)]
  }
  #check console
  #myData1[ time == DR[1] & area == myArea, ]$GAS
  #myData2[ time == DR[1] & area == myArea, ]$GAS
  
  expect_true(isTRUE(all.equal(myData2$areas[ time == DR[1] & area == myArea, ]$GAS, myData1$areas[ time == DR[1] & area == myArea, ]$GAS + 2500)))
  
  PS3 <-  prodStack(x = myDataList, refStudy = myData1, areas = myArea, dateRange = DR, .runApp = FALSE, interactive = TRUE)
  
  expect_true(is(PS3, "MWController"))
  expect_equal(PS3$ncharts, 3)
  expect_equal(PS3$ncol, 2)
  expect_equal(PS3$nrow, 2)
  
  resCompare2_1 <- antaresProcessing::compare(myData1, myData2, method = "diff")
  resCompare3_1 <- antaresProcessing::compare(myData1, myData3, method = "diff")
  
  PS3$init()
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3, widgetsNumber = 3)
  expect_true(all.equal(resCompare2_1$areas[ time == DR[1] & area == myArea, GAS ], (dataHtmlWidgetPS$gas[[2]])))
  #no change for myData3
  dataHtmlWidgetPS3 <- .get_data_from_htmlwidget(PS3, widgetsNumber = 2)
  expect_true(all.equal(resCompare3_1$areas[ time == DR[1] & area == myArea, GAS ], (dataHtmlWidgetPS3$gas[[2]])))
  #after DR + 5 hours (no change)
  expect_true(all.equal(0, dataHtmlWidgetPS$neggas[[20]]))   
  
})

describe("prodStack must work with refStudy, if interactive is set to TRUE and if x, refStudy are optsH5 , ", {
  if (.requireRhdf5_Antares(stopP = FALSE)){
    skip_if_not(.runProdStackTest)
    suppressMessages(writeAntaresH5(pathtemp, opts = opts, overwrite = TRUE))
    optsH5 <- setSimulationPath(pathtemp)
    
    myArea <- "b"
    DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
    #MWController 
    # test debug prodStack(x = optsH5, refStudy  = optsH5, dateRange = DR, h5requestFiltering = list(areas = myArea, mcYears = 2))
    PS3 <-  prodStack(x = optsH5, 
                      refStudy  = optsH5, 
                      dateRange = DR, 
                      h5requestFiltering = list(areas = myArea, mcYears = 2),
                      .runApp = FALSE, 
                      interactive = TRUE)
    
    res <- PS3$init()
    expect_true(is(PS3, "MWController"))
    expect_equal(PS3$ncharts, 1)
    expect_equal(PS3$ncol, 1)
    expect_equal(PS3$nrow, 1)
    
    #check that PS1 == PS2 or PS3 == 0
    dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3)
    expect_equal(0, max(dataHtmlWidgetPS$totalProduction, na.rm = TRUE))
    expect_equal(0, max(dataHtmlWidgetPS$gas, na.rm = TRUE))
    
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
    .h5Antares_edit_variable(
      pathH5 = pathNewH5File, 
      area = myArea, 
      timeId = 1:40, 
      antVar = "LIGNITE", 
      newValue = 15000,
      mcYear = 2
    )
    
    optsH5New <- setSimulationPath(path = pathNewH5File)
    PS3 <-  prodStack(x = optsH5New, 
                      refStudy  = optsH5, 
                      dateRange = DR, 
                      h5requestFiltering = list(areas = myArea, mcYears = 2),
                      .runApp = FALSE, 
                      interactive = TRUE)
    res <- PS3$init()
    
    #TEST non interactive for debug 
    PS_FInt <-  prodStack(x = optsH5New, 
                          refStudy = optsH5, 
                          mcYearh5 = 2,
                          interactive = FALSE, 
                          areas = myArea, 
                          dateRange = DR)
    dataHtmlWidgetPSFint <- .get_data_from_htmlwidget(PS_FInt)
    expect_equal(-23000, min(dataHtmlWidgetPSFint$totalProduction, na.rm = TRUE))
    expect_equal(0, max(dataHtmlWidgetPSFint$neggas, na.rm = TRUE))
    expect_equal(23000, max(dataHtmlWidgetPSFint$neglignite, na.rm = TRUE))
    
    res <- PS3$init()
    expect_true(is(PS3, "MWController"))
    expect_equal(PS3$ncharts, 1)
    expect_equal(PS3$ncol, 1)
    expect_equal(PS3$nrow, 1)
    
    resOptsH5Old <- readAntares(opts = optsH5, areas = myArea, showProgress = FALSE, mcYears = 2)
    resOptsH5New <- readAntares(opts = optsH5New, areas = myArea, showProgress = FALSE, mcYears = 2)
    #timeId for time = "2018-04-24 00:00:00 UTC" ? timeId = 2713
    timeIdVal <- 2713
    expect_equal(resOptsH5New[timeId == timeIdVal, LIGNITE], 15000)
    expect_equal(resOptsH5Old[timeId == timeIdVal, LIGNITE], 38000)
    
    resCompareData <- antaresProcessing::compare(x = resOptsH5Old, y = resOptsH5New)
    expect_equal(resCompareData[timeId == timeIdVal, LIGNITE], -23000)
    
    dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3)
    expect_equal(resCompareData[timeId == timeIdVal, LIGNITE], -dataHtmlWidgetPS$neglignite[[1]])
    #no change after timeID > 40
    expect_equal(resCompareData[timeId == (timeIdVal + 90), LIGNITE], -dataHtmlWidgetPS$neglignite[[50]])
  }
  
})

describe("prodStack must work with refStudy, if interactive is set to TRUE and if x is a list of optsH5 and refStudy optsH5  , ", {
  if (.requireRhdf5_Antares(stopP = FALSE)){
    skip_if_not(.runProdStackTest)
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
    myArea <- "b"
    pathH5FolderToEdit <- file.path(pathInitial, listFolderToCreate[[2]])
    pathH5FileToEdit <- file.path(pathH5FolderToEdit, list.files(pathH5FolderToEdit))
    newValueGAS <- 50000
    mcYearToTestList <- c(2, NULL)
    for (mcYearToTest in mcYearToTestList){
      .h5Antares_edit_variable(
        pathH5 = pathH5FileToEdit, 
        area = myArea, 
        timeId = 1:40, 
        antVar = "GAS", 
        newValue = newValueGAS,
        mcYear = mcYearToTest
      )
      
      optsList <- list()
      antaresDataListH5 <- list()
      for (i in 1:length(listFolderToCreate)){
        pathOptsI <- file.path(pathInitial, listFolderToCreate[[i]])
        optsList[[i]] <- setSimulationPath(path = pathOptsI)
        antaresDataListH5[[i]] <- readAntares(areas = myArea, mcYear = mcYearToTest)
      }
      
      DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
      #first, try with interactive == FALSE
      PSListNoInt <-  prodStack(x = optsList, 
                                refStudy = optsH5, 
                                interactive = FALSE, 
                                areas = myArea, 
                                dateRange = DR,
                                mcYearh5 = mcYearToTest)
      
      #get the data from the h5 file
      antaresDataRef <- readAntares(opts = optsH5, areas = myArea, mcYears = mcYearToTest)
      expect_equal(max(antaresDataListH5[[2]]$GAS), newValueGAS) 
      expect_equal(max(antaresDataListH5[[1]]$GAS), max(antaresDataRef$GAS)) 
      expect_equal(max(antaresDataListH5[[3]]$GAS), max(antaresDataRef$GAS)) 
      expect_equal(antaresDataListH5[[2]]$`OV. COST`, antaresDataRef$`OV. COST`)
      
      #get the data from htmlwidget
      dataHtmlWidgetPS1 <- .get_data_from_htmlwidget(PSListNoInt, widgetsNumber = 1)
      dataHtmlWidgetPS2 <- .get_data_from_htmlwidget(PSListNoInt, widgetsNumber = 2)
      #compare data  
      resCompareData1_ref <- antaresProcessing::compare(x = antaresDataRef, y = antaresDataListH5[[1]])
      resCompareData2_ref <- antaresProcessing::compare(x = antaresDataRef, y = antaresDataListH5[[2]])
      expect_equal(resCompareData1_ref[timeId == timeId[40], GAS], -dataHtmlWidgetPS1$gas[[2]])
      expect_equal(resCompareData2_ref[timeId == timeId[40], GAS], dataHtmlWidgetPS2$gas[[2]])
      
      #second, try without refStudy and interactive == FALSE
      PSListNoInt <-  prodStack(x = optsList,
                                dateRange = DR,
                                areas = myArea,
                                interactive = FALSE,
                                mcYearh5 = mcYearToTest)
      
      dataHtmlWidgetPSNoInt <- .get_data_from_htmlwidget(PSListNoInt, widgetsNumber = 2)
      expect_gt(max(dataHtmlWidgetPSNoInt$totalProduction, na.rm = TRUE), 100000)
      expect_equal(max(dataHtmlWidgetPSNoInt$gas, na.rm = TRUE), 50000)
      
      #thirdly, try without refStudy and interactive == TRUE
      PSWORef <-  prodStack(x = optsList,
                            dateRange = DR,
                            h5requestFiltering = list(areas = myArea, 
                                                      mcYears = mcYearToTest),
                            .runApp = FALSE,
                            interactive = TRUE)
      
      res <- PSWORef$init()
      expect_true(is(PSWORef, "MWController"))
      expect_equal(PSWORef$ncharts, 3)
      expect_equal(PSWORef$ncol, 2)
      expect_equal(PSWORef$nrow, 2)    
      
      #fourth, MWController with refStudy and interactive == TRUE
      # test debug prodStack(x = optsH5, refStudy  = optsH5, dateRange = DR, h5requestFiltering = list(areas = myArea, mcYears = 2))
      PSWRefI <-  prodStack(x = optsList,
                            refStudy  = optsH5,
                            dateRange = DR,
                            h5requestFiltering = list(areas = myArea,
                                                      mcYears = mcYearToTest),
                            .runApp = FALSE,
                            interactive = TRUE)
      
      res <- PSWRefI$init()
      expect_true(is(PSWRefI, "MWController"))
      expect_equal(PSWRefI$ncharts, 3)
      expect_equal(PSWRefI$ncol, 2)
      expect_equal(PSWRefI$nrow, 2)
      
      #check that PS1 == PS2 or PSWRefI == 0
      dataHtmlWidgetPS31 <- .get_data_from_htmlwidget(PSWRefI, widgetsNumber = 2)
      expect_equal(newValueGAS, max(dataHtmlWidgetPS31$totalProduction, na.rm = TRUE))
      expect_equal(newValueGAS, max(dataHtmlWidgetPS31$gas, na.rm = TRUE))
      
      dataHtmlWidgetPS21 <- .get_data_from_htmlwidget(PSWRefI, widgetsNumber = 1)
      expect_equal(0, max(dataHtmlWidgetPS21$totalProduction, na.rm = TRUE))
      expect_equal(0, max(dataHtmlWidgetPS21$gas, na.rm = TRUE))
      
      resOptsH5Old <- readAntares(opts = optsH5, areas = myArea, showProgress = FALSE, mcYears = mcYearToTest)
      resOptsH5New <- readAntares(opts = optsList[[2]], areas = myArea, showProgress = FALSE, mcYears = mcYearToTest)
      #timeId for time = "2018-04-24 00:00:00 UTC" ? timeId = 2713
      timeIdVal <- 2713
      expect_equal(resOptsH5New[timeId == timeIdVal, GAS], newValueGAS)
      expect_equal(resOptsH5Old[timeId == timeIdVal, GAS], 0)
      
      resCompareData <- antaresProcessing::compare(x = resOptsH5Old, y = resOptsH5New)
      expect_equal(resCompareData[timeId == timeIdVal, GAS], newValueGAS)
      
      expect_equal(resCompareData[timeId == timeIdVal, GAS], dataHtmlWidgetPS31$gas[[1]])
      #no change after timeID > 40
      expect_equal(resCompareData[timeId == (timeIdVal + 90), GAS], dataHtmlWidgetPS31$gas[[50]])
      expect_equal(0, dataHtmlWidgetPS21$gas[[1]])
      expect_equal(0, dataHtmlWidgetPS21$gas[[50]])
    }
  }
  
})

describe("prodStack, no interactive, ne error with compare main", {
  myData <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myApplica <- prodStack(x = myData, 
                         interactive = TRUE, 
                         compare = "main",
                         .runApp = FALSE)
  myApplica$init()
  expect_true(is(myApplica, "MWController"))
})
