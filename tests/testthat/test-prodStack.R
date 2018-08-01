context("prodStack")

describe("prodStack, no interactive", {

  dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  testClass <- function(obj){
    class(obj)[1] == 'combineWidgets'
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

describe("prodStack must work with refStudy, if x and y are antaresData, ", {
  myData1 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData2 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  
  myArea <- "a"
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  PS1 <- prodStack(x = myData1, interactive = FALSE, areas = myArea, dateRange = DR)
  PS2 <- prodStack(x = myData2, interactive = FALSE, areas = myArea, dateRange = DR)
  PS3 <-  prodStack(x = myData2, refStudy  = myData1, interactive = FALSE, areas = myArea, dateRange = DR)
  
  resCompare <- antaresProcessing::compare(myData2, myData1, method = "diff")
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3)
  
  expect_true(isTRUE(max(resCompare$areas$GAS)==max(dataHtmlWidgetPS$neggas, na.rm = TRUE)))
  
  #pb timeZine local (PC, Travis, etc)
  for(i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(DR[1])-timeEditShift
    timeEditPlus <- as.Date(DR[1])+timeEditShift
    myData1$areas[ (time==timeEditMinus | time==timeEditPlus) & area==myArea, GAS := as.integer(GAS + 2500)]
  }
  #check console
  #myData1$areas[ time==DR[1] & area==myArea,]$GAS
  #myData2$areas[ time==DR[1] & area==myArea,]$GAS
  
  expect_true(isTRUE(all.equal(myData2$areas[ time==DR[1] & area==myArea,]$GAS + 2500, myData1$areas[ time==DR[1] & area==myArea,]$GAS)))
  
  PS3 <-  prodStack(x = myData2, refStudy = myData1, interactive = FALSE, areas = myArea, dateRange = DR)
  PS1 <- prodStack(x = myData1, interactive = FALSE, areas = myArea, dateRange = DR)
  PS2 <- prodStack(x = myData2, interactive = FALSE, areas = myArea, dateRange = DR)
  
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3)
  resCompare <- antaresProcessing::compare(myData1, myData2, method = "diff")
  expect_true(all.equal(resCompare$areas[ time==DR[1] & area == myArea, GAS ], -(dataHtmlWidgetPS$neggas[[2]])))
  #after DR + 5 hours (no change)
  expect_true(all.equal(0, dataHtmlWidgetPS$neggas[[20]]))
})

describe("prodStack must work with refStudy, if x is a list of antaresData and y an antaresData, ", {
  myData1 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData2 <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  myData3 <- readAntares(areas = c("a", "b", "c"), links = "all", showProgress = FALSE)
  myData4 <- readAntares(areas = c("a", "b"), links = "all", showProgress = FALSE)
  myArea <- "a"
  
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  PS1 <- prodStack(x = myData1, interactive = FALSE, areas = myArea, dateRange = DR)
  PS2 <- prodStack(x = myData2, interactive = FALSE, areas = myArea, dateRange = DR)
  PS3 <- prodStack(x = myData3, interactive = FALSE, areas = myArea, dateRange = DR)
  PS4 <- prodStack(x = myData4, interactive = FALSE, areas = myArea, dateRange = DR)
  
  myDataList <- list(myData4, myData3, myData2)
  
  PS1_list <-  prodStack(x = myDataList, refStudy = myData1, interactive = FALSE, areas = myArea, dateRange = DR)
  
  resCompare <- antaresProcessing::compare(myDataList[[3]], myData1, method = "diff")
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS1_list, widgetsNumber = 3)
  
  all.equal(max(resCompare$areas$GAS), max(dataHtmlWidgetPS$neggas, na.rm = TRUE))
  
  #pb timeZone local (PC, Travis, etc)
  for(i in 0:5){
    timeEditShift <- lubridate::hours(i)
    timeEditMinus <- as.Date(DR[1])-timeEditShift
    timeEditPlus <- as.Date(DR[1])+timeEditShift
    myData1$areas[ (time==timeEditMinus | time==timeEditPlus) & area==myArea, GAS := as.integer(GAS + 2500)]
    myData4$areas[ (time==timeEditMinus | time==timeEditPlus) & area==myArea, GAS := as.integer(GAS + 2500)]
    myData3$areas[ (time==timeEditMinus | time==timeEditPlus) & area==myArea, GAS := as.integer(GAS + 2500)]
  }
  #check console
  #myData1$areas[ time==DR[1] & area==myArea,]$GAS
  #myData3$areas[ time==DR[1] & area==myArea,]$GAS
  
  expect_true(isTRUE(all.equal(myData2$areas[ time==DR[1] & area==myArea,]$GAS + 2500, myData1$areas[ time==DR[1] & area==myArea,]$GAS)))
  
  PS1_list <-  prodStack(x = myDataList, refStudy = myData1, interactive = FALSE, areas = myArea, dateRange = DR)
  PS1 <- prodStack(x = myData1, interactive = FALSE, areas = myArea, dateRange = DR)
  PS2 <- prodStack(x = myData2, interactive = FALSE, areas = myArea, dateRange = DR)
  
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS1_list, widgetsNumber = 3)
  resCompare <- antaresProcessing::compare(myData1, myData2, method = "diff")
  expect_true(all.equal(resCompare$areas[ time==DR[1] & area == myArea, GAS ], -(dataHtmlWidgetPS$neggas[[2]])))
  #after DR + 5 hours (no edit)
  expect_true(all.equal(0, dataHtmlWidgetPS$neggas[[20]]))
  
  #no change for myData3
  resCompare3 <- antaresProcessing::compare(myData3, myData1, method = "diff")
  dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS1_list, widgetsNumber = 2)
  expect_true(all.equal(resCompare3$areas[ time==DR[1] & area == myArea, GAS], dataHtmlWidgetPS$neggas[[2]]))
  expect_true(all.equal(0, dataHtmlWidgetPS$neggas[[20]]))
  
})

describe("prodStack must work with refStudy, if x and y are optsH5, ", {
  if(requireNamespace("rhdf5", quietly = TRUE)){
    suppressMessages(writeAntaresH5(pathtemp, opts = opts, overwrite = TRUE))
    optsH5 <- setSimulationPath(pathtemp)
    
    myArea <- "b"
    DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
    PS1 <- prodStack(x = optsH5, interactive = FALSE, areas = myArea, dateRange = DR)
    PS2 <- prodStack(x = optsH5, interactive = FALSE, areas = myArea, dateRange = DR)
    PS3 <-  prodStack(x = optsH5, refStudy = optsH5, interactive = FALSE, areas = myArea, dateRange = DR)
    
    #check that PS1 == PS2 or PS3 == 0
    dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3)
    expect_equal(0, max(dataHtmlWidgetPS$totalProduction, na.rm = TRUE))
    expect_equal(0, max(dataHtmlWidgetPS$gas, na.rm = TRUE))
    
    # with a new Study H5 test if compare prodStack works
    ## create a new folder h5
    pathInitial <- file.path(dirname(pathtemp), basename(pathtemp))
    pathNewH5 <- file.path(pathInitial, "testH5")
    if(!dir.exists(pathNewH5)){
      dir.create(pathNewH5)
    }
    #write the study
    #windows pb ? pathNewH5 <- gsub("/", "\\", pathNewH5, fixed = TRUE)
    optsData <- antaresRead::setSimulationPath(path = studyPath)
    suppressWarnings(writeAntaresH5(path = pathNewH5, opts = optsData, 
                                    overwrite = TRUE))
    
    
    pathNewH5File <- file.path(pathNewH5, list.files(pathNewH5))
    .h5Antares_edit_variable(
      pathH5 = pathNewH5File, 
      area = myArea, 
      timeId = 1:40, 
      antVar = "LIGNITE", 
      newValue = 15000
    )
    
    optsH5New <- setSimulationPath(path = pathNewH5)
    PS1 <- prodStack(x = optsH5, interactive = FALSE, areas = myArea, dateRange = DR)
    PS2 <- prodStack(x = optsH5New, interactive = FALSE, areas = myArea, dateRange = DR)
    PS3 <-  prodStack(x = optsH5New, refStudy = optsH5, interactive = FALSE, areas = myArea, dateRange = DR)
    
    resOptsH5Old <- readAntares(opts = optsH5, areas = myArea, showProgress = FALSE)
    resOptsH5New <- readAntares(opts = optsH5New, areas = myArea, showProgress = FALSE)
    resOptsH5Old[timeId==timeId[40] , LIGNITE]
    resOptsH5New[timeId==timeId[40] , LIGNITE]
    expect_equal(resOptsH5New[time==DR[1] , LIGNITE], 15000)
    
    resCompareData <- antaresProcessing::compare(x= resOptsH5Old, y = resOptsH5New)
    expect_equal(resCompareData[timeId==timeId[40], LIGNITE], -24000)
    
    dataHtmlWidgetPS <- .get_data_from_htmlwidget(PS3)
    expect_equal(resCompareData[timeId==timeId[40] , LIGNITE], -dataHtmlWidgetPS$neglignite[[2]])
    #no change after timeID > 40
    expect_equal(resCompareData[timeId==timeId[90] , LIGNITE], -dataHtmlWidgetPS$neglignite[[50]])
  }
  
})

describe("prodStack must work with refStudy, if x is a list of optsH5 and y an optsH5, ", {
  if(requireNamespace("rhdf5", quietly = TRUE)){
    suppressMessages(writeAntaresH5(pathtemp, opts = opts, overwrite = TRUE))
    optsH5 <- setSimulationPath(pathtemp)
    
    # with new Studies H5 test if compare prodStack works
    ## create new folders h5
    pathInitial <- file.path(dirname(pathtemp), basename(pathtemp))
    
    listFolderToCreate <- c("testH5v2", "testH5v3", "testH5v4")
    for(folder in listFolderToCreate){
      pathNewH5 <- file.path(pathInitial, folder)
      if(!dir.exists(pathNewH5)){
        dir.create(pathNewH5)
      }
      
      #write the study
      #windows pb ? pathNewH5 <- gsub("/", "\\", pathNewH5, fixed = TRUE)
      optsData <- antaresRead::setSimulationPath(path = studyPath)
      suppressWarnings(
        writeAntaresH5(
          path = pathNewH5, 
          opts = optsData, 
          overwrite = TRUE)
      )
    }
    myArea <- "b"
    pathH5FolderToEdit <- file.path(pathInitial, listFolderToCreate[[2]])
    pathH5FileToEdit <- file.path(pathH5FolderToEdit, list.files(pathH5FolderToEdit))
    .h5Antares_edit_variable(
      pathH5 = pathH5FileToEdit, 
      area = myArea, 
      timeId = 1:40, 
      antVar = "LIGNITE", 
      newValue = 100000
    )
    
    optsList <- list()
    for(i in 1:length(listFolderToCreate)){
      pathOptsI <- file.path(pathInitial, listFolderToCreate[[i]])
      optsList[[i]] <- setSimulationPath(path = pathOptsI)
    }
    DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
    PS1 <- prodStack(x = optsH5, interactive = FALSE, areas = myArea, dateRange = DR)
    PS2 <- prodStack(x = optsList, interactive = FALSE, areas = myArea, dateRange = DR)
    PS3 <-  prodStack(x = optsList, refStudy = optsH5, interactive = FALSE, areas = myArea, dateRange = DR)
    
    #### ecrire des tests pour verifier qu un seul graphique ne change et pas les autres 
    
    
    
  }
})