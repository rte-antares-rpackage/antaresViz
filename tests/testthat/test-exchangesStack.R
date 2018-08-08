context("exchangesStack")

describe("no interactivy", {

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

describe("exchangesStack, no interactive", {
  dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  testClass <- function(obj){
    class(obj)[1] == 'combineWidgets'
  }
  listArgs <- list(noarg = list(x = dta, interactive = FALSE, areas = "a"),
                   allAreas = list(x = dta, interactive = FALSE, areas = "all"),
                   main = list(x = dta, interactive = FALSE, areas = "all", main = "Title"),
                   ylab = list(x = dta, interactive = FALSE, areas = "all", main = "Title", ylab = "Subt")
  )
  
  
  lapply(listArgs, function(X){
    test_that (names(listArgs), {
      re1 <- do.call(exchangesStack, X)
      expect_true(testClass(re1))
    })
  })
  
})

describe("exchangesStack, no interactive return error", {
  
  dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  
  expect_error(exchangesStack(dta, interactive = FALSE, compare = "areas"))
  
})

describe("exchangesStack, no interactive, 
         parameter refStudy must work with antaresDataTable", {
  myData1 <- readAntares(links = "all", showProgress = FALSE)
  myData2 <- readAntares(links = "all", showProgress = FALSE)
  myArea <- "a"
  DR <- c("2018-04-24 00:00:00 UTC", "2018-04-26 00:00:00 UTC")
  exS1 <-  exchangesStack(x = myData1, interactive = FALSE, areas = myArea, dateRange = DR, stepPlot = TRUE)
  dataExS1 <- .get_data_from_htmlwidget(exS1)
  indexHour <- grep("2018-04-25T00:00:00.000Z", dataExS1$hour)
  expect_gt(indexHour, 2)
  expect_equal(dataExS1$nega_offshore[indexHour], 9)
})