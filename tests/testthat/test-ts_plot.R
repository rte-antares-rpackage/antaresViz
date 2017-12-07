context("tsPlot")

describe(".getTSData", {
  # Helper function that checks the structure of object returned by .getTSData
  # is ok
  check_obj <- function(x, aggregated = FALSE) {
    expect_is(x, "data.table")
    expect_true(all(c("element", "timeId", "time", "value") %in% names(x)))
  }
  
  mydata <- readAntares(timeStep = "daily", mcYears = "all", showProgress = FALSE)
  tpl <- mydata[, .(mcYear, element = area, timeId, time, value = 0)]
  
  it ("returns a table with element, timeId, time and value columns", {
    dt <- .getTSData(mydata, tpl, "LOAD", "all")
    check_obj(dt)
  })
  
  it ("can filter data by element, date, mcYear", {
    # element
    myarea <- getAreas()[1]
    dt <- .getTSData(mydata, tpl, "LOAD", elements = myarea)
    check_obj(dt)
    expect_true(all(dt$element == myarea))
    expect_equal(dt$value, mydata[area == myarea, LOAD])
    
    # date
    dateRange <- c(min(dt$time), min(dt$time))
    dt <- .getTSData(mydata, tpl, "LOAD", "all", dateRange = dateRange)
    check_obj(dt)
    expect_true(all(dt$time == dateRange[1]))
    expect_equal(dt$value, mydata[time == dateRange[1], LOAD])
    
    # mcYear
    dt <- .getTSData(mydata, tpl, "LOAD", "all", mcYear = 1)
    check_obj(dt)
    expect_true(all(dt$mcYear == 1))
    expect_equal(dt$value, mydata[mcYear == 1, LOAD])
  })
  
  it ("can aggregate data", {
    # sum
    dt <- .getTSData(mydata, tpl, "LOAD", "all", aggregate = "sum")
    check_obj(dt)
    expect_true(all(dt$element == "LOAD"))
    expectValue <- mydata[, .(LOAD = sum(LOAD)), by = .(mcYear, timeId)]
    expect_equal(dt$value, expectValue$LOAD)
    
    # mean
    dt <- .getTSData(mydata, tpl, "LOAD", "all", aggregate = "mean")
    check_obj(dt)
    expect_true(all(dt$element == "LOAD"))
    expectValue <- mydata[, .(LOAD = mean(LOAD)), by = .(mcYear, timeId)]
    expect_equal(dt$value, expectValue$LOAD)
  })
})


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
                   cdf = list(x = dta, interactive = FALSE, elements = "all", type = "cdf"),
                   heatmap = list(x = dta, interactive = FALSE, elements = "all", type = "heatmap")
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