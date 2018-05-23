context(".getTSData")

describe(".getTSData", {
  # Helper function that checks the structure of object returned by .getTSData
  # is ok
  check_obj <- function(x, aggregated = FALSE) {
    expect_is(x, "data.table")
    expect_true(all(c("element", "timeId", "time", "value") %in% names(x)))
  }
  
  mydata <- readAntares(timeStep = "daily", mcYears = "all", showProgress = FALSE)
  tpl <- mydata[, .(mcYear, element = area, timeId, time, value = 0)]
  
  mydata_av <- readAntares(timeStep = "daily", mcYears = NULL, showProgress = FALSE)
  tpl_av <- mydata_av[, .(element = area, timeId, time, value = 0)]
  
  it ("returns a table with element, timeId, time and value columns", {
    dt <- .getTSData(mydata, tpl, "LOAD", "all")
    check_obj(dt)
  })
  
  it ("can filter data by element, date, mcYear", {
    # element
    myarea <- getAreas()[1]
    dt <- .getTSData(mydata_av, tpl_av, "LOAD", elements = myarea)
    check_obj(dt)
    expect_true(all(dt$element == myarea))
    expect_equal(dt$value, mydata_av[area == myarea, LOAD])
    
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
    dt <- .getTSData(mydata_av, tpl_av, "LOAD", "all", aggregate = "sum")
    check_obj(dt)
    expect_true(all(dt$element == "LOAD"))
    expectValue <- mydata_av[, .(LOAD = sum(LOAD)), by = .(timeId)]
    expect_equal(dt$value, expectValue$LOAD)
    
    # mean
    dt <- .getTSData(mydata_av, tpl_av, "LOAD", "all", aggregate = "mean")
    check_obj(dt)
    expect_true(all(dt$element == "LOAD"))
    expectValue <- mydata_av[, .(LOAD = mean(LOAD)), by = .(timeId)]
    expect_equal(dt$value, expectValue$LOAD)
  })
})