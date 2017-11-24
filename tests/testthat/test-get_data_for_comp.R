context(".getDataForComp")
# Helper function
check_structure <- function(x, y = NULL, compare = NULL, compareOpts = NULL) {
  res <- .getDataForComp(x, y, compare, compareOpts)
  expect_equal(names(res), c("x", "compare", "compareOpts"))
  
  # Check number of data objects
  if (!inherits(x, "antaresData")) N <- length(x)
  else if (!is.null(y)) N <- 2
  else if (!is.null(compareOpts) && !is.null(compareOpts$ncharts)) N <- compareOpts$ncharts
  else if (!is.null(compare)) N <- 2
  else N <- 1
  
  expect_equal(res$compareOpts$ncharts, N)
  expect_equal(length(res$x), N)
  for (o in res$x) expect_is(o, "antaresDataList")
}

adt <- readAntares(showProgress = FALSE)
adl <- readAntares("all", "all", showProgress = FALSE)

describe(".getDataForComp", {
  it("always returns the same structure whatever the input data", {
    check_structure(adt)
    check_structure(adt, compare = "test")
    check_structure(adt, compareOpts = list(ncharts = 3))
    check_structure(adt, adt)
    check_structure(list(adt, adt, adt))
    check_structure(adl)
    check_structure(adl, compare = "test")
    check_structure(adl, compareOpts = list(ncharts = 3))
    check_structure(adl, adl)
    check_structure(list(adl, adl, adl))
  })
})
