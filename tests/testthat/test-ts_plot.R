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

if(requireNamespace("rhdf5", quietly = TRUE)){
  rhdf5::H5close()
}
if(dir.exists(pathtemp)){
  unlink(pathtemp, recursive = TRUE)
}

