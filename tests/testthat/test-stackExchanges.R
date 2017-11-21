context("stackExchanges")


describe("stackExchanges, no interactive", {
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

describe("stackExchanges, no interactive return error", {
  
  dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  
  expect_error(exchangesStack(dta, interactive = FALSE, compare = "areas"))
  
})