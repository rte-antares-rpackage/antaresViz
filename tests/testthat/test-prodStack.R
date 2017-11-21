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