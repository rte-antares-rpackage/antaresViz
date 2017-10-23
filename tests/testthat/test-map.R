context("plotMap")


describe("plotMap, no interactive", {

  dta <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  testClass <- function(obj){
    class(obj)[1] == 'combineWidgets'
  }
  load(system.file("mapLayout/ml.rda", package = "antaresViz"))

  listArgs <- list(noarg = list(x = dta, interactive = FALSE, mapLayout = ml),
                   colorLinks = list(x = dta, interactive = FALSE, mapLayout = ml, colLinkVar = "FLOW LIN."),
                   colorAll = list(x = dta, interactive = FALSE, mapLayout = ml, colLinkVar = "FLOW LIN.",
                                   colAreaVar = "OP. COST")
  )

  lapply(listArgs, function(X){
    test_that (names(listArgs), {
      re1 <- do.call(plotMap, X)
      expect_true(testClass(re1))
    })
  })

})
