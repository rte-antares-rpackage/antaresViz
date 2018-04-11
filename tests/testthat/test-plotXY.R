describe("prodStack, no interactive", {
  if(requireNamespace("rbokeh")){
    dta <- readAntares(areas = "all", showProgress = FALSE)
    g <- plotXY(dta, "NODU", "LOAD", precision = 50, sizeOnCount = FALSE)
    expect_true("htmlwidget" %in% class(g))
  }
})