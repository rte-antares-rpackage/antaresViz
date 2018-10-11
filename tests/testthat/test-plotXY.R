context("prodStack no interactive")

describe("prodStack, no interactive", {
  if(.requireRhdf5_Antares(stopP = FALSE)){
    dta <- readAntares(areas = "all", showProgress = FALSE)
    g <- plotXY(dta, "NODU", "LOAD", precision = 50, sizeOnCount = FALSE)
    expect_true("htmlwidget" %in% class(g))
  }
})
