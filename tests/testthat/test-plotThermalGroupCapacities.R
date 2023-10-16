context("plotThermalGroupCapacities")

test_that("plotThermalGroupCapacities", {
  opts <- setSimulationPath(studyPath)
  GG <- plotThermalGroupCapacities( data = thermalGroupCapacities(opts))

  expect_true("htmlwidget" %in% class(GG))

})

