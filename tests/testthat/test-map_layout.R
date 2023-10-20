test_that("build objet 'mapLayout' no interactive", {
  # create study with areas/links according to geojson file test
  
  antaresEditObject::createStudy(path = tempdir(), 
              study_name = "zonal_test", 
              antares_version = "8.2.0")
  
  lapply(c("21_FR", "24_FR", "23_FR", "16_FR"), 
         antaresEditObject::createArea)
  
  antaresEditObject::createLink(from = "21_FR", 
             to = "24_FR")
  
  path_geojson_test <- system.file("mapLayout/filter_zonal.geojson", package = "antaresViz")
  obj_mapLayout <-mapLayout_no_interactive(path_geojson_file = path_geojson_test)
  
  testthat::expect_s3_class(obj_mapLayout, 'mapLayout')
  
  # opts_zonal <- setSimulationPath(file.path(tempdir(), 
  #                                           "zonal_test"))
  # 
  # runSimulation("zonal_testsim")
  # 
  # mydata <- readAntares(areas = obj$coords$area, 
  #                       links = obj$links$link, 
  #                       timeStep = "daily",
  #                       select = "nostat", 
  #                       opts = opts_zonal)
  # 
  # # viz
  # myOption <- plotMapOptions(areaChartColors = c("yellow", "violetred"), 
  #                            linkDefaultCol = "green")
  # plotMap(x = mydata, mapLayout = obj, 
  #         options = myOption)
})
