
test_that("build objet 'mapLayout' no interactive", {
  skip_if_not_installed("antaresEditObject", 
                        minimum_version = "0.3.0")
  
 # create study ----
  # create study with areas/links according to geojson file test
  suppressWarnings(
    antaresEditObject::createStudy(path = tempdir(), 
                                   study_name = "zonal_test", 
                                   antares_version = "8.2.0")
  )
  
  lapply(c("21_FR", "24_FR", "23_FR", "16_FR"), 
         antaresEditObject::createArea)
  
  antaresEditObject::createLink(from = "21_FR", 
             to = "24_FR")
  
 # read geojson ----
  path_geojson_test <- system.file("mapLayout/filter_zonal.geojson", 
                                   package = "antaresViz")
  
  geo_file <- sf::st_read(path_geojson_test)
  
 # error case ----
    # bad areas name
  bad_area_name <- geo_file
  bad_area_name$name <- sample(c("titi", "toto"), 
                          size = length(bad_area_name$name), 
                          replace = TRUE)
  
  bad_area_name <- geojsonio::geojson_write(input = bad_area_name)
  
  testthat::expect_error(
    mapLayout_no_interactive(path_geojson_file = bad_area_name$path), 
    regexp = "study must have areas according to geojson file"
  )
  
    # bad structure geojson file
  bad_struct_file <- geo_file
  bad_struct_file <- bad_struct_file[, setdiff(names(bad_struct_file), "name")]
  
  bad_struct_file <- geojsonio::geojson_write(input = bad_struct_file)
  
  testthat::expect_error(
    mapLayout_no_interactive(path_geojson_file = bad_struct_file$path), 
    regexp = "geosjon file must have key 'name'"
  )
  
    # no "Long" "Lat" key
  bad_struct_file <- geo_file
  bad_struct_file <- bad_struct_file[, setdiff(names(bad_struct_file), 
                                               c("Lat", "Long"))]
  
  bad_struct_file <- geojsonio::geojson_write(input = bad_struct_file)
  
  testthat::expect_error(
    mapLayout_no_interactive(path_geojson_file = bad_struct_file$path), 
    regexp = "geosjon file must have key \\{'Lat;'Long'\\}"
  )
  
  # remove file
  file.remove(bad_struct_file$path)
  
  # good case ----
  # build "mapLayout" object
  obj_mapLayout <- mapLayout_no_interactive(path_geojson_file = path_geojson_test)
  
  # tests
  testthat::expect_s3_class(obj_mapLayout, 'mapLayout')
  testthat::expect_true(all(
    c("coords", "links", "map", "all_coords") %in%
                          names(obj_mapLayout)))
  
  # delete study ----
  unlink(file.path(tempdir(), "zonal_test"), recursive = TRUE)
  
  # # @examples
  #   # commented code if you want to test to plot this "mapLayout"
  # 
  # # run from ui if it don't work
  # runSimulation("zonal_testsim",
  #               path_solver = "D:/AppliRTE/bin/antares-8.2-solver.exe") 
  # 
  # # read only one simulation
  # opts_zonal <- setSimulationPath(file.path(tempdir(),
  #                                           "zonal_test"))
  # 
  # mydata <- readAntares(areas = obj_mapLayout$coords$area,
  #                       links = obj_mapLayout$links$link,
  #                       timeStep = "daily",
  #                       select = "nostat",
  #                       opts = simOptions())
  # 
  # # viz
  # myOption <- plotMapOptions(areaChartColors = c("yellow", "violetred"),
  #                            linkDefaultCol = "green")
  # plotMap(x = mydata, mapLayout = obj_mapLayout,
  #         options = myOption)
})
