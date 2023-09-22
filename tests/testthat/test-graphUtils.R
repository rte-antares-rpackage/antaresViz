# context(".compOpts")
# 
# 
# test_that(".compOpts", {
#   
#   expect_true(.compOpts("", "cp")$ncharts == 2)
#   expect_true(.compOpts("", NULL)$ncharts == 1)
#   expect_true(.compOpts(list(), NULL)$ncharts == 1)
# })  
# 
# if(.requireRhdf5_Antares(stopP = FALSE)){
#   context(".dateRangeJoin")
#   test_that(".dateRangeJoin", {
#     dt <- list()
#     dt$x <- list(list(dateRange = as.Date(c("2010-01-01", "2010-01-10"))),
#                  list(dateRange = as.Date(c("2010-01-02", "2010-01-09"))))
#     
#     
#     
#     
#     expect_true(.dateRangeJoin(dt, "union", "min") == as.Date("2010-01-01"))
#     expect_true(.dateRangeJoin(dt, "union", "max") == as.Date("2010-01-10"))
#     expect_true(.dateRangeJoin(dt, "intersect", "max") == as.Date("2010-01-09"))
#     expect_true(.dateRangeJoin(dt, "intersect", "min") == as.Date("2010-01-02"))
#     
#     dt2 <- list()
#     dt2$x <- list(list(ar = list(dateRange = as.Date(c("2010-01-01", "2010-01-10")))),
#                   list(ar = list(dateRange = as.Date(c("2010-01-02", "2010-01-09")))))
#     
#     expect_true(.dateRangeJoin(dt2, "union", "min", "ar") == as.Date("2010-01-01"))
#     expect_true(.dateRangeJoin(dt2, "union", "max", "ar") == as.Date("2010-01-10"))
#     expect_true(.dateRangeJoin(dt2, "intersect", "max", "ar") == as.Date("2010-01-09"))
#     expect_true(.dateRangeJoin(dt2, "intersect", "min", "ar") == as.Date("2010-01-02"))
#     
#   })
#   
#   context(".loadH5Data")
#   test_that(".loadH5Data", {
#     opts <- setSimulationPath(studyPath)
#     sharerequest <- list()
#     sharerequest$mcYearh_l <- "all"
#     sharerequest$tables_l <- c("areas", "links", "clusters", "districts")
#     sharerequest$timeSteph5_l <- "hourly"
#     expect_true("antaresDataList" %in% class(.loadH5Data(sharerequest, opts)))
#     
#   })
# }
