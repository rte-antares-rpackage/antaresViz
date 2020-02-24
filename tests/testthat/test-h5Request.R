# context("H5 utils")
# 
# describe("h5", {
#   if(.requireRhdf5_Antares(stopP = FALSE)){
#     suppressMessages(writeAntaresH5(pathtemp, overwrite = TRUE))
#     optsH5 <- setSimulationPath(pathtemp)
#     
#     expect_true(class(.getDateRange(optsH5, "hourly")) == "Date")
#     
#     expect_true(class(.getGraphFunction("ts")) == "function")
#     
#     fid <- rhdf5::H5Fopen(optsH5$h5path)
#     expect_true(all(.getTimeStep(fid) %in% c("hourly","daily","weekly","monthly","annual")))
#     
#     expect_true(length(.getElements(optsH5, "areas", fid, "hourly")$areas) == 9)
#     expect_true(length(.getElements(optsH5, "clusters", fid, "hourly")$clusters) == 7)
#     expect_true(identical(.getClustersNames(fid, "hourly"),
#                           .getElements(optsH5, "clusters", fid, "hourly")$clusters))
#     
#     
#     
#     expect_true(length(.getVariablesH5(fid, "hourly", "areas")$areas) == 29)
#   
#     expect_true(length(.getTableInH5(fid, "hourly")) == 4)
#     
#     .tryCloseH5()
#   }
#   
# 
#   
# })
    