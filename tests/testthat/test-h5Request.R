describe("h5", {
  if(requireNamespace("rhdf5")){
    writeAntaresH5()
    optsH5 <- setSimulationPath(getwd(), 1)
    
    expect_true(class(.getDateRange(optsH5, "hourly")) == "Date")
    
    expect_true(class(.getGraphFunction("ts")) == "function")
    
    fid <- rhdf5::H5Fopen(optsH5$h5path)
    expect_true(all(.getTimeStep(fid) %in% c("hourly","daily","weekly","monthly","annual")))
    
    expect_true(length(.getElements(optsH5, "areas", fid, "hourly")$areas) == 18)
    expect_true(length(.getElements(optsH5, "clusters", fid, "hourly")$clusters) == 15)
    expect_true(identical(.getClustersNames(fid, "hourly"),
                          .getElements(optsH5, "clusters", fid, "hourly")$clusters))
    
    
    
    expect_true(length(.getVariablesH5(fid, "hourly", "areas")$areas) == 29)
  
    expect_true(length(.getTableInH5(fid, "hourly")) == 3)
    
    .tryCloseH5()
  }

  
})
    