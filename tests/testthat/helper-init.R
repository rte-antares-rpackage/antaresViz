#Copyright © 2016 RTE Réseau de transport d’électricité

# Copy the test study in a temporary folder

## force tests to be executed if in dev release which we define as
## having a sub-release, eg 0.9.15.5 is one whereas 0.9.16 is not
if (length(strsplit(packageDescription("antaresViz")$Version, "\\.")[[1]]) > 3) { 
  Sys.setenv("RunAllAntaresVizTests"="yes")
}
.runThisTest <- FALSE
.runThisTest <- Sys.getenv("RunAllAntaresVizTests") == "yes"

if(.runThisTest){
  .runProdStackTest <- TRUE
  .runExchangesStackTest <- TRUE
  .runTsPlotTest <- TRUE
  #bug when executing in the Test environment, so keep 
  # .runPlotMapTest to FALSE
  .runPlotMapTest <- FALSE
}else{
  .runProdStackTest <- FALSE
  .runExchangesStackTest <- FALSE  
  .runTsPlotTest <- FALSE
  .runPlotMapTest <- FALSE
}

path <- tempdir()

sourcedir <- system.file("inst/testdata", package = "antaresRead")
if (sourcedir == ""){
  sourcedir <- system.file("testdata", package = "antaresRead")
}


# Hack: For some unknown reason, this script is executed at some point of
# the R CMD CHECK before package is correctly installed and tests actually run.
# The following "if" prevents errors at this step
if (sourcedir != "") {
  
  ar_path_study <- file.path(sourcedir, "antares-test-study.tar.gz")
  if (!file.exists(ar_path_study)) {
    ar_path_study <- file.path(sourcedir, "antares-test-study-latest.tar.gz")
  }
  
  # if (Sys.info()["sysname"] == "Windows") {
  #   untar(
  #     tarfile = ar_path_study,
  #     exdir = path,
  #     extras = "--force-local"
  #   )
  # } else {
    untar(
      tarfile = ar_path_study,
      exdir = path
    )
  # }
  assign("studyPath", file.path(path, "test_case"), envir = globalenv())
  assign("nweeks", 2, envir = globalenv())
  assign("pathtemp", path, envir = globalenv())
}

opts <- setSimulationPath(get("studyPath", envir = globalenv()))
