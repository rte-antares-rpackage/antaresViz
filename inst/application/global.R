#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
require(shiny)
require(antaresRead)
require(antaresViz)
require(manipulateWidget)
require(data.table)


# choose a directory
source("src/scripts/directoryInput.R")

# shared inputs
shared_prodStack <- data.frame(
  module = "prodStack", 
  panel = "prodStack", 
  input = c("dateRange", "unit"),
  type = c("dateRangeInput", "selectInput"), stringsAsFactors = FALSE)

shared_plotts <- data.frame(
  module = "plotts", 
  panel = "plotts", 
  input = "dateRange", 
  type = "dateRangeInput", stringsAsFactors = FALSE)


shared_plotMap <- data.frame(
  module = "plotMap", 
  panel = "Map", 
  input = "dateRange", 
  type = "dateRangeInput", stringsAsFactors = FALSE)

shared_exchangesStack <- data.frame(
  module = "exchangesStack", 
  panel = "exchangesStack", 
  input = c("dateRange", "unit"),
  type = c("dateRangeInput", "selectInput"), stringsAsFactors = FALSE)

shared_input <- rbind(shared_prodStack, shared_plotts, shared_plotMap, shared_exchangesStack)


build_input_data <- function(data){
  data$input_id <- paste0(data$module, "-shared_", data$input)
  data$last_update <- NA
  class(data$last_update) <- c("character")
  data <- data.table(data)
  data
}


ref_map_table <- antaresMaps::getEuropeReferenceTable()
choices_map <- c("all", ref_map_table$code)
names(choices_map) <- c("all", ref_map_table$name)

# path <- tempdir()
# 
# path <- "C:\\Users\\Datastorm\\Desktop"
# sourcedir <- system.file("testdata", package = "antaresRead")
# 
# if (sourcedir != "") {
#   if (Sys.info()['sysname'] == "Windows") {
#     untar(file.path(sourcedir, "antares-test-study.tar.gz"), exdir = path,
#           extras = "--force-local")
#   } else {
#     untar(file.path(sourcedir, "antares-test-study.tar.gz"), exdir = path)
#   }
# 
#   assign("studyPath", file.path(path, "test_case"), envir = globalenv())
#   assign("nweeks", 2, envir = globalenv())
# }
# 
# opts <- setSimulationPath("C:\\Users\\Datastorm\\Desktop\\antares\\test_case", 1)
# mydata <- readAntares(areas = "all",
#                       links = "all",
#                       timeStep = "daily",
#                       select = "nostat", mcYears = "all")
# layout <- readLayout()
# ml <- mapLayout(layout)
# save("ml", file = "ml.RDS")
#
# ml <- readRDS("C:/Users/Datastorm/Documents/git/antaresViz/ml.RDS")
#
# module_plotmap <- plotMap(mydata, ml, .runApp = FALSE)

