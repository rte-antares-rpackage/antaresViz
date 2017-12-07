require(shiny)
require(antaresRead)
require(antaresViz)
require(manipulateWidget)
require(data.table)


# choose a directory
source("src/scripts/directoryInput.R")

# shared inputs
.global_shared_prodStack <- data.frame(
  module = "prodStack", 
  panel = "prodStack", 
  input = c("dateRange", "unit", "mcYear", "mcYearh", "timeSteph5", "legend", "drawPoints", "stepPlot"),
  type = c("dateRangeInput", "selectInput", "selectInput", "selectInput", "selectInput", 
           "checkboxInput", "checkboxInput", "checkboxInput"), stringsAsFactors = FALSE)

.global_shared_plotts <- data.frame(
  module = "plotts", 
  panel = "tsPlot", 
  input = c("dateRange", "mcYear", "mcYearh", "timeSteph5", "legend", "drawPoints", "stepPlot"),
  type = c("dateRangeInput", "selectInput", "selectInput", "selectInput", 
           "checkboxInput", "checkboxInput", "checkboxInput"), stringsAsFactors = FALSE)


.global_shared_plotMap <- data.frame(
  module = "plotMap", 
  panel = "Map", 
  input = c("dateRange", "mcYear", "mcYearh", "timeSteph5"),
  type = c("dateRangeInput", "selectInput", "selectInput", "selectInput"), stringsAsFactors = FALSE)

.global_shared_exchangesStack <- data.frame(
  module = "exchangesStack", 
  panel = "exchangesStack", 
  input = c("dateRange", "unit", "mcYear", "mcYearh", "timeSteph5", "legend", "drawPoints", "stepPlot"),
  type = c("dateRangeInput", "selectInput", "selectInput", "selectInput", "selectInput", 
           "checkboxInput", "checkboxInput", "checkboxInput"), stringsAsFactors = FALSE)

.global_shared_input <- rbind(.global_shared_prodStack, .global_shared_plotts, .global_shared_plotMap, .global_shared_exchangesStack)


.global_build_input_data <- function(data){
  data$input_id <- paste0(data$module, "-shared_", data$input)
  data$last_update <- NA
  data$update_call <- ""
  class(data$last_update) <- c("character")
  data <- data.table(data)
  data
}

#------------
# compare
#-----------

.global_compare_prodstack <- c("mcYear", "main", "unit", "areas", "legend", 
                       "stack", "stepPlot", "drawPoints")

.global_compare_exchangesStack <- c("mcYear", "main", "unit", "area",
                            "legend", "stepPlot", "drawPoints")

.global_compare_tsPlot <- c("mcYear", "main", "variable", "type", "confInt", "elements", 
                    "aggregate", "legend", "highlight", "stepPlot", "drawPoints", "secondAxis")

.global_compare_plotMap <- c("mcYear", "type", "colAreaVar", "sizeAreaVars", "areaChartType", "showLabels",
  "popupAreaVars", "labelAreaVar","colLinkVar", "sizeLinkVar", "popupLinkVars")


#----- generate help for antaresRead function
# library(tools)
# add.html.help <- function(package, func, tempsave = paste0(getwd(), "/temp.html")) {
#   pkgRdDB = tools:::fetchRdDB(file.path(find.package(package), "help", package))
#   topics = names(pkgRdDB)
#   rdfunc <- pkgRdDB[[func]]
#   tools::Rd2HTML(pkgRdDB[[func]], out = tempsave)
# }
# add.html.help("antaresRead", "readAntares", "inst/application/www/readAntares.html")
# add.html.help("antaresRead", "removeVirtualAreas", "inst/application/www/removeVirtualAreas.html")
# add.html.help("antaresRead", "writeAntaresH5", "inst/application/www/writeAntaresH5.html")
