#Copyright © 2016 RTE Réseau de transport d’électricité

#' @import data.table
#' @import antaresRead
#' @import antaresProcessing
#' @import dygraphs
#' @import shiny
#' @import htmltools
#' @import manipulateWidget
#' @import leaflet
#' @import leaflet.minicharts
#' @import assertthat
#' @importFrom plotly plot_ly layout config add_bars add_heatmap add_text add_trace
#' @importFrom grDevices col2rgb colorRampPalette colors gray rainbow rgb
#' @importFrom graphics plot par
#' @importFrom methods is
#' @importFrom stats density quantile lm predict
#' @importFrom utils object.size
#' @importFrom stats as.formula
#' 
globalVariables(
  c("value", "element", "mcYear", "suffix", "time", "timeId", "dt", ".", 
    "x", "y", ".id", ".initial", ".session", "FLOW LIN.", "area", "direction", 
    "flow", "formulas", "link", ".output", "J", "ROW BAL.", "change", "to",
    "wdayId", "weekId")
)

.idCols <- antaresRead:::.idCols
.timeIdToDate <- antaresRead:::.timeIdToDate
.getTimeId <- antaresRead:::.getTimeId
.mergeByRef <- antaresRead:::.mergeByRef
.checkColumns <- antaresProcessing:::.checkColumns
.checkAttrs <- antaresProcessing:::.checkAttrs

DEFAULT_CAT_COLORS <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                      "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

# Private variables accessible only by functions from the package
pkgEnv <- antaresRead:::pkgEnv

.onLoad <- function(libname, pkgname) {
  setInteractivity("auto")
  options(antaresVizSizeGraph = 200)
}

# Generate the list of aliases for function prodStack()
#
# The definition of the variables used in aliases is stored in file 
# "GraphicalCharter.csv"
graphicalCharter <- fread(input=system.file("GraphicalCharter.csv", package = "antaresViz"))

formulas <- lapply(graphicalCharter$formula, function(s) parse(text = s))
names(formulas) <- graphicalCharter$name

colors <- graphicalCharter[, rgb(red, green, blue, maxColorValue = 255)]
names(colors) <- graphicalCharter$name


needed <- graphicalCharter$Needed_Col
names(needed) <- graphicalCharter$name
needed <- strsplit(needed, ",")
# Private function that generates a production stack alias, given a list of 
# variable names. The variable names need to be present in file 
# GraphicalCharter.csv
.getProdStackAlias <- function(description = "", var = NULL, lines = NULL) {
  list(
    description = description,
    nedded_col = unique(unlist(needed[var])),
    variables = formulas[var],
    colors = unname(colors[var]),
    lines = formulas[lines],
    lineColors = unname(colors[lines]) 
  )
}

# List of aliases for parameter "variables" in function prodStack()
#
# Each element has five elements:
# - description: A concise description of the production stack.
# - variables:   Definition of the variables to draw
# - colors:      Vector of colors with same length as "variables"
# - lines:       (optional) Definition of curves to draw on top of the stack
# - lineColors:  Vector of colors with same length as lines. Mandatory only if
#                "lines" is set
#
pkgEnv$prodStackAliases <- list(
  
  eco2mix = .getProdStackAlias(
    description = "Production stack used on Eco2mix website: 
    http://www.rte-france.com/fr/eco2mix/eco2mix-mix-energetique",
    var = c("pumpedStorage", "import/export", "bioenergy", "wind", "solar", 
            "nuclear", "hydraulic", "gas", "coal", "lignite", "oil", "other"),
    lines = c("load", "totalProduction")
  ),
  
  thermalFirst = .getProdStackAlias(
    description = "thermal first",
    var = c("pumpedStorage", "import/export", "nuclear", "lignite", "coal", "gas",
            "oil", "mixFuel", "misc. DTG", "bioenergy", "wind", "solar", 
            "hydraulicRor", "hydraulicStor")
  ),
  
  netLoad = .getProdStackAlias(
    description = "netLoad",
    var = c("pumpedStorage", "import/export", "nuclear", "lignite", "coal", "gas",
            "oil", "mixFuel", "misc. DTG", "hydraulicStor"),
    lines = c("netLoad")
  ),
  
  mustRun = .getProdStackAlias(
    description = "must-run",
    var = c("pumpedStorage", "import/export", "mustRunTotal", "thermalDispatchable",
            "hydraulicDispatchable", "renewableNoDispatchable")
  )
)

rm(graphicalCharter, formulas, colors)


colorsVars <- fread(input=system.file("color.csv", package = "antaresViz"))
colorsVars$colors <- rgb(colorsVars$red, colorsVars$green, colorsVars$blue, maxColorValue = 255)


# message limit size
antaresVizSizeGraphError = "Too much data, please reduce selection. If you work with hourly data, you can reduce dateRange selection. 
You can also use 'limitSizeGraph' function in R or 'Memory Controls' panel in shiny to update this."