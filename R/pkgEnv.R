#' @import data.table
#' @import plyr

# Private variables accessible only by functions from the package
pkgEnv <- antaresRead:::pkgEnv

# Generate the list of aliases for function productionStack()
#
# The definition of the variables used in aliases is stored in file 
# "GraphicalCharter.csv"
graphicalCharter <- fread(input=system.file("GraphicalCharter.csv", package = "antaresViz"))

formulas <- lapply(graphicalCharter$formula, function(s) parse(text = s))
names(formulas) <- graphicalCharter$name

colors <- graphicalCharter[, rgb(red, green, blue, maxColorValue = 255)]
names(colors) <- graphicalCharter$name

# Private function that generates a production stack alias, given a list of 
# variable names. The variable names need to be present in file 
# GraphicalCharter.csv
.getProdStackAlias <- function(description = "", var = NULL, lines = NULL) {
  list(
    description = description,
    variables = formulas[var],
    colors = unname(colors[var]),
    lines = formulas[lines],
    lineColors = unname(colors[lines]) 
  )
}

# List of aliases for parameter "variables" in function productionStack()
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
    var = c("pumpedStorage", "minusBalance", "bioenergie", "wind", "solar", 
            "nuclear", "hydraulic", "gas", "coal", "lignite", "fuel", "other"),
    lines = c("load", "totalProduction")
  ),
  
  thermalFirst = .getProdStackAlias(
    description = "thermal first",
    var = c("pumpedStorage", "minusBalance", "nuclear", "lignite", "coal", "gas",
            "oil", "mixFuel", "other", "bioenergie", "wind", "solar", 
            "hydraulicRor", "hydraulicStro")
  ),
  
  netLoad = .getProdStackAlias(
    description = "netLoad",
    var = c("pumpedStorage", "minusBalance", "nuclear", "lignite", "coal", "gas",
            "oil", "mixFuel", "other", "hydraulicStro"),
    lines = c("netLoad")
  ),
  
  mustRun = .getProdStackAlias(
    description = "must-run",
    var = c("pumpedStorage", "minusBalance", "mustRunTotal", "thermalDispatchable",
            "hydraulicDispatchable", "renewableNoDispatchable")
  )
)

rm(graphicalCharter, formulas, colors)