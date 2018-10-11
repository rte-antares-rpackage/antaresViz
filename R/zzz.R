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
#' @importFrom graphics plot.default par
#' @importFrom methods is
#' @importFrom stats density quantile lm predict
#' @importFrom utils object.size capture.output
#' @importFrom stats as.formula
#' @importFrom geojsonio geojson_json
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
.requireRhdf5_Antares <- antaresRead:::.requireRhdf5_Antares
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
graphicalCharter <- fread(input = system.file("GraphicalCharter.csv", package = "antaresViz"))

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
    lineColors = unname(colors[lines]),
    lineWidth = 2
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


# message limit size
antaresVizSizeGraphError = "Too much data, please reduce selection. If you work with hourly data, you can reduce dateRange selection. 
You can also use 'limitSizeGraph' function in R or 'Memory Controls' panel in shiny (if present) to update this."

antaresVizSizeGraphError_fr = "Trop de donn\u00e9es,veuillez r\u00e9duire votre s\u00e9lection. Si vous travaillez en donn\u00e9es horaire, vous pouvez r\u00e9duire la p\u00e9riode de visualisation. 
Il est \u00e9galement possible d'utiliser la fonction 'limitSizeGraph' en R ou l'onglet 'Memory Controls' dans shiny (si pr\u00e9sent) pour changer la limite."

# language for labels
language_labels <- fread(input = system.file("language_labels.csv", package = "antaresViz"), encoding = "UTF-8")

availableLanguages_labels <- colnames(language_labels)

.getLabelLanguage <- function(label, language = "en"){
  if (language %in% colnames(language_labels)){
    up_label <- language_labels[get("en") %in% label, get(language)]
    if (length(up_label) == 0){
      up_label <- label
    } else {
      # in case of double
      up_label <- up_label[1]
    }
  } else {
    up_label <- label
  }
  up_label
}

# language for columns
language_columns <- fread(input = system.file("language_columns.csv", package = "antaresViz"), encoding = "UTF-8")

language_columns$en <- as.character(language_columns$en)

language_columns$fr <- as.character(language_columns$fr)
# Encoding(language_columns$fr) <- "latin1"

expand_language_columns <- copy(language_columns)

# add _std _min _max
language_columns[, tmp_row := 1:nrow(language_columns)]

language_columns <- language_columns[, list(en = c(en, paste0(en, c("_std", "_min", "_max"))),
                        fr = c(fr, paste0(fr, c("_std", "_min", "_max")))), by = tmp_row]

language_columns[, tmp_row := NULL]


.getColumnsLanguage <- function(columns, language = "en"){
  if (language %in% colnames(language_columns)){
    ind_match <- match(columns, language_columns$en)
    up_columns <- columns
    if (any(!is.na(ind_match))){
      up_columns[which(!is.na(ind_match))] <- language_columns[[language]][ind_match[!is.na(ind_match)]]
    }
  } else {
    up_columns <- columns
  }
  up_columns
}

# map color
colorsVars <- fread(input = system.file("color.csv", package = "antaresViz"))
colorsVars <- unique(colorsVars, by = "Column")
colorsVars$colors <- rgb(colorsVars$red, colorsVars$green, colorsVars$blue, maxColorValue = 255)

# expand to fr name
expand_language_columns <- expand_language_columns[get("en") %in% colorsVars$Column]

ind_match <- match(expand_language_columns$en, colorsVars$Column)
rev_ind_match <- match(colorsVars$Column, expand_language_columns$en)

col_fr <- colorsVars[Column %in% expand_language_columns$en][, Column := expand_language_columns$fr[rev_ind_match[!is.na(rev_ind_match)]]]
colorsVars <- unique(rbindlist(list(colorsVars, col_fr)))


.check_if_is_html_cont <- function(htmlWidget = NULL){
  if (!("htmlwidget" %in% class(htmlWidget) | "MWController" %in% class(htmlWidget))){
    stop("no htmlwidget or no MWController")
  }
}

# for test
# get the data from an htmlwidget
# the first element is x and the following are y
#' get data from htmlwidget
#' 
#' @param htmlwidget an htmlwidget
#' @param widgetsNumber htmlwidget id number in the list
#' 
#' @noRd
.get_data_from_htmlwidget <- function(htmlwidget = NULL, widgetsNumber = NULL){
  .check_if_is_html_cont(htmlwidget)
  
  if (is(htmlwidget, "MWController") & !is.null(htmlwidget$charts)){
    if (length(htmlwidget$charts) == 1){
      widgetsNumber <- 1
    }
  }else{
    if (!is.null(htmlwidget$widgets)){
      if (length(htmlwidget$widgets) == 1){
        widgetsNumber <- 1
      }
    }
  }

  if (is.null(widgetsNumber)){
    stop("no widgetsNumber")
  }
  
  #check if data exist ====
  if (is(htmlwidget, "MWController")){
    if (is.null(htmlwidget$charts[[widgetsNumber]]$widgets[[1]])){
      stop("no data")
    }
    if (is.null(htmlwidget$charts[[widgetsNumber]]$widgets[[1]]$x)){
      stop("no data")
    }    
    if (is.null(htmlwidget$charts[[widgetsNumber]]$widgets[[1]]$x$attrs$labels)){
      stop("no data")
    }    
  }else{
    if (is.null(htmlwidget$widgets[[widgetsNumber]]$widgets[[1]])){
      stop("no data")
    }
    if (is.null(htmlwidget$widgets[[widgetsNumber]]$widgets[[1]]$x)){
      stop("no data")
    }
    if (is.null(htmlwidget$widgets[[widgetsNumber]]$widgets[[1]]$x$attrs$labels)){
      stop("no data")
    }    
  }
  
  #get the data =====
  resList <- list()
  if (is(htmlwidget, "MWController")){
    # htmlwidget$charts and no htmlwidget$widgets
    for (i in 1:length(htmlwidget$charts[[widgetsNumber]]$widgets[[1]]$x$attrs$labels)){
      myLabelI <- htmlwidget$charts[[widgetsNumber]]$widgets[[1]]$x$attrs$labels[[i]]
      resList[[myLabelI]] <- htmlwidget$charts[[widgetsNumber]]$widgets[[1]]$x$data[[i]]
    }
  }else{
    for (i in 1:length(htmlwidget$widgets[[widgetsNumber]]$widgets[[1]]$x$attrs$labels)){
      myLabelI <- htmlwidget$widgets[[widgetsNumber]]$widgets[[1]]$x$attrs$labels[[i]]
      resList[[myLabelI]] <- htmlwidget$widgets[[widgetsNumber]]$widgets[[1]]$x$data[[i]]
    }
  }
  
  return(resList)
}

#' edit h5 file for TEST 
#' currently only for hourly data and areas
#' 
#' @param pathH5 path H5 file
#' @param area character
#' @param timeId timeId to change
#' @param antVar antares Variable to change
#' @param newValue the newValue
#' 
#' @noRd
.h5Antares_edit_variable <- function(pathH5 = NULL, area = NULL, timeId = 1, antVar = NULL, newValue = NULL, mcYear = NULL, link = NULL){
  
  if (!is.null(area) & !is.null(link)){
    stop("area and link must not be set together")
  }
  
  if (!is.null(area)){
    categoryVar <- "areas"
  }else{
    categoryVar <- "links"
  }
  
  if (is.null(mcYear)){
    typeOfData <- "/mcAll"
  }else{
    typeOfData <- "/mcInd"
  }
  timeStepType <- paste("/hourly", categoryVar, sep = "/") 
  nameStructure <- paste0(timeStepType, typeOfData, "/structure")
  
  H5locAntaresh5 <- rhdf5::H5Fopen(name = pathH5)
  hourlyDataStructure <- rhdf5::h5read(H5locAntaresh5, name = nameStructure)
  
  if (!is.null(area)){
    indexCateroryInstance <- grep(area, hourlyDataStructure$area)[1]
  }else{
    indexCateroryInstance <- grep(link, hourlyDataStructure$link)[1]
  }
  
  indexAntVar <- grep(antVar, hourlyDataStructure$variable)[1]
  indexTimeId <- timeId
  if (is.null(mcYear)){
    indexMcYear <- 1
  }else{
    indexMcYear <- grep(mcYear, hourlyDataStructure$mcYear)[1]
  }
  
  listIndex <- list(indexTimeId, indexAntVar, indexCateroryInstance, indexMcYear)
  #debug print(listIndex)
  
  hourlyData <- rhdf5::h5read(
    H5locAntaresh5, 
    name = paste0(timeStepType, typeOfData, "/data"),
    index = listIndex)
  
  hourlyData[,,,] <- newValue
  
  rhdf5::h5writeDataset.array(
    obj = hourlyData, 
    h5loc = H5locAntaresh5, 
    name = paste0(timeStepType, typeOfData, "/data"),
    index = listIndex
  )
  
  rhdf5::H5Fclose(h5file = H5locAntaresh5)
  rhdf5::h5closeAll()
}
