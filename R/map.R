# Copyright © 2016 RTE Réseau de transport d’électricité

#' Display results of a simulation on a map
#' 
#' This function generates an interactive map that let the user visually explore
#' the results of an Antares simulation. By default the function starts a Shiny 
#' gadget that let the user which variables to represent.
#' 
#' @param x
#'   Object of class \code{antaresDataList} created with 
#'   \code{\link[antaresRead]{readAntares}} and containing areas and links data
#' @param mapLayout
#'   Object created with function \code{\link{mapLayout}}
#' @param colAreaVar
#'   Name of a variable present in \code{x$areas}. The values of this variable
#'   are represented by the color of the areas on the map. If \code{"none"}, then
#'   the default color is used for all areas. 
#' @param sizeAreaVars
#'   Vector of variables present in \code{x$areas} to associate with the size of 
#'   areas on the map. If this parameter has length equal to 0, all areas have the
#'   same size. If it has length equal to one, then the radius of the areas change
#'   depending on the values of the variable choosen. If it has length greater than
#'   1 then areas are represented by a polar area chart where the size of each section
#'   depends on the values of each variable.
#' @param areaChartType
#'   If parameter \code{sizeAreaVars} contains multiple variables, this parameter
#'   determines the type of representation. Possible values are \code{"bar"} for
#'   bar charts, \code{"pie"} for pie charts, \code{"polar-area"} and 
#'   \code{"polar-radius"} for polar area charts where the values are represented
#'   respectively by the area or the radius of the slices.
#' @param uniqueScale
#'   If the map contains polar or bar charts, should the different variables 
#'   represented use the same scale or should each variable have its own scale ?
#'   This parameter should be TRUE only if the variables have the same unit and 
#'   are comparable : for instance production variables. 
#' @param showLabels
#'   Used only when \code{sizeAreaVars} contains multiple variables. If it is 
#'   \code{TRUE}, then values of each variable are displayed. 
#' @param popupAreaVars
#'   Vector of variables to display when user clicks on an area.
#' @param labelAreaVar
#'   Variable to display inside the areas. This parameter is used only if 
#'   parameter \code{sizeAreaVars} contains zero or one variable.
#' @param colLinkVar
#'   Name of a variable present in \code{x$links}. The values of this variable
#'   are represented by the color of the links on the map. If \code{"none"}, then
#'   the default color is used for all links  
#' @param sizeLinkVar
#'   Name of a variable present in \code{x$links}. Its values are represented by
#'   the line width of the links on the map.
#' @param popupLinkVars
#'   Vector of variables to display when user clicks on a link.
#' @param type
#'   If \code{type="avg"}, the data is averaged by area/and or link and
#'   represented on the map. If it is equal to \code{"detail"}, only one time
#'   step at a time. In interactive mode, an input control permits to choose the
#'   time step shown.
#' @param timeId
#'   A single time id present in the data. Only used if \code{type="detail"}
#' @param main
#'   Title of the map.
#' @param options
#'   List of parameters that override some default visual settings. See the
#'   help of \code{\link{plotMapOptions}}.
#' @inheritParams prodStack
#'   
#' @return 
#' An htmlwidget of class "leaflet". It can be modified with package 
#' \code{leaflet}. By default the function starts a shiny gadget that lets the
#' user play with most of the parameters of the function. The function returns
#' a leaflet map when the user clicks on the button \code{"done"}.
#' 
#' @examples 
#' \dontrun{
#' mydata <- readAntares(areas = "all", links = "all", timeStep = "daily",
#'                       select = "nostat")
#' 
#' # Place areas on a map. Ths has to be done once for a given study. Then the
#' # object returned by "mapLayout" may be saved and reloaded with
#' # functions save and load
#' 
#' layout <- readLayout()
#' ml <- mapLayout(layout)
#' save("ml", file = "ml.rda")
#' 
#' plotMap(mydata, ml)
#' 
#' # Specify the variables to use to control the color or size of elements.
#' plotMap(mydata, ml, 
#'         sizeAreaVars = c("WIND", "SOLAR", "H. ROR"),
#'         sizeLinkVar = "FLOW LIN.")
#' 
#' # Change default graphical properties
#' plotMap(mydata, ml, options = list(colArea="red", colLink = "orange"))
#' 
#' }
#' 
#' @export
plotMap <- function(x, y = NULL, mapLayout, colAreaVar = "none", sizeAreaVars = c(),
                    areaChartType = c("bar", "pie", "polar-area", "polar-radius"),
                    uniqueScale = FALSE,
                    showLabels = FALSE,
                    popupAreaVars = c(),
                    labelAreaVar = "none",
                    colLinkVar = "none", sizeLinkVar = "none", 
                    popupLinkVars = c(),
                    type = c("detail", "avg"),
                    timeId = NULL,
                    mcYear = "average",
                    main = "",
                    compare = NULL,
                    compareOpts = list(),
                    interactive = getInteractivity(),
                    options = plotMapOptions(),
                    width = NULL, height = NULL) {
  
  type <- match.arg(type)
  areaChartType <- match.arg(areaChartType)
  options <- do.call(plotMapOptions, options)
  if (is.null(mcYear)) mcYear <- "average"
  if (inherits(y, "mapLayout")) {
    mapLayout <- y
    y <- NULL
  }
  
  group <- paste0("map-group-", sample(1e9, 1))
  
  # Check that parameters have the good class
  if (!is(mapLayout, "mapLayout")) stop("Argument 'mapLayout' must be an object of class 'mapLayout' created with function 'mapLayout'.")
  
  params <- .getDataForComp(x, y, compare, compareOpts, function(x) {
    if (!is(x, "antaresData")) {
      stop("Argument 'x' must be an object of class 'antaresData' created with function 'readAntares'.")
    } else {
      x <- as.antaresDataList(x)
      if (is.null(x$areas) && is.null(x$links)) stop("Argument 'x' should contain at least area or link data.")
    }
    
    # Should parameter mcYear be shown in the UI ?
    showMcYear <- !attr(x, "synthesis") && length(unique(x[[1]]$mcYear)) > 1
    
    # Should links and/or areas be displayed ?
    areas <- !is.null(x$areas)
    links <- !is.null(x$links)
    
    # First and last time ids in data
    timeIdMin <- min(x[[1]]$timeId)
    timeIdMax <- max(x[[1]]$timeId)
    
    # Select first timeId if necessary
    if (is.null(timeId)) timeId <- timeIdMin
    
    # Keep only links and areas present in the data
    if (areas) {
      areaList <- unique(x$areas$area)
      mapLayout$coords <- mapLayout$coords[area %in% areaList]
    }
    if (links) {
      linkList <- unique(x$links$link)
      mapLayout$links <- mapLayout$links[link %in% linkList]
    }
    
    # Precompute synthetic results and set keys for fast filtering
    syntx <- synthesize(x) 
    
    oldkeys <- lapply(x, key)
    
    if (attr(x, "synthesis")) {
      mcYear <- "average"
    } else {
      if (areas) setkeyv(x$areas, "mcYear")
      if (links) setkeyv(x$links, "mcYear")
    }
    
    # Function that draws the final map when leaving the shiny gadget.
    plotFun <- function(t, colAreaVar, sizeAreaVars, popupAreaVars, areaChartType, 
                        uniqueScale, showLabels, labelAreaVar, colLinkVar, sizeLinkVar, 
                        popupLinkVars, 
                        type = c("detail", "avg"), mcYear,
                        initial = TRUE, session = NULL, outputId = "output1") {

      type <- match.arg(type)
      if (type == "avg") t <- NULL
      else if (is.null(t)) t <- 0
      
      # Prepare data
      if (mcYear == "average") x <- syntx
      
      if (initial) {
        map <- .initMap(x, mapLayout, options) %>% syncWith(group)
      } else {
        map <- leafletProxy(outputId, session)
      }
      
      map %>% 
        .redrawLinks(x, mapLayout, mcYear, t, colLinkVar, sizeLinkVar, popupLinkVars, options) %>% 
        .redrawCircles(x, mapLayout, mcYear, t, colAreaVar, sizeAreaVars, popupAreaVars, 
                       uniqueScale, showLabels, labelAreaVar, areaChartType, options)
    }
    
    # Create the interactive widget
    areaValColumns <- setdiff(names(x$areas), .idCols(x$areas))
    areaValColumnsSynt <- setdiff(names(syntx$areas), .idCols(syntx$areas))
    
    areaNumValColumns <- sapply(x$areas, is.numeric)
    areaNumValColumns <- names(areaNumValColumns)[areaNumValColumns == TRUE]
    areaNumValColumns <- intersect(areaValColumns, areaNumValColumns)
    
    linkValColums <- setdiff(names(x$links), .idCols(x$links))
    
    linkNumValColumns <- sapply(x$links, is.numeric)
    linkNumValColumns <- names(linkNumValColumns)[linkNumValColumns == TRUE]
    linkNumValColumns <- intersect(linkValColums, linkNumValColumns)
    # We don't want to show the time id slider if there is only one time id
    hideTimeIdSlider <- timeIdMin == timeIdMax
    
    list(
      plotFun = plotFun,
      x = x,
      showMcYear = showMcYear,
      areaValColumns = areaValColumns,
      areaValColumnsSynt = areaValColumnsSynt,
      areaNumValColumns = areaNumValColumns,
      linkValColums = linkValColums,
      linkNumValColumns = linkNumValColumns,
      hideTimeIdSlider = hideTimeIdSlider,
      timeId = timeId
    )
  })
  
  if (!interactive) {
    map <-  params$x[[1]]$plotFun(timeId, colAreaVar, sizeAreaVars, popupAreaVars, areaChartType,
                    uniqueScale, showLabels, labelAreaVar, colLinkVar, 
                    sizeLinkVar, popupLinkVars, type = type, mcYear = mcYear)
    return(combineWidgets(map, title = main, width = width, height = height))
  } else {
    
    manipulateWidget(
      {
        params$x[[.id]]$plotFun(params$x[[.id]]$timeId, colAreaVar, sizeAreaVars, popupAreaVars, areaChartType,
                uniqueScale, showLabels, labelAreaVar,
                colLinkVar, sizeLinkVar, popupLinkVars, type, mcYear, .initial, .session,
                .output)
      },
      
      mcYear = mwSelect(c("average", unique(x[[1]]$mcYear)), mcYear, .display = params$x[[.id]]$showMcYear),
      type = mwRadio(list("By time id"="detail", "Average" = "avg"), value = type),
      
      Areas = mwGroup(
        colAreaVar = mwSelect(
          choices = {
            if (mcYear == "average") c("none", params$x[[.id]]$areaValColumnsSynt)
            else c("none", params$x[[.id]]$areaValColumns)
          },
          value = colAreaVar, 
          label = "Color"
        ),
        sizeAreaVars = mwSelect(params$x[[.id]]$areaNumValColumns, sizeAreaVars, label = "Size", multiple = TRUE),
        areaChartType = mwSelect(list("bar chart" = "bar", 
                                      "pie chart" = "pie",
                                      "polar (area)" = "polar-area",
                                      "polar (radius)" = "polar-radius"),
                                 value = areaChartType,
                                 .display = length(sizeAreaVars) >= 2),
        uniqueScale = mwCheckbox(uniqueScale, label = "Unique scale", 
                                 .display = length(sizeAreaVars) >= 2 && areaChartType != "pie"),
        showLabels = mwCheckbox(showLabels, label = "Show labels", 
                                .display = length(sizeAreaVars) >= 2),
        popupAreaVars = mwSelect(
          choices = {
            if (mcYear == "average") c("none", params$x[[.id]]$areaValColumnsSynt)
            else c("none", params$x[[.id]]$areaValColumns)
          }, 
          popupAreaVars, 
          label = "Popup", 
          multiple = TRUE
        ),
        labelAreaVar = mwSelect(
          choices = {
            if (mcYear == "average") c("none", params$x[[.id]]$areaValColumnsSynt)
            else c("none", params$x[[.id]]$areaValColumns)
          }, 
          labelAreaVar, label = "Label", 
          .display = length(sizeAreaVars) < 2
        )
      ),
      
      Links = mwGroup(
        colLinkVar = mwSelect(c("none", params$x[[.id]]$linkValColums), colLinkVar, label = "Color"),
        sizeLinkVar = mwSelect(c("none", params$x[[.id]]$linkNumValColumns), sizeLinkVar, label = "Width"),
        popupLinkVars = mwSelect(params$x[[.id]]$linkValColums, popupLinkVars, label = "Popup", multiple = TRUE)
      ),
      .viewer = "browser",
      .width = width,
      .height = height,
      .return = function(w, e) combineWidgets(w, title = main, width = width, height = height),
      .compare = params$compare,
      .compareOpts = params$compareOpts
    )
  }
}
