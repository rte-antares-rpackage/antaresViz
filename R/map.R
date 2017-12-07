# Copyright © 2016 RTE Réseau de transport d’électricité

#' Display results of a simulation on a map
#' 
#' This function generates an interactive map that let the user visually explore
#' the results of an Antares simulation. By default the function starts a Shiny 
#' gadget that let the user which variables to represent.
#' 
#' @param x
#'   Object of class \code{antaresDataList} created with 
#'   \code{\link[antaresRead]{readAntares}} and containing areas and links data.
#'    It can be a list of \code{antaresData} objects. 
#'    In this case, one chart is created for each object.
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
#' @param sizeMiniPlot \code{boolean} variable size for miniplot
#' @inheritParams prodStack
#'   
#'   
#' @details 
#' 
#' compare argument can take following values :
#' \itemize{
#'    \item "mcYear"
#'    \item "type"
#'    \item "colAreaVar"
#'    \item "sizeAreaVars"
#'    \item "areaChartType"
#'    \item "showLabels"
#'    \item "popupAreaVars"
#'    \item "labelAreaVar"
#'    \item "colLinkVar"
#'    \item "sizeLinkVar"
#'    \item "popupLinkVars"
#'    }
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
#' ml <- mapLayout(layout = layout)
#' save("ml", file = "ml.rda")
#' 
#' plotMap(x = mydata, mapLayout = ml)
#' 
#' # Specify the variables to use to control the color or size of elements.
#' plotMap(mydata, mapLayout =  ml, 
#'         sizeAreaVars = c("WIND", "SOLAR", "H. ROR"),
#'         sizeLinkVar = "FLOW LIN.")
#' 
#' # Change default graphical properties
#' plotMap(x = mydata, mapLayout = ml, options = list(colArea="red", colLink = "orange"))
#' plotMap(x = list(mydata, mydata), mapLayout =  ml)
#' 
#' # Use h5 for dynamic request / exploration in a study
#' # Set path of simulaiton
#' setSimulationPath(path = path1)
#' 
#' # Convert your study in h5 format
#' writeAntaresH5(path = mynewpath)
#' 
#' # Redefine sim path with h5 file
#' opts <- setSimulationPath(path = mynewpath)
#' plotMap(x = opts, mapLayout = ml)
#' 
#' # Compare elements in a single study
#' plotMap(x = opts, mapLayout = ml,  .compare = "mcYear")
#' 
#' # Compare 2 studies
#' plotMap(x = list(opts, opts2), mapLayout = ml)
#' 
#' }
#' 
#' @export
plotMap <- function(x, mapLayout, colAreaVar = "none", sizeAreaVars = c(),
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
                    width = NULL, height = NULL, dateRange = NULL, xyCompare = c("union","intersect"),
                    h5requestFiltering = list(),
                    timeSteph5 = "hourly",
                    mcYearh5 = NULL,
                    tablesh5 = c("areas", "links"),
                    sizeMiniPlot = FALSE,...) {
  
  
  if(!is.null(compare) && !interactive){
    stop("You can't use compare in no interactive mode")
  }
  
  Column <- optionsT <- NULL
  tpMap <- plotMapOptions()
  
  #Check compare
  .validCompare(compare,  c("mcYear", "type", "colAreaVar", "sizeAreaVars", "areaChartType", "showLabels",
                            "popupAreaVars", "labelAreaVar","colLinkVar", "sizeLinkVar", "popupLinkVars"))
  
  runScale <- ifelse(!identical(options[names(options)!="preprocess"] ,
                                tpMap[names(tpMap)!="preprocess"]), FALSE, TRUE)
  
  type <- match.arg(type)
  areaChartType <- match.arg(areaChartType)
  xyCompare <- match.arg(xyCompare)
  
  if(colAreaVar != "none" & colAreaVar%in%colorsVars$Column & runScale)
  {
  raw <- colorsVars[Column == colAreaVar]
  options <- plotMapOptions(areaColorScaleOpts = colorScaleOptions(
    negCol = "#FFFFFF",
    zeroCol = rgb(raw$red, raw$green, raw$blue,  maxColorValue = 255),
    posCol = rgb(raw$red/2, raw$green/2, raw$blue/2, maxColorValue = 255)))
  
  }
  if (is.null(mcYear)) mcYear <- "average"
  
  if(!is.null(compare) && "list" %in% class(x)){
    if(length(x) == 1) x <- list(x[[1]], x[[1]])
  }
  if(!is.null(compare) && ("antaresData" %in% class(x)  | "simOptions" %in% class(x))){
    x <- list(x, x)
  }
  # .testXclassAndInteractive(x, interactive)
  
  h5requestFiltering <- .convertH5Filtering(h5requestFiltering = h5requestFiltering, x = x)
  
  
  compareOptions <- .compOpts(x, compare)
  if(is.null(compare)){
    if(compareOptions$ncharts > 1){
      compare <- ""
    }
  }
  
  group <- paste0("map-group-", sample(1e9, 1))
  
  # Check that parameters have the good class
  if (!is(mapLayout, "mapLayout")) stop("Argument 'mapLayout' must be an object of class 'mapLayout' created with function 'mapLayout'.")
  
  init_dateRange <- dateRange
  
  # new_env for save and control mapLayout
  env_plotFun <- new.env()

  processFun <- function(x, mapLayout) {
    if (!is(x, "antaresData")) {
      stop("Argument 'x' must be an object of class 'antaresData' created with function 'readAntares'.")
    } else {
      x <- as.antaresDataList(x)
      if(!is.null(x$areas)){
        if(nrow(x$areas) == 0){
          x$areas <- NULL
        }
      }
      if(!is.null(x$links)){
        if(nrow(x$links) == 0){
          x$links <- NULL
        }
      }
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
      
      if(mcYear != "average"){
        .printWarningMcYear()
      }
      
      mcYear <- "average"
    } else {
      if (areas) setkeyv(x$areas, "mcYear")
      if (links) setkeyv(x$links, "mcYear")
    }
    
    opts <- simOptions(x)
    if(!is.null(x$areas)){
      x$areas[,time := .timeIdToDate(x$areas$timeId, attr(x, "timeStep"), opts)]
    }
    
    if(!is.null(x$links)){
      x$links[,time := .timeIdToDate(x$links$timeId, attr(x, "timeStep"), opts)]
    }
    
    if(is.null(init_dateRange)){
      if(!is.null(x$areas)){
        init_dateRange <- range(as.Date(x$areas$time))
      }else{
        init_dateRange <- range(as.Date(x$links$time))
      }
    }
    
    # Function that draws the final map when leaving the shiny gadget.
    plotFun <- function(t, colAreaVar, sizeAreaVars, popupAreaVars, areaChartType, 
                        uniqueScale, showLabels, labelAreaVar, colLinkVar, sizeLinkVar, 
                        popupLinkVars, 
                        type = c("detail", "avg"), mcYear,
                        initial = TRUE, session = NULL, outputId = "output1",
                        dateRange = NULL, sizeMiniPlot = FALSE, options = NULL) {
      type <- match.arg(type)
      if (type == "avg") t <- NULL
      else if (is.null(t)) t <- 0
      
      # Prepare data
      if (mcYear == "average") x <- syntx
      
      # print("dateRange")
      # print(dateRange)
      if(!is.null(dateRange)){
        dateRange <- sort(dateRange)
        # xx <<- copy(x$areas)
        # dd <<- dateRange
        if(!is.null(x$areas))
        {
          # in case of missing transformation...
          if("character" %in% class(x$areas$time)){
            x$areas[,time := .timeIdToDate(x$areas$timeId, attr(x, "timeStep"), simOptions(x))]
          }
          x$areas  <- x$areas[time >= as.POSIXlt(dateRange[1], tz = "UTC") & time < as.POSIXlt(dateRange[2] + 1, tz = "UTC")]
        }
        if(!is.null(x$links))
        {
          # in case of missing transformation...
          if("character" %in% class(x$links$time)){
            x$links[,time := .timeIdToDate(x$links$timeId, attr(x, "timeStep"), simOptions(x))]
          }
          x$links <- x$links[time >= as.POSIXlt(dateRange[1], tz = "UTC") & time < as.POSIXlt(dateRange[2] + 1, tz = "UTC")]
        }
      }
      
      if (initial) {
        assign("currentMapLayout", mapLayout, envir = env_plotFun)
        map <- .initMap(x, mapLayout, options) %>% syncWith(group)
      } else if(!isTRUE(all.equal(mapLayout, get("currentMapLayout", envir = env_plotFun)))){
        assign("currentMapLayout", mapLayout)
        map <- .initMap(x, mapLayout, options) %>% syncWith(group)
      } else {
        # in some case, map doesn't existed yet....!
        if("output_1_zoom" %in% names(session$input)){
          map <- leafletProxy(outputId, session)
        } else {
          map <- .initMap(x, mapLayout, options) %>% syncWith(group)
        }
      }
      map %>% 
        .redrawLinks(x, mapLayout, mcYear, t, colLinkVar, sizeLinkVar, popupLinkVars, options) %>% 
        .redrawCircles(x, mapLayout, mcYear, t, colAreaVar, sizeAreaVars, popupAreaVars, 
                       uniqueScale, showLabels, labelAreaVar, areaChartType, options, sizeMiniPlot)
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
      timeId = timeId,
      dateRange = init_dateRange
    )
  }
  
  if (!interactive) {
    x <- .cleanH5(x, timeSteph5, mcYearh5, tablesh5, h5requestFiltering)
    
    
    params <- .getDataForComp(.giveListFormat(x), NULL, compare, compareOpts, processFun = processFun, mapLayout = mapLayout)
    L_w <- lapply(params$x, function(X){
      X$plotFun(t = timeId, colAreaVar = colAreaVar, sizeAreaVars = sizeAreaVars,
                popupAreaVars = popupAreaVars, areaChartType = areaChartType,
                uniqueScale = uniqueScale, showLabels = showLabels,
                labelAreaVar = labelAreaVar, colLinkVar = colLinkVar, 
                sizeLinkVar = sizeLinkVar, popupLinkVars = popupLinkVars,
                type = type, mcYear = mcYear, dateRange = dateRange,
                sizeMiniPlot = sizeMiniPlot, options = options)
    })
    return(combineWidgets(list = L_w,  title = main, width = width, height = height))  
    
    
  }
  
  ##remove notes
  mcYearH5 <- NULL
  paramsH5 <- NULL
  sharerequest <- NULL
  timeStepdataload <- NULL
  timeSteph5 <- NULL
  x_in <- NULL
  x_tranform <- NULL
  
  manipulateWidget(
    {
      if(!is.null(params))
      {
        if(.id <= length(params$x)){
          .tryCloseH5()
          
          tmp_options <- optionsT
          if(is.null(tmp_options)){
            tmp_options <-  plotMapOptions()
          }
            
          params$x[[.id]]$plotFun(t = params$x[[.id]]$timeId,
                                  colAreaVar = colAreaVar,
                                  sizeAreaVars = sizeAreaVars,
                                  popupAreaVars = popupAreaVars,
                                  areaChartType = areaChartType,
                                  uniqueScale = uniqueScale,
                                  showLabels = showLabels,
                                  labelAreaVar = labelAreaVar,
                                  colLinkVar = colLinkVar,
                                  sizeLinkVar = sizeLinkVar, 
                                  popupLinkVars = popupLinkVars,
                                  type = type,
                                  mcYear = mcYear,
                                  initial = .initial,
                                  session = .session,
                                  outputId = .output,
                                  dateRange = dateRange,
                                  sizeMiniPlot = sizeMiniPlot,
                                  options = tmp_options)
          
          
        } else {
          combineWidgets("No data for this selection")
        }
      }else{
        combineWidgets()
      }
    },
    
    x = mwSharedValue({x}),
    x_in = mwSharedValue({
      .giveListFormat(x)
    }),
    options = mwSharedValue({options}),
    optionsT = mwSharedValue({
      if(colAreaVar %in% colorsVars$Column & runScale){
        raw <- colorsVars[Column == colAreaVar]
        plotMapOptions(areaColorScaleOpts = colorScaleOptions(
          negCol = "#FFFFFF",
          zeroCol = rgb(raw$red, raw$green, raw$blue,  maxColorValue = 255),
          posCol = rgb(raw$red/2, raw$green/2, raw$blue/2, maxColorValue = 255))
        )
      }else{
        options
      }
    }),
    h5requestFiltering = mwSharedValue({h5requestFiltering}),
    
    paramsH5 = mwSharedValue({
      paramsH5List <- .h5ParamList(X_I = x_in, xyCompare = xyCompare, h5requestFilter = h5requestFiltering)
      rhdf5::H5close()
      paramsH5List
    }),
    H5request = mwGroup(
      timeSteph5 = mwSelect(choices = paramsH5$timeStepS, 
                            value =  paramsH5$timeStepS[1], 
                            label = "timeStep", 
                            multiple = FALSE),
      tables = mwSelect(choices = paramsH5[["tabl"]][paramsH5[["tabl"]] %in% c("areas", "links")], 
                        value = {
                          if(.initial) {paramsH5[["tabl"]][paramsH5[["tabl"]] %in% c("areas", "links")]} else {NULL}
                        }, 
                        label = "table", multiple = TRUE),
      mcYearH5 = mwSelect(choices = c(paramsH5[["mcYearS"]]), 
                         value = {
                           if(.initial){paramsH5[["mcYearS"]][1]}else{NULL}
                         }, 
                         label = "mcYear", multiple = TRUE),
      .display = {any(unlist(lapply(x_in, .isSimOpts)))}
    ),
    sharerequest = mwSharedValue({
      list(timeSteph5_l = timeSteph5, mcYearh_l = mcYearH5, tables_l = tables)
    }),
    x_tranform = mwSharedValue({
      sapply(1:length(x_in),function(zz){
        .loadH5Data(sharerequest, x_in[[zz]], h5requestFilter = paramsH5$h5requestFilter[[zz]])
      }, simplify = FALSE)
    }),
    
    ##Stop h5
    mcYear = mwSelect({
      c("average", as.character(.compareOperation(lapply(params$x, function(vv){
        unique(vv$x[[1]]$mcYear)
      }), xyCompare)))
    }, 
    value = { if(.initial) mcYear else NULL}, 
    .display = any(unlist(lapply(params$x, function(X){X$showMcYear})))
    ),
    type = mwRadio(list("By time id"="detail", "Average" = "avg"), value = type),
    dateRange = mwDateRange(
      value = {
        if(.initial) params$x[[1]]$dateRange
        else NULL
      },
      min = params$x[[1]]$dateRange[1], 
      max = params$x[[1]]$dateRange[2],label = "Daterange"
    ),
    
    Areas = mwGroup(
      colAreaVar = mwSelect(
        choices = {
          if (mcYear == "average") {
            c("none",
              as.character(.compareOperation(lapply(params$x, function(vv){
                unique(vv$areaValColumnsSynt)
              }), xyCompare))
            )
          }else{
            c("none", as.character(.compareOperation(lapply(params$x, function(vv){
              unique(vv$areaValColumns)
            }), xyCompare)))
          }
        },
        value = {
          if(.initial) colAreaVar
          else NULL
        },
        label = "Color"
      ),
      sizeAreaVars = mwSelect(
        {
          as.character(.compareOperation(lapply(params$x, function(vv){
            unique(vv$areaNumValColumns)
          }), xyCompare))
        }, 
        value = {
          if(.initial) sizeAreaVars
          else NULL
        }, label = "Size", multiple = TRUE),
      miniPlot = mwGroup(
        areaChartType = mwSelect(list("bar chart" = "bar", 
                                      "pie chart" = "pie",
                                      "polar (area)" = "polar-area",
                                      "polar (radius)" = "polar-radius"),
                                 value = {
                                   if(.initial) areaChartType
                                   else NULL
                                 }),
                                 sizeMiniPlot = mwCheckbox(FALSE),
                                 .display = length(sizeAreaVars) >= 2),
        uniqueScale = mwCheckbox(uniqueScale, label = "Unique scale", 
                                 .display = length(sizeAreaVars) >= 2 && areaChartType != "pie"),
        showLabels = mwCheckbox(showLabels, label = "Show labels", 
                                .display = length(sizeAreaVars) >= 2),
        popupAreaVars = mwSelect(
          choices = 
          {
            if (mcYear == "average") {
              c("none",
                as.character(.compareOperation(lapply(params$x, function(vv){
                  unique(vv$areaValColumnsSynt)
                }), xyCompare))
              )
            }else{
              c("none", as.character(.compareOperation(lapply(params$x, function(vv){
                unique(vv$areaValColumns)
              }), xyCompare)))
            }
          }, 
          value = {
            if(.initial) popupAreaVars
            else NULL
          }, 
          label = "Popup", 
          multiple = TRUE
        ),
        labelAreaVar = mwSelect(
          choices =     {
            if (mcYear == "average") {
              c("none",
                as.character(.compareOperation(lapply(params$x, function(vv){
                  unique(vv$areaValColumnsSynt)
                }), xyCompare))
              )
            }else{
              c("none", as.character(.compareOperation(lapply(params$x, function(vv){
                unique(vv$areaValColumns)
              }), xyCompare)))
            }
          }, 
          value = {
            if(.initial) labelAreaVar
            else NULL
          }, label = "Label", 
          .display = length(sizeAreaVars) < 2
        ),
        .display = any(sapply(params$x, function(p) {"areas" %in% names(p$x)}))
      ),
      
      Links = mwGroup(
        colLinkVar = mwSelect(
          {
            c("none", 
              as.character(.compareOperation(lapply(params$x, function(vv){
                unique(vv$linkValColums)
              }), xyCompare)))
          }, 
          value = {
            if(.initial) colLinkVar
            else NULL
          }, label = "Color"),
        sizeLinkVar = mwSelect({c("none",
                                  as.character(.compareOperation(lapply(params$x, function(vv){
                                    unique(vv$linkNumValColumns)
                                  }), xyCompare))
        )}, 
        value = {
          if(.initial) sizeLinkVar
          else NULL
        }, label = "Width"),
        popupLinkVars = mwSelect(  { c("none", 
                                       as.character(.compareOperation(lapply(params$x, function(vv){
                                         unique(vv$linkValColums)
                                       }), xyCompare)))
        },
        value = {
          if(.initial) popupLinkVars
          else NULL
        }, label = "Popup", multiple = TRUE),
        .display = any(sapply(params$x, function(p) {"links" %in% names(p$x)}))
      ),
      mapLayout = mwSharedValue(mapLayout),
      main = mwText(main, label = "title"),
      params = mwSharedValue({
        .getDataForComp(x_tranform, NULL, compare, compareOpts, 
                        processFun = processFun, mapLayout = mapLayout)
      }),
      .width = width,
      .height = height,
      .compare = {
        compare
      },
      .compareOpts = {
        compareOptions
      },
      .return = function(w, e) {combineWidgets(w, title = main, width = width, height = height)},
      ...
    )
    
}


