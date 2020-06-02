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
#' @param typeSizeAreaVars
#'   \code{logical}. Select \code{sizeAreaVars} using alias ? Default to \code{FALSE}
#' @param aliasSizeAreaVars
#'   If \code{typeSizeAreaVars} is set to TRUE, name of alias. You can find the list 
#'   of alias with the function \code{\link[antaresRead]{showAliases}}
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
#'   time id present in the data.
#' @param main
#'   Title of the map.
#' @param options
#'   List of parameters that override some default visual settings. See the
#'   help of \code{\link{plotMapOptions}}.
#' @param sizeMiniPlot \code{boolean} variable size for miniplot
#' @param h5requestFiltering Contains arguments used by default for h5 request,
#'   typically h5requestFiltering = list(mcYears = 3)
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
#'    \item "typeSizeAreaVars"
#'    \item "aliasSizeAreaVars"
#'    }
#' @return 
#' An htmlwidget of class "leaflet". It can be modified with package 
#' \code{leaflet}. By default the function starts a shiny gadget that lets the
#' user play with most of the parameters of the function. The function returns
#' a leaflet map when the user clicks on the button \code{"OK"}.
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
#' # Use custom alias
#' setAlias("custom_alias", "short description", c("OIL", "GAS", "COAL")) 
#' plotMap(x = mydata, mapLayout = ml, typeSizeAreaVars = TRUE, 
#'     aliasSizeAreaVars = "custom_alias")
#'     
#' plotMap(x = mydata, mapLayout = ml, interactive = FALSE, 
#'     language = "fr", aliasSizeAreaVars = "Renouvelable", typeSizeAreaVars = TRUE)
#' 
#' # Use h5 for dynamic request / exploration in a study
#' # Set path of simulaiton
#' setSimulationPath(path = path1)
#' 
#' # Convert your study in h5 format
#' writeAntaresH5(path = myNewPath)
#' 
#' # Redefine sim path with h5 file
#' opts <- setSimulationPath(path = myNewPath)
#' plotMap(x = opts, mapLayout = ml)
#' 
#' # Compare elements in a single study
#' plotMap(x = opts, mapLayout = ml,  .compare = "mcYear")
#' 
#' # Compare 2 studies
#' plotMap(x = list(opts, opts2), mapLayout = ml)
#' 
#' # Compare 2 studies with argument refStudies 
#' plotMap(x = opts, refStudy = opts2, mapLayout = ml)
#' plotMap(x = opts, refStudy = opts2, mapLayout = ml, interactive = FALSE, mcYearh5 = 2) 
#' plotMap(x = opts, refStudy = opts2, mapLayout = ml, h5requestFiltering = 
#' list(mcYears = myMcYear))
#' }
#' 
#' @export
plotMap <- function(x, 
                    refStudy = NULL,
                    mapLayout, 
                    colAreaVar = "none", 
                    sizeAreaVars = c(),
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
                    typeSizeAreaVars = FALSE,
                    aliasSizeAreaVars = c(),
                    compare = NULL,
                    compareOpts = list(),
                    interactive = getInteractivity(),
                    options = plotMapOptions(),
                    width = NULL, height = NULL, dateRange = NULL, xyCompare = c("union","intersect"),
                    h5requestFiltering = list(),
                    timeSteph5 = "hourly",
                    mcYearh5 = NULL,
                    tablesh5 = c("areas", "links"),
                    sizeMiniPlot = FALSE,language = "en", 
                    hidden = NULL, ...) {
  
  .check_x(x)
  .check_compare_interactive(compare, interactive)
  
  Column <- optionsT <- NULL
  tpMap <- plotMapOptions()
  
  .check_languages(language)
  
  if(language != "en"){
    colAreaVar <- .getColumnsLanguage(colAreaVar, language)
    sizeAreaVars <- .getColumnsLanguage(sizeAreaVars, language)
    popupAreaVars <- .getColumnsLanguage(popupAreaVars, language)
    labelAreaVar <- .getColumnsLanguage(labelAreaVar, language)
    colLinkVar <- .getColumnsLanguage(colLinkVar, language)
    sizeLinkVar <- .getColumnsLanguage(sizeLinkVar, language)
    popupLinkVars <- .getColumnsLanguage(popupLinkVars, language)
    aliasSizeAreaVars <- .getColumnsLanguage(aliasSizeAreaVars, language)
  }
  
  # alias
  map_alias <- .getMapAlias(language = language)
  
  # Check hidden
  .validHidden(hidden, c("H5request", "timeSteph5", "tables", "mcYearH5", "mcYear", "dateRange", "Areas", "colAreaVar", 
                         "sizeAreaVars", "miniPlot", "areaChartType", "sizeMiniPlot", "uniqueScale", "showLabels",
                         "popupAreaVars", "labelAreaVar", "Links", "colLinkVar", "sizeLinkVar", "popupLinkVars","type", 
                         "typeSizeAreaVars", "aliasSizeAreaVars"))
  
  # Check compare
  .validCompare(compare,  c("mcYear", "type", "colAreaVar", "sizeAreaVars", "areaChartType", "showLabels",
                            "popupAreaVars", "labelAreaVar","colLinkVar", "sizeLinkVar", "popupLinkVars", 
                            "typeSizeAreaVars", "aliasSizeAreaVars"))
  
  runScale <- ifelse(!identical(options[names(options)!="preprocess"] ,
                                tpMap[names(tpMap)!="preprocess"]), FALSE, TRUE)
  
  type <- match.arg(type)
  areaChartType <- match.arg(areaChartType)
  xyCompare <- match.arg(xyCompare)
  
  tmp_colAreaVar <- gsub("(_std$)|(_min$)|(_max$)", "", colAreaVar)
  if(tmp_colAreaVar != "none" & tmp_colAreaVar%in%colorsVars$Column & runScale)
  {
    raw <- colorsVars[Column == tmp_colAreaVar]
    options <- plotMapOptions(areaColorScaleOpts = colorScaleOptions(
      negCol = "#FF0000",
      # zeroCol = rgb(raw$red, raw$green, raw$blue,  maxColorValue = 255),
      # posCol = rgb(raw$red/2, raw$green/2, raw$blue/2, maxColorValue = 255)),
      zeroCol = "#FFFFFF",
      posCol = rgb(raw$red, raw$green, raw$blue, maxColorValue = 255)))
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
      x <- copy(as.antaresDataList(x))
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
    if (is.null(timeId)){
      timeId <- timeIdMin
    }else{
      timeIdTp <- timeId
      if(!is.null(x$areas)){
        x$areas <- x$areas[timeId %in% timeIdTp]
      }
      if(!is.null(x$links)){
        x$links <- x$links[timeId %in% timeIdTp]
      }
    }
    
    # Keep only links and areas present in the data
    if (areas) {
      areaList <- unique(x$areas$area)
      mapLayout$coords <- mapLayout$coords[area %in% areaList]
      if(!is.null(mapLayout$map)){
        mapLayout$map <- mapLayout$map[match(mapLayout$coords$geoAreaId, mapLayout$map$geoAreaId), ]
      }
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
      
      if(!any(mapLayout$coords$area %in% unique(x$areas$area))){
        return(combineWidgets(.getLabelLanguage("Invalid Map Layout : wrongs nodes/links informations", language)))
      }
      
      type <- match.arg(type)
      if (type == "avg") t <- NULL
      else if (is.null(t)) t <- 0
      
      # Prepare data
      if (mcYear == "average") x <- syntx
      
      if(!is.null(dateRange)){
        dateRange <- sort(dateRange)
        # xx <<- copy(x)
        # dd <<- dateRange
        if(!is.null(x$areas))
        {
          # in case of missing transformation...
          if("character" %in% class(x$areas$time)){
            x$areas[,time := .timeIdToDate(x$areas$timeId, attr(x, "timeStep"), simOptions(x))]
          }
          if("Date" %in% class(x$areas$time)){
            x$areas[,time := as.POSIXct(time, tz = "UTC")]
          }
          x$areas  <- x$areas[time >= as.POSIXlt(dateRange[1], tz = "UTC") & time < as.POSIXlt(dateRange[2], tz = "UTC") + 1]
        }
        if(!is.null(x$links))
        {
          # in case of missing transformation...
          if("character" %in% class(x$links$time)){
            x$links[,time := .timeIdToDate(x$links$timeId, attr(x, "timeStep"), simOptions(x))]
          }
          if("Date" %in% class(x$links$time)){
            x$links[,time := as.POSIXct(time, tz = "UTC")]
          }
          x$links <- x$links[time >= as.POSIXlt(dateRange[1], tz = "UTC") & time < as.POSIXlt(dateRange[2], tz = "UTC") + 1]
        }
      }
      
      if (initial) {
        assign("currentMapLayout", mapLayout, envir = env_plotFun)
        map <- .initMap(x, mapLayout, options, language = language) %>% syncWith(group)
        #TODO 
        #IF WE DONT COMMENT THIS LINES THEN WE HAVE A BUG 
        ## with 2 or more optsH5, plotFun return always a htmlWidget (and not a leafet_proxy)
        ## for the first graph and so we cannot update the first one 
        ## .initMap : return a leaflet Htmlwidget
        ## maybe we will need to do something if we have different mapLayout for different studies
        ## ??
      # } else if(!isTRUE(all.equal(mapLayout, get("currentMapLayout", envir = env_plotFun)))){
      #   print(" no initial 1")
      #   print(group)
      #   assign("currentMapLayout", mapLayout)
      #   map <- .initMap(x, mapLayout, options, language = language) %>% syncWith(group)
      } else {
        # in some case, map doesn't existed yet....!
        if("output_1_zoom" %in% names(session$input)){
          map <- leafletProxy(outputId, session)
        } else {
          map <- .initMap(x, mapLayout, options, language = language) %>% syncWith(group)
        }
      }
      map <- map %>% 
        .redrawLinks(x, mapLayout, mcYear, t, colLinkVar, sizeLinkVar, popupLinkVars, options) %>% 
        .redrawCircles(x, mapLayout, mcYear, t, colAreaVar, sizeAreaVars, popupAreaVars, 
                       uniqueScale, showLabels, labelAreaVar, areaChartType, options, sizeMiniPlot)
      
      # combineWidgets(map, width = width, height = height) # bug
      map
      
    }
    
    
    # Create the interactive widget
    if(language != "en"){
      ind_to_change <- which(colnames(x$areas) %in% language_columns$en)
      if(length(ind_to_change) > 0){
        new_name <- language_columns[get("en") %in% colnames(x$areas), ]
        v_new_name <- new_name[[language]]
        names(v_new_name) <- new_name[["en"]]
        setnames(x$areas, colnames(x$areas)[ind_to_change], unname(v_new_name[colnames(x$areas)[ind_to_change]]))
      }
      
      ind_to_change <- which(colnames(syntx$areas) %in% language_columns$en)
      if(length(ind_to_change) > 0){
        new_name <- language_columns[get("en") %in% colnames(syntx$areas), ]
        v_new_name <- new_name[[language]]
        names(v_new_name) <- new_name[["en"]]
        setnames(syntx$areas, colnames(syntx$areas)[ind_to_change], unname(v_new_name[colnames(syntx$areas)[ind_to_change]]))
      }
      
      ind_to_change <- which(colnames(x$links) %in% language_columns$en)
      if(length(ind_to_change) > 0){
        new_name <- language_columns[get("en") %in% colnames(x$links), ]
        v_new_name <- new_name[[language]]
        names(v_new_name) <- new_name[["en"]]
        setnames(x$links, colnames(x$links)[ind_to_change], unname(v_new_name[colnames(x$links)[ind_to_change]]))
      }
      
      ind_to_change <- which(colnames(syntx$links) %in% language_columns$en)
      if(length(ind_to_change) > 0){
        new_name <- language_columns[get("en") %in% colnames(syntx$links), ]
        v_new_name <- new_name[[language]]
        names(v_new_name) <- new_name[["en"]]
        setnames(syntx$links, colnames(syntx$links)[ind_to_change], unname(v_new_name[colnames(syntx$links)[ind_to_change]]))
      }
    }
    
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
    
    listParamH5NoInt <- list(
      timeSteph5 = timeSteph5,
      mcYearh5 = mcYearh5,
      tablesh5 = tablesh5, 
      h5requestFiltering = h5requestFiltering
    )
    params <- .getParamsNoInt(x = x, 
                              refStudy = refStudy, 
                              listParamH5NoInt = listParamH5NoInt, 
                              compare = compare, 
                              compareOptions = compareOptions, 
                              processFun = processFun,
                              mapLayout = mapLayout)
    
    if(!typeSizeAreaVars){
      sizeAreaVars <- sizeAreaVars
    } else {
      sizeAreaVars <- unique(do.call("c", map_alias[aliasSizeAreaVars]))
    }
    
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
  meanYearH5 <- NULL
  
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
          
          if(!typeSizeAreaVars){
            sizeAreaVars <- sizeAreaVars
          } else {
            sizeAreaVars <- unique(do.call("c", map_alias[aliasSizeAreaVars]))
          }
          widget <- params$x[[.id]]$plotFun(t = params$x[[.id]]$timeId,
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
          

          
          # controlWidgetSize(widget, language) # bug due to leaflet and widget

          widget
        } else {
          combineWidgets(.getLabelLanguage("No data for this selection", language))
        }
      }else{
        combineWidgets()
      }
    },
    
    x = mwSharedValue({x}),
    x_in = mwSharedValue({
      .giveListFormat(x)
    }),
    
    h5requestFiltering = mwSharedValue({h5requestFiltering}),
    
    paramsH5 = mwSharedValue({
      paramsH5List <- .h5ParamList(X_I = x_in, xyCompare = xyCompare, h5requestFilter = h5requestFiltering)
      rhdf5::H5close()
      paramsH5List
    }),
    H5request = mwGroup(
      label = .getLabelLanguage("H5request", language),
      timeSteph5 = mwSelect(
        {
          if(length(paramsH5) > 0){
            choices = paramsH5$timeStepS
            names(choices) <- sapply(choices, function(x) .getLabelLanguage(x, language))
          } else {
            choices <- NULL
          }
          choices
        }, 
        value =  paramsH5$timeStepS[1],
        label = .getLabelLanguage("timeStep", language), 
        multiple = FALSE, .display = !"timeSteph5" %in% hidden
      ),
      tables = mwSelect( 
        {
          if(length(paramsH5) > 0){
            choices = paramsH5[["tabl"]][paramsH5[["tabl"]] %in% c("areas", "links")]
            names(choices) <- sapply(choices, function(x) .getLabelLanguage(x, language))
          } else {
            choices <- NULL
          }
          choices
        },
        value = {
          if(.initial) {paramsH5[["tabl"]][paramsH5[["tabl"]] %in% c("areas", "links")]} else {NULL}
        }, 
        label = .getLabelLanguage("table", language), multiple = TRUE, 
        .display = !"tables" %in% hidden
      ),
      mcYearH5 = mwSelectize(
        choices = {
          ch <- c("Average" = "", paramsH5[["mcYearS"]])
          names(ch)[1] <- .getLabelLanguage("Average", language)
          ch
        },
        value = {
          if(.initial){paramsH5[["mcYearS"]][1]}else{NULL}
        },
        label = .getLabelLanguage("mcYears to be imported", language), 
        multiple = TRUE, options = list(maxItems = 4),
        .display = (!"mcYearH5" %in% hidden  & !meanYearH5)
      ),
      meanYearH5 = mwCheckbox(value = FALSE, 
                              label = .getLabelLanguage("Average mcYear", language),
                              .display = !"meanYearH5" %in% hidden),
      .display = {any(unlist(lapply(x_in, .isSimOpts))) &  !"H5request" %in% hidden}
    ),
    sharerequest = mwSharedValue({
      if(length(meanYearH5) > 0){
        if(meanYearH5){
          list(timeSteph5_l = timeSteph5, mcYearh_l = NULL, tables_l = tables)
        } else {
          list(timeSteph5_l = timeSteph5, mcYearh_l = mcYearH5, tables_l = tables)
        }
      } else {
        list(timeSteph5_l = timeSteph5, mcYearh_l = mcYearH5, tables_l = tables)
      }
    }),
    x_tranform = mwSharedValue({
      resXT <- .get_x_transform(x_in = x_in,
                                sharerequest = sharerequest,
                                refStudy = refStudy, 
                                h5requestFilter = paramsH5$h5requestFilter )
      resXT 
      
    }),
    
    ##Stop h5
    mcYear = mwSelect(
      {
        # allMcY <- c("average",  .compareOperation(lapply(params$x, function(vv){
        #   unique(c(vv$x$areas$mcYear, vv$x$links$mcYear))
        # }), xyCompare))
        # names(allMcY) <- c(.getLabelLanguage("average", language), allMcY[-1])
        # allMcY
        # BP 2017
        allMcY <- .compareOperation(lapply(params$x, function(vv){
          unique(c(vv$x$areas$mcYear, vv$x$links$mcYear))
        }), xyCompare)
        names(allMcY) <- allMcY
        if(is.null(allMcY)){
          allMcY <- "average"
          names(allMcY) <- .getLabelLanguage("average", language)
        }
        allMcY
        
      }, 
      value = { if(.initial) mcYear else NULL}, 
      .display = any(unlist(lapply(params$x, function(X){X$showMcYear}))) & !"mcYear" %in% hidden, 
      label = .getLabelLanguage("mcYear to be displayed", language)
    ),
    type = mwRadio(
      {
        choices <- c("detail", "avg")
        names(choices) <- c(.getLabelLanguage("By time id", language), .getLabelLanguage("Average", language))
        choices
      },
      value = type, 
      label = .getLabelLanguage("type", language), 
      .display = !"type" %in% hidden
    ),
    dateRange = mwDateRange(
      value = {
        if(.initial) params$x[[1]]$dateRange
        else NULL
      },
      min = params$x[[1]]$dateRange[1], 
      max = params$x[[1]]$dateRange[2], 
      language = eval(parse(text = "language")),
      # format = "dd MM",
      separator = " : ",
      label = .getLabelLanguage("dateRange", language), 
      .display = !"dateRange" %in% hidden
    ),
    
    Areas = mwGroup(
      label = .getLabelLanguage("Areas", language),
      colAreaVar = mwSelect(
        choices = {
          if(length(params) > 0){
            if (mcYear == "average") {
              tmp <- c("none", as.character(.compareOperation(lapply(params$x, function(vv){
                unique(vv$areaValColumnsSynt)
              }), xyCompare)))
            }else{
              tmp <- c("none", as.character(.compareOperation(lapply(params$x, function(vv){
                unique(vv$areaValColumns)
              }), xyCompare)))
            }
            names(tmp) <- c(.getLabelLanguage("none", language), tmp[-1])
            tmp
          } else {
            NULL
          }
        },
        value = {
          if(.initial) colAreaVar
          else NULL
        },
        label = .getLabelLanguage("Color", language), 
        .display = !"colAreaVar" %in% hidden
      ),
      typeSizeAreaVars = mwCheckbox(value = FALSE, 
                                    label = .getLabelLanguage("Size by alias", language), 
                                    .display = !"typeSizeAreaVars" %in% hidden),
      aliasSizeAreaVars = mwSelect(
        {
          if(length(params) > 0){
            areaNumVal <- as.character(.compareOperation(lapply(params$x, function(vv){
              unique(vv$areaNumValColumns)
            }), "intersect"))
            .availableMapAlias(map_alias, areaNumVal)
          } else {
            NULL
          }
        }, 
        value = {
          if(.initial) aliasSizeAreaVars
          else NULL
        }, 
        label = .getLabelLanguage("Size", language), 
        multiple = TRUE, .display = !"aliasSizeAreaVars" %in% hidden & typeSizeAreaVars
      ),
      sizeAreaVars = mwSelect(
        {
          if(length(params) > 0){
            as.character(.compareOperation(lapply(params$x, function(vv){
              unique(vv$areaNumValColumns)
            }), xyCompare))
          } else {
            NULL
          }
        }, 
        value = {
          if(.initial) sizeAreaVars
          else NULL
        }, 
        label = .getLabelLanguage("Size", language), 
        multiple = TRUE, .display = !"sizeAreaVars" %in% hidden & !typeSizeAreaVars
      ),
      miniPlot = mwGroup(
        label = .getLabelLanguage("miniPlot", language),
        areaChartType = mwSelect(
          {
            choices <- c("bar", "pie", "polar-area", "polar-radius")
            names(choices) <- c(.getLabelLanguage("bar chart", language),
                                .getLabelLanguage("pie chart", language),
                                .getLabelLanguage("polar (area)", language),
                                .getLabelLanguage("polar (radius)", language))
            choices
          },
          value = {
            if(.initial) areaChartType
            else NULL
          }, label = .getLabelLanguage("areaChartType", language), 
          .display = !"areaChartType" %in% hidden
        ),
        sizeMiniPlot = mwCheckbox(sizeMiniPlot, label = .getLabelLanguage("sizeMiniPlot", language)),
        .display = (length(sizeAreaVars) >= 2 | typeSizeAreaVars) & !"miniPlot" %in% hidden
      ),
      uniqueScale = mwCheckbox(uniqueScale, label = .getLabelLanguage("Unique scale", language), 
                               .display = length(sizeAreaVars) >= 2 && areaChartType != "pie" & !"uniqueScale" %in% hidden
      ),
      showLabels = mwCheckbox(showLabels, label = .getLabelLanguage("Show labels", language), 
                              .display = length(sizeAreaVars) >= 2 & !"showLabels" %in% hidden
      ),
      popupAreaVars = mwSelect(
        choices = 
        {
          if(length(params) > 0){
            if (mcYear == "average") {
              tmp <- c("none", as.character(.compareOperation(lapply(params$x, function(vv){
                unique(vv$areaValColumnsSynt)
              }), xyCompare))
              )
            }else{
              tmp <- c("none", as.character(.compareOperation(lapply(params$x, function(vv){
                unique(vv$areaValColumns)
              }), xyCompare)))
            }
            names(tmp) <- c(.getLabelLanguage("none", language), tmp[-1])
            tmp
          } else {
            NULL
          }
        }, 
        value = {
          if(.initial) popupAreaVars
          else NULL
        }, 
        label = .getLabelLanguage("Popup", language), 
        multiple = TRUE, .display = !"popupAreaVars" %in% hidden
      ),
      labelAreaVar = mwSelect(
        choices = {
          if(length(params) > 0){
            if (mcYear == "average") {
              tmp <- c("none",
                       as.character(.compareOperation(lapply(params$x, function(vv){
                         unique(vv$areaValColumnsSynt)
                       }), xyCompare))
              )
            }else{
              tmp <- c("none", as.character(.compareOperation(lapply(params$x, function(vv){
                unique(vv$areaValColumns)
              }), xyCompare)))
            }
            names(tmp) <- c(.getLabelLanguage("none", language), tmp[-1])
            tmp
          } else {
            NULL
          }
        }, 
        value = {
          if(.initial) labelAreaVar
          else NULL
        }, label = .getLabelLanguage("Label", language), 
        .display = length(sizeAreaVars) < 2 & !"labelAreaVar" %in% hidden
      ),
      .display = any(sapply(params$x, function(p) {"areas" %in% names(p$x)})) & !"Areas" %in% hidden
    ),
    
    Links = mwGroup(
      label = .getLabelLanguage("Links", language),
      colLinkVar = mwSelect(
        {
          if(length(params) > 0){
            tmp <- c("none", as.character(.compareOperation(lapply(params$x, function(vv){
              unique(vv$linkValColums)
            }), xyCompare)))
            names(tmp) <- c(.getLabelLanguage("none", language), tmp[-1])
            tmp
          } else {
            NULL
          }
        }, 
        value = {
          if(.initial) colLinkVar
          else NULL
        }, label = .getLabelLanguage("Color", language), .display = !"colLinkVar" %in% hidden
      ),
      sizeLinkVar = mwSelect(
        {
          if(length(params) > 0){
            tmp <- c("none", as.character(.compareOperation(lapply(params$x, function(vv){
              unique(vv$linkNumValColumns)
            }), xyCompare)))
            names(tmp) <- c(.getLabelLanguage("none", language), tmp[-1])
            tmp
          } else {
            NULL
          }
          
        }, 
        value = {
          if(.initial) sizeLinkVar
          else NULL
        }, label = .getLabelLanguage("Width", language), .display = !"sizeLinkVar" %in% hidden
      ),
      popupLinkVars = mwSelect(
        {
          if(length(params) > 0){
            tmp <- c("none", as.character(.compareOperation(lapply(params$x, function(vv){
              unique(vv$linkValColums)
            }), xyCompare)))
            names(tmp) <- c(.getLabelLanguage("none", language), tmp[-1])
            tmp
          } else {
            NULL
          }
        },
        value = {
          if(.initial) popupLinkVars
          else NULL
        }, label = .getLabelLanguage("Popup", language), multiple = TRUE, .display = !"popupLinkVars" %in% hidden
      ),
      .display = any(sapply(params$x, function(p) {"links" %in% names(p$x)})) & !"Links" %in% hidden
    ),
    mapLayout = mwSharedValue(mapLayout),
    params = mwSharedValue({
      if(length(x_tranform) > 0 & length(mapLayout) > 0){
        .getDataForComp(x_tranform, NULL, compare, compareOpts, 
                        processFun = processFun, mapLayout = mapLayout)
      }
    }),
    options = mwSharedValue({options}),
    optionsT = mwSharedValue({
      if(length(colAreaVar) > 0){
        tmp_colAreaVar <- gsub("(_std$)|(_min$)|(_max$)", "", colAreaVar)
        if(tmp_colAreaVar %in% colorsVars$Column & runScale){
          raw <- colorsVars[Column == tmp_colAreaVar]
          plotMapOptions(areaColorScaleOpts = colorScaleOptions(
            negCol = "#FF0000",
            # zeroCol = rgb(raw$red, raw$green, raw$blue,  maxColorValue = 255),
            # posCol = rgb(raw$red/2, raw$green/2, raw$blue/2, maxColorValue = 255)),
            zeroCol = "#FFFFFF",
            posCol = rgb(raw$red, raw$green, raw$blue, maxColorValue = 255))
          )
        }else{
          options
        }
      }else{
        options
      }
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


.getMapAlias <- function(language = "en"){
  # get alias
  tmp <- capture.output({cur_alias <- antaresRead::showAliases()})
  
  others_alias <- logical(nrow(cur_alias))
  list_alias <- lapply(1:nrow(cur_alias), function(x){
    var <- as.character(cur_alias[x, "select"])
    var <- gsub("^([[:space:]]*)|([[:space:]]*)$", "", strsplit(var, ",")[[1]])
    
    # dont keep alias linked with links, clusters, ....
    others_alias[x] <<- any(c("links", "clusters", "districts") %in% var)
    var <- setdiff(var, c("areas", "links", "clusters", "districts", "mcYears"))
    var <- unname(sapply(var, function(x) .getColumnsLanguage(x, language)))
    var
  })
  
  names(list_alias) <- unname(sapply(1:nrow(cur_alias), function(x){
    .getColumnsLanguage(as.character(cur_alias[x, "name"]), language)
  }))
  
  list_alias[!others_alias]
}

.availableMapAlias <- function(alias, areaNumValColumns){

  keep_alias <- sapply(alias, function(x){
    all(x %in% areaNumValColumns)
  })
  
  ind_keep_alias <- which(keep_alias)
  if(length(ind_keep_alias) > 0){
    return(names(alias[ind_keep_alias]))
  } else {
    return(NULL)
  }
}