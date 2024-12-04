# Copyright © 2016 RTE Réseau de transport d’électricité

#' @title Plot the exchanges of an area
#'
#' @description This function draws a stack representing the evolution of the exchanges of
#' an area with its neighbours. Positive values denotes exports and negative
#' values imports.
#' User can either plot all flows from/to an area using the \code{default} stack or use a custom one.
#' User can see available stacks with \code{exchangesStackAliases} and create new ones
#' with \code{setExchangesStackAlias}.
#' 
#' @param x
#'   Object of class \code{antaresData} created with function
#'   [antaresRead::readAntares]. It is required to contain link data.
#'   If it also contains area data with column `ROW BAL.`, then exchanges with
#'   the rest of the world are also displayed on the chart.
#' @param stack
#' Name of the stack to use. If \code{default}, all flows available will be plotted. One can visualize available stacks with 
#' \code{exchangesStackAliases}
#' @param area
#'   Name of a single area. The flows from/to this area will be drawn by the
#'   function.
#' @param ylab Title of the Y-axis.
#' @param h5requestFiltering Contains arguments used by default for h5 request,
#'   typically h5requestFiltering = list(links = getLinks(areas = myArea), mcYears = myMcYear)
#' @param description
#'   Description of the stack. It is displayed by function 
#'   \code{exchangesStackAliases}.
#' @inheritParams prodStack
#'
#' @return
#' A htmlwidget of class \code{dygraph}. It can be modified with functions from
#' package \code{dygraphs}.
#'
#'
#' @details
#' Compare argument can take following values :
#' \itemize{
#'    \item "mcYear"
#'    \item "main"
#'    \item "unit"
#'    \item "area"
#'    \item "legend"
#'    \item "stepPlot"
#'    \item "drawPoints"
#'    }
#'
#' @examples
#' \donttest{
#' library(antaresRead)
#' # with study test for example (study is in package antaresRead)
#' sourcedir <- system.file("testdata", package = "antaresRead")
#'
#' # untar study in temp dir
#' path_latest <- file.path(tempdir(), "latest")
#' untar(file.path(sourcedir, "antares-test-study.tar.gz"), exdir = path_latest)
#' 
#' study_path <- file.path(path_latest, "test_case")
#' 
#' # set path to your Antares simulation
#' opts <- setSimulationPath(study_path)
#' 
#' if(interactive()){
#'   mydata <- readAntares(links = "all", timeStep = "daily")
#'   exchangesStack(mydata)
#'   
#'   # Also display exchanges with the rest of the world
#'   mydata <- readAntares(areas = "all", links = "all", timeStep = "daily")
#'   exchangesStack(mydata)
#'   
#'   # Use compare :
#'   exchangesStack(mydata, compare = "mcYear")
#'   exchangesStack(mydata, compare = "area")
#'   exchangesStack(mydata, compare = "unit")
#'   exchangesStack(mydata, compare = "legend")
#'   # Compare studies with refStudy argument
#'   exchangesStack(x = myData1, refStudy = myData2)
#'   exchangesStack(x = myData1, refStudy = myData2, interactive = FALSE)
#'   exchangesStack(x = list(myData2, myData3, myData4), refStudy = myData1)
#'   exchangesStack(x = list(myData2, myData3, myData4), refStudy = myData1, interactive = FALSE)
#'   
#'   
#'   # Compare 2 studies
#'   exchangesStack(x = list(opts, opts))
#'   
#'   # Compare 2 studies with argument refStudy
#'   exchangesStack(x = opts, refStudy = opts)
#' }
#' }
#'
#' @export
exchangesStack <- function(x,
                           stack = "default",
                           area = NULL, mcYear = "average",
                           dateRange = NULL, colors = NULL,
                           yMin = NULL, yMax =  NULL, customTicks=NULL,
                           main = NULL, ylab = NULL, unit = c("MWh", "GWh", "TWh"),
                           compare = NULL, compareOpts = list(),
                           interactive = getInteractivity(),
                           legend = TRUE, legendId = sample(1e9, 1),
                           groupId = legendId,
                           updateLegendOnMouseOver = TRUE,
                           legendItemsPerRow = 5,
                           width = NULL, height = NULL,
                           xyCompare = c("union", "intersect"),
                           h5requestFiltering = deprecated(),
                           stepPlot = FALSE, drawPoints = FALSE,
                           timeSteph5 = deprecated(),
                           mcYearh5 = deprecated(),
                           tablesh5 = deprecated(),
                           language = "en",
                           hidden = NULL,
                           refStudy = NULL,
                           ...) {
  
  deprecated_vector_params <- c(lifecycle::is_present(h5requestFiltering),
                                lifecycle::is_present(timeSteph5),
                                lifecycle::is_present(mcYearh5),
                                lifecycle::is_present(tablesh5))
  
  if(any(deprecated_vector_params)){
    lifecycle::deprecate_warn(
      when = "0.18.1", 
      what = "exchangesStack(h5requestFiltering)",
      details = "all these parameters are relative to the 'rhdf5' package, 
      which is removed from the dependencies"
    )
    
    h5requestFiltering <- NULL
    timeSteph5 <- NULL
    mcYearh5 <- NULL
    tablesh5 <- NULL
  }
  
  # force (deprecated)
  h5requestFiltering <- NULL
  timeSteph5 <- NULL
  mcYearh5 <- NULL
  tablesh5 <- NULL
    

    #we can hide these values
  exchangesStackValHidden <- c("H5request", "timeSteph5", "mcYearhH5", "mcYear", "main",
                               "dateRange", "unit", "area", "legend", "stepPlot", "drawPoints")
  exchangesStackValCompare <- c("mcYear", "main", "unit", "area", "legend", "stepPlot", "drawPoints")

  listParamsCheck <- list(
    x = x,
    compare = compare,
    interactive = interactive,
    language = language,
    hidden = hidden,
    valHidden = exchangesStackValHidden,
    valCompare = exchangesStackValCompare,
    mcYear = mcYear,
    h5requestFiltering = h5requestFiltering,
    compareOptions = compareOpts
  )
  
  
  
  listParamsCheck <- .check_params_A_get_cor_val(listParamsCheck)
  x <- listParamsCheck$x
  compare <- listParamsCheck$compare
  compareOptions <- listParamsCheck$compareOptions
  h5requestFiltering <- listParamsCheck$h5requestFiltering
  mcYear <- listParamsCheck$mcYear

  xyCompare <- match.arg(xyCompare)
  unit <- match.arg(unit)

  init_area <- area
  init_dateRange <- dateRange

  processFun <- function(x) {
    .check_x_antaresData(x)


    if(!is.null(attributes(x$areas)$hvdcAreas)){
      x <- hvdcModification(x, removeHvdcAreas = TRUE, reafectLinks = TRUE)
    }

    row <- NULL # exchanges with rest of the world

    # Check that input contains links data
    if (is(x, "antaresDataTable")) {
      if (!attr(x, "type") == "links") stop("'x' should contain link data")
    } else if (is(x, "antaresDataList")) {
      if (is.null(x$links)) stop("'x' should contain link data")

      # If they are present, add the echanges with the rest of the world
      if (!is.null(x$areas) && !is.null(x$areas$`ROW BAL.`)) {
        if ("mcYear" %in% names(x$areas)) {
          row <- x$areas[, .(area, link = paste(area, " - ROW"), timeId, mcYear,
                             flow = - `ROW BAL.`, to = "ROW", direction = 1)]
        } else {
          row <- x$areas[, .(area, link = paste(area, " - ROW"), timeId,
                             flow = - `ROW BAL.`, to = "ROW", direction = 1)]
        }
      }
    
      x <- x$links
    }


    # should mcYear parameter be displayed on the UI?
    displayMcYear <- !attr(x, "synthesis") && length(unique(x$mcYear)) > 1

    timeStep <- attr(x, "timeStep")
    opts <- simOptions(x)

    dataDateRange <- as.Date(.timeIdToDate(range(x$timeId), timeStep, opts))
    if (length(init_dateRange) < 2) init_dateRange <- dataDateRange

    linksDef <- getLinks(namesOnly = FALSE, withDirection = TRUE, opts = opts)
    linksDef <- linksDef[link %in% x$link]
    areaList <- linksDef[, unique(area)]

    if (is.null(init_area)) init_area = areaList[1]
    
      plotFun <- function(id, area, dateRange, unit, mcYear, legend, stepPlot, drawPoints, main, stack, updateLegendOnMouseOver , yMin, yMax, customTicks) {
      
      # Prepare data for stack creation
      a <- area
      linksDef <- getLinks(area, opts = simOptions(x), namesOnly = FALSE,
                           withDirection = TRUE)
      
      if(stack != "default") stackOpts <- .aliasToStackExchangesOptions(stack)
      
      dt <- x

      if (mcYear == "average") {
        dt <- synthesize(dt)
        if (!is.null(row)) row <- row[, .(flow = mean(flow)), by = .(area, link, timeId, to, direction)]
      } else if ("mcYear" %in% names(x)) {
        mcy <- mcYear
        dt <- dt[mcYear == mcy]
        if (!is.null(row)) row <- row[mcYear == mcy, .(area, link, timeId, flow, to, direction)]
      }else{
        .printWarningMcYear()
      }

      if ("annual" %in% attr(dt, "timeStep")){
        dateRange <- NULL
      }

      flux_name <- .getColumnsLanguage("FLOW LIN.", language = language)
      if (!flux_name %in% colnames(dt)){
        flux_name <- "FLOW LIN."
      }
      
      
      if (!is.null(dateRange)){
        dt <- merge(dt[as.Date(.timeIdToDate(timeId, timeStep, simOptions(x))) %between% dateRange,
                       .(link, timeId, flow = get(flux_name))],
                    linksDef, by = "link")
      } else {
        dt <- merge(dt[, .(link, timeId, flow = get(flux_name))], linksDef, by = "link")
      }

      if (!is.null(row)) {
        if (!is.null(dateRange)){
          row <- row[as.Date(.timeIdToDate(timeId, timeStep, simOptions(x))) %between% dateRange]
        }
        
        dt <- rbind(dt, row[area == a])
      }
      dt[, flow := flow * direction / switch(unit, MWh = 1, GWh = 1e3, TWh = 1e6)]

      if (nrow(dt) == 0){return(combineWidgets(.getLabelLanguage("No data",language)))}

      dt <- dcast(dt, timeId ~ to, value.var = "flow")

      # if ("ROW" %in% colnames(dt)){
      #   dt[, ROW := NULL]
      # }

      # Graphical parameters
      
      if (is.null(main) | isTRUE(all.equal("", main))){
        main <- paste(.getLabelLanguage("Flows from/to", language), area)
      }
      if (is.null(ylab)){
        ylab <- sprintf(.getLabelLanguage("Flows (%s)", language), unit)
      }

      if (is.null(colors)) {
        colors <- substring(rainbow(ncol(dt) - 1, s = 0.7, v = 0.7), 1, 7)
        }
      else {
        colors <-  rev(rep(colors, length.out = ncol(dt)- 1))
      }

      # BP 2017
      # if (length(main) > 0){
      #   mcYear <- ifelse(mcYear == "average", "moyen", mcYear)
      #   if (grepl("h5$", main)){
      #     # main <- paste0(gsub(".h5$", "", main), " : ", area, " (tirage ", mcYear, ")")
      #     main <- paste0(gsub(".h5$", "", main), " : Tirage ", mcYear)
      #   } else {
      #     # main <- paste0("Échanges ", area, " (tirage ", mcYear, ")")
      #     main <- paste0("Tirage ", mcYear)
      #   }
      # }

      # Stack
      if (stack != "default"){
      g <- try(.plotExchangesStack(dt,
                                       stackOpts$variables,
                                       stackOpts$colors,
                                       stackOpts$lines,
                                       stackOpts$lineColors,
                                       stackOpts$lineWidth,
                                       opts=simOptions(x),
                                       timeStep=timeStep,
                                       main = main,
                                       unit = unit,
                                       legendId = legendId + id - 1,
                                       groupId = groupId,
                                       updateLegendOnMouseOver = updateLegendOnMouseOver,
                                       dateRange = dateRange,
                                       yMin=yMin , yMax=yMax , customTicks=customTicks,
                                       stepPlot = stepPlot, drawPoints = drawPoints, language = language), silent = TRUE)
      }
      else{
        
        g <- .plotStack(dt, timeStep, opts, colors,
                        legendId = legendId + id - 1, groupId = groupId,
                        main = main, ylab = ylab, updateLegendOnMouseOver = updateLegendOnMouseOver,
                        yMin=yMin , yMax=yMax , customTicks=customTicks,,stepPlot = stepPlot,
                        drawPoints = drawPoints, language = language, type = "Exchanges")
        
      }
      
      
    
      if ("try-error" %in% class(g)){
        return (
          combineWidgets(paste0(.getLabelLanguage("Can't visualize stack", language), " '", stack, "'<br>", g[1]))
        )
      }
      
      if (legend & !"ramcharts_base" %in% class(g)) {
        # Add a nice legend
        
        if (stack != "default"){
        legend <- prodStackExchangesLegend(stack, legendItemsPerRow, legendId = legendId + id - 1, 
                                           language = language)}
        else{
          legend <- tsLegend(names(dt)[-1], colors, types = "area",
                             legendItemsPerRow = legendItemsPerRow,
                             legendId = legendId + id - 1)
        }
      } else legend <- NULL
      
      combineWidgets(g, footer = legend, width = width, height = height)
      }
  
    list(
      plotFun = plotFun,
      areaList = areaList,
      area = init_area,
      dataDateRange = dataDateRange,
      dateRange = init_dateRange,
      displayMcYear = displayMcYear,
      x = x
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
                             processFun = processFun)
    
     
      L_w <- lapply(seq_along(params$x), function(i){
        myData <- params$x[[i]]
        myData$plotFun(i, myData$area, params$x[[1]]$dateRange, unit,
                              mcYear, legend, stepPlot, drawPoints, main, stack, updateLegendOnMouseOver, yMin, yMax, customTicks)
      }    )
  
    return(combineWidgets(list = L_w))
  }

  table <- NULL

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
      # .tryCloseH5()
      # udpate for mw 0.11 & 0.10.1
      if(!is.null(params)){
        ind <- .id %% length(params$x)
        if(ind == 0) ind <- length(params$x)
        widget <- params$x[[ind]]$plotFun(.id, area, dateRange, unit, mcYear, legend, stepPlot, drawPoints, main,
                                          stack, updateLegendOnMouseOver, yMin, yMax, customTicks)
        controlWidgetSize(widget, language)
      } else {
        combineWidgets(.getLabelLanguage("No data for this selection", language))
      }
    },
    x = mwSharedValue(x),
    h5requestFiltering = mwSharedValue({h5requestFiltering}),

    x_in = mwSharedValue({
      .giveListFormat(x)
    }),

    # paramsH5 = mwSharedValue({
    #   .h5ParamList(X_I = x_in, xyCompare = xyCompare, h5requestFilter = h5requestFiltering)
    # }),

    H5request = mwGroup(
      label = .getLabelLanguage("H5request", language),
      timeSteph5 = mwSelect(
        {
          if (length(paramsH5) > 0){
            choices = paramsH5$timeStepS
            names(choices) <- sapply(choices, function(x) .getLabelLanguage(x, language))
            choices
          } else {
            NULL
          }
        },
        value =  if (.initial)  paramsH5$timeStepS[1] else NULL,
        label = .getLabelLanguage("timeStep", language),
        multiple = FALSE, .display = !"timeSteph5" %in% hidden
      ),
      mcYearH5 = mwSelectize(
        choices = {
          ch <- c("Average" = "", paramsH5[["mcYearS"]])
          names(ch)[1] <- .getLabelLanguage("Average", language)
          ch
        },
        value = {
          if (.initial){paramsH5[["mcYearS"]][1]}else{NULL}
        },
        label = .getLabelLanguage("mcYears to be imported", language),
        multiple = TRUE, options = list(maxItems = 4),
        .display = (!"mcYearH5" %in% hidden  & !meanYearH5)
      ),
      meanYearH5 = mwCheckbox(value = FALSE,
                              label = .getLabelLanguage("Average mcYear", language),
                              .display = !"meanYearH5" %in% hidden),
      .display = {
        any(unlist(lapply(x_in, .isSimOpts))) & !"H5request" %in% hidden
      }
    ),

    #TODO partager ce code avec prodStack() mais avant cree le widget tables_l et lui
    #mettre comme valeur links
    # ne pas montrer ce widget a l utilisateur
    sharerequest = mwSharedValue({
      if (length(meanYearH5) > 0){
        if (meanYearH5){
          list(timeSteph5_l = timeSteph5, mcYearh_l = NULL, tables_l = NULL)
        } else {
          list(timeSteph5_l = timeSteph5, mcYearh_l = mcYearH5, tables_l = NULL)
        }
      } else {
        list(timeSteph5_l = timeSteph5, mcYearh_l = mcYearH5, tables_l = NULL)
      }
    }),

    x_tranform = mwSharedValue({
      areas <- "all"
      links <- "all"
      if (length(paramsH5$h5requestFilt[[1]]) > 0){
        areas <- NULL
        links <- NULL
      }

      # h5requestFilteringTp <- paramsH5$h5requestFilter
      # if (!is.null(sharerequest))
      # {
      #   for (i in 1:length(h5requestFilteringTp))
      #   {
      #     if (sharerequest$tables == "areas"){
      #       h5requestFilteringTp[[i]]$districts = NULL
      #     }
      #     if (sharerequest$tables == "districts"){
      #       h5requestFilteringTp[[i]]$areas = NULL
      #     }
      #   }
      # }


      # TODO next version get only what we need
      # if (!is.null(area)){
      #   print(area)
      #   areas <- area
      #   links <- getLinks(area)
      # }else{
      #   print(init_area)
      #   areas <- init_area
      #   links <- getLinks(init_area)
      # }

      resXT <- .get_x_transform(x_in = x_in,
                                sharerequest = sharerequest,
                                refStudy = refStudy,
                                h5requestFilter = paramsH5$h5requestFilter,
                                areas = areas,
                                links = links)
      resXT
    }),

    mcYear = mwSelect({
      # allMcY <- c("average",  .compareOperation(lapply(params$x, function(vv){
      #   unique(vv$x$mcYear)
      # }), xyCompare))
      # names(allMcY) <- c(.getLabelLanguage("average", language), allMcY[-1])

      # BP 2017
      allMcY <- .compareOperation(lapply(params$x, function(vv){
        unique(vv$x$mcYear)
      }), xyCompare)
      allMcY=c("average",allMcY)
      names(allMcY) <- allMcY
      
      if (is.null(allMcY)){
        allMcY <- "average"
        names(allMcY) <- .getLabelLanguage("average", language)
      }
      allMcY
    },
    value = {
      if (.initial) mcYear
      else NULL
    },
    .display = {
      # length(c("average", if (!is.null(params)){
      #   as.character(.compareOperation(lapply(params$x, function(vv){
      #     unique(vv$x$mcYear)
      #   }), xyCompare))})) != 1 &
      !"mcYear" %in% hidden
    },
    label = .getLabelLanguage("mcYear to be displayed", language)
    ),

    area = mwSelect({
      if (!is.null(params)){
        as.character(.compareOperation(lapply(params$x, function(vv){
          unique(vv$areaList)
        }), xyCompare))
      }
    },
    value = {
      if (.initial){
        if (!is.null(area)){
          area
        } else {
          if (!is.null(params)){
            as.character(.compareOperation(lapply(params$x, function(vv){
              unique(vv$areaList)
            }), xyCompare))[1]
          } else {
            NULL
          }
        }
      }
      else NULL
    }, label = .getLabelLanguage("area", language), .display = !"area" %in% hidden),
    dateRange = mwDateRange(value = {
      if (.initial){
        res <- NULL
        if (!is.null(params)){
          res <- c(.dateRangeJoin(params = params, xyCompare = xyCompare, "min", tabl = NULL),
                   .dateRangeJoin(params = params, xyCompare = xyCompare, "max", tabl = NULL))
        }

        ##Lock 7 days for hourly data
        if (!is.null(attributes(params$x[[1]]$x)$timeStep)){
          if (attributes(params$x[[1]]$x)$timeStep == "hourly"){
            if (params$x[[1]]$dateRange[2] - params$x[[1]]$dateRange[1] > 7){
              res[1] <- params$x[[1]]$dateRange[2] - 7
            }
          }
        }
        res
      }else{NULL}
    },
    min = {
      if (!is.null(params)){
        if (attributes(params$x[[1]]$x)$timeStep != "annual"){
          .dateRangeJoin(params = params, xyCompare = xyCompare, "min", tabl = table)
        } else {
          NULL
        }
      }
    },
    max = {
      if (!is.null(params)){
        if (attributes(params$x[[1]]$x)$timeStep != "annual"){
          .dateRangeJoin(params = params, xyCompare = xyCompare, "max", tabl = table)
        } else {
          NULL
        }
      }
    },
    language = eval(parse(text = "language")),
    # format = "dd MM",
    separator = " : ",
    .display = timeStepdataload != "annual" & !"dateRange" %in% hidden,
    label = .getLabelLanguage("dateRange", language)
    ),
    stack = mwSelect(c("default",names(pkgEnv$exchangesStackAliases)), stack,
                     label = .getLabelLanguage("stack", language), .display = !"stack" %in% hidden),

    unit = mwSelect(c("MWh", "GWh", "TWh"), unit, label = .getLabelLanguage("unit", language),
                    .display = !"unit" %in% hidden),
    yMin= mwNumeric(value=yMin,label="yMin",
                    .display = !"yMin" %in% hidden),
    yMax= mwNumeric(value=yMax,label="yMax",
                    .display = !"yMax" %in% hidden),
    legend = mwCheckbox(legend, label = .getLabelLanguage("legend", language),
                        .display = !"legend" %in% hidden),
    stepPlot = mwCheckbox(stepPlot, label = .getLabelLanguage("stepPlot", language),
                          .display = !"stepPlot" %in% hidden),
    drawPoints = mwCheckbox(drawPoints, label = .getLabelLanguage("drawPoints", language),
                            .display = !"drawPoints" %in% hidden),
    updateLegendOnMouseOver = mwCheckbox(updateLegendOnMouseOver, label =.getLabelLanguage("updateLegendOnMouseOver", language),
                           .display = !"updateLegendOnMouseOver" %in% hidden),
    timeStepdataload = mwSharedValue({
      attributes(x_tranform[[1]])$timeStep
    }),

    main = mwText(main, label = .getLabelLanguage("title", language),
                  .display = !"main" %in% hidden),

    params = mwSharedValue({
      .getDataForComp(x_tranform, NULL, compare, compareOpts,
                      processFun = processFun)
    }),

    .compare = {
      compare
    },
    .compareOpts = {
      compareOptions
    },
    ...
  )

}


#' Returns the variables and colors corresponding to an alias
#' 
#' @param variables
#'   character string représenting an alias
#'   
#' @return 
#' This function returns a list with four components:
#' \item{variables}{Definition of the variables of the stack}
#' \item{colors}{colors for the variables}
#' \item{lines}{Definition of the curves to draw on top of the production stack}
#' \item{lineColors}{colors for the curves}
#' @noRd
.aliasToStackExchangesOptions <- function(variables) {
  if (! variables %in% names(pkgEnv$exchangesStackAliases)) {
    stop("Unknown alias '", variables, "'.")
  }
  
  pkgEnv$exchangesStackAliases[[variables]]
}


#' Generate an interactive stack
#' 
#' @param x
#'   data.table of class "antaresDataTable" containing data for one and only one
#'   area.
#' @param variables
#'   list created with function "alist" representing the definition of the
#'   variables to plot. 
#' @param colors
#'   vector of colors. It must have the same length as variables.
#' 
#' @return 
#'   an htmlWidget created with function "dygraph"
#'   
#' @note 
#' When series have positive and negative values, stacked area graphs are not
#' clearly defined. In our case we want positive values shown in the part of
#' the graph above 0 and the negative values below 0. To achieve that, we have
#' to hack the default behavior of dygraphs:
#' 
#' 1 - for each time step, sum all negative values and plot the corresponding
#' area in white
#' 2 - plot the areas corresponding to the negative values of each series as if
#' they were positive. This will completely cover the area drawn in step 1.
#' 3 - plot areas corresponding to the positive values of each series.
#'
#' Notice that dygraphs plot series in reverse order, so the data table we need
#' to create must contain a time column, then the positive values of each
#' series, then the negative values of each column (absolute values) and finally
#' a column with the total of negative values.
#' 
#' dygraphs does not offer the possibility to add a curve over a stacked graph.
#' Once again this require a hack: before ploting any area, plot the curve series
#' without filling the area under the curve, then plot an invisible series equal
#' to the opposite of the curve in order to "go back" to zero. This way, the 
#' next series is drawn from 0.
#' 
#' @noRd
.plotExchangesStack <- function(x, variables, colors, lines, lineColors, lineWidth, opts, timeStep,
                                    main = NULL, unit = "MWh", legendId = "",
                                    groupId = legendId, width = NULL, height = NULL, dateRange = NULL, yMin=NULL, yMax=NULL, customTicks = NULL,
                                    stepPlot = FALSE, drawPoints = FALSE, updateLegendOnMouseOver=TRUE, language = "en", type = "Exchanges") {
  
  
  formulas <- append(variables, lines)
  variables <- names(variables)
  lines <- names(lines)
  
  
  dt <- data.table(timeId = x$timeId)
  
  for (n in names(formulas)) {
    dt[, c(n) := x[, eval(formulas[[n]]) / switch(unit, MWh = 1, GWh = 1e3, TWh = 1e6)]]
  }

  
  p <- .plotStack(dt, timeStep, opts, colors, lines, lineColors, lineWidth, legendId,
                  groupId,
                  main = main, ylab = sprintf("Flows (%s)", unit), 
                  width = width, height = height, dateRange = dateRange, yMin = yMin, yMax =  yMax, customTicks= customTicks,
                  stepPlot = stepPlot, drawPoints = drawPoints, updateLegendOnMouseOver=updateLegendOnMouseOver , language = language, type = type)
  p
}

#' @rdname tsLegend
#' @export
prodStackExchangesLegend <- function(stack = "default", 
                                     legendItemsPerRow = 5, legendId = "", language = "en") {
  
  stackOpts <- .aliasToStackExchangesOptions(stack)
  
  if (!is.null(stackOpts$variables)) {
    names(stackOpts$variables) <- sapply(names(stackOpts$variables), function(x){
      .getColumnsLanguage(x, language)
    })
  }
  if (!is.null(stackOpts$lines)) {
    names(stackOpts$lines) <- sapply(names(stackOpts$lines), function(x){
      .getColumnsLanguage(x, language)
    })
  }
  
  tsLegend(
    labels = c(names(stackOpts$variables), names(stackOpts$lines)), 
    colors = c(stackOpts$colors, stackOpts$lineColors),
    types = c(rep("area", length(stackOpts$variables)), rep("line", length(stackOpts$lines))),
    legendItemsPerRow = legendItemsPerRow,
    legendId = legendId
  )
}
