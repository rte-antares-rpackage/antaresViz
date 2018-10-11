#' Compare options from length of data
#' 
#' @param x list of data
#' @param compare character
#' 
#' @noRd
.compOpts <- function(x, compare){
  len <- 1
  
  if ("list" == class(x)[1]){
    len <- length(x)
  }
  if (length(x) > 1)
  {
    
    ncol = ifelse(len > 2, 2, 1)
    nrow = floor( (len - 1) / 2) + 1 + ifelse(len == 2, 1, 0)
    return(list(ncharts = len, nrow = nrow, ncol = ncol))
  }
  if (!is.null(compare)){
    return(
      list(ncharts = 2, nrow = 2, ncol = 1)
    )
  }
  
  return(list(ncharts = 1, nrow = 1, ncol = 1))
  
}

#' Join date range
#' 
#' @param params list of data
#' @param xyCompare character
#' @param minMax character
#' @param tabl character
#' 
#' @noRd
.dateRangeJoin <- function(params, xyCompare, minMax, tabl = NULL){
  if (minMax == "min" & xyCompare == "union"){
    if (!is.null(tabl))
    {
      date_range <- lapply(params$x, function(X){
        X[[tabl]]$dateRange[1]
      })
      date_range <- date_range[which(sapply(date_range, function(x) !is.null(x)))]
      
      return(min(do.call("c", date_range)))
    }else{
      date_range <- lapply(params$x, function(X){
        X$dateRange[1]
      })
      date_range <- date_range[which(sapply(date_range, function(x) !is.null(x)))]
      
      return(min(do.call("c", date_range)))
    }
  }
  if (minMax == "min" & xyCompare == "intersect"){
    if (!is.null(tabl))
    {
      date_range <- lapply(params$x, function(X){
        X[[tabl]]$dateRange[1]
      })
      date_range <- date_range[which(sapply(date_range, function(x) !is.null(x)))]
      
      return(max(do.call("c", date_range)))
    }else{
      date_range <- lapply(params$x, function(X){
        X$dateRange[1]
      })
      date_range <- date_range[which(sapply(date_range, function(x) !is.null(x)))]
      
      return(max(do.call("c", date_range)))
    }
  }
  if (minMax == "max" & xyCompare == "union"){
    if (!is.null(tabl))
    {
      date_range <- lapply(params$x, function(X){
        X[[tabl]]$dateRange[2]
      })
      date_range <- date_range[which(sapply(date_range, function(x) !is.null(x)))]
      
      return(max(do.call("c", date_range)))
    }else{
      date_range <- lapply(params$x, function(X){
        X$dateRange[2]
      })
      date_range <- date_range[which(sapply(date_range, function(x) !is.null(x)))]

      return(max(do.call("c", date_range)))
    }
  }
  if (minMax == "max" & xyCompare == "intersect"){
    if (!is.null(tabl))
    {
      date_range <- lapply(params$x, function(X){
        X[[tabl]]$dateRange[2]
      })
      date_range <- date_range[which(sapply(date_range, function(x) !is.null(x)))]
      
      return(min(do.call("c", date_range)))
    }else{
      date_range <- lapply(params$x, function(X){
        X$dateRange[2]
      })
      date_range <- date_range[which(sapply(date_range, function(x) !is.null(x)))]
      
      return(min(do.call("c", date_range)))
    }
  }
}



#' Transform data
#' 
#' @param x list of data
#' @param compare character
#' @param compareOpts list
#' @param processFun function
#' @param ...
#' 
#' @noRd
.transformDataForComp <- function(x, compare = NULL,
                                  compareOpts = NULL,
                                  processFun = as.antaresDataList, ...) {
  if (!is.list(x)){return(NULL)}
  if (is.null(compareOpts)) compareOpts <- list()
  assert_that(is.function(processFun))
  assert_that(is.list(x))
  assert_that(all(sapply(x, inherits, what = "antaresData")), 
              msg = "'x' is not an antaresData or a list of antaresData objects")
  x <- lapply(x, processFun, ...)
  compareOpts$ncharts <- length(x)
  if (is.null(compare)) compare <- list()
  compareOpts <- do.call(compareOptions, compareOpts)
  list(
    x = x,
    compare = compare,
    compareOpts = compareOpts
  )
}

#' Transform x in homogeneous format, list of antaresdatalist / opts
#' 
#' @param x list of data
#' 
#' @noRd
.giveListFormat <- function(x){
  if (.isSimOpts(x) | "antaresData" %in% class(x)){
    list(.rescoverFormat(x))
  }else{
    if ("list" %in% class(x)){
      lapply(x, .rescoverFormat)
    }else{
      stop("class x must be antaresData or simOptions or list")
    }
  }
}

#' Transform x in antaresdatalist / opts
#' 
#' @param x data
#' 
#' @noRd
.rescoverFormat <- function(x){
  if ("antaresData" %in% class(x))
  {
    re <- as.antaresDataList(x)
  }else{
    if (.isSimOpts(x)){
      re <- x
    }else{
      stop("class x must be antaresData or simOptions")
    }
  }
  re
}


#' Test opst
#' 
#' @param test if x is simOptions class
#' 
#' @noRd
.isSimOpts <- function(x){
  if ("simOptions" %in% class(x)){
    if (!is.null(x$h5path)){
      if (!file.exists(x$h5path)){
        warning(paste0("h5file does not exists for this study :",
                       x$studyName))
        return(FALSE)
      }else{
        return(TRUE)
      }
    }else{
      #opts but no h5 (TXT)
      return(TRUE)
    }
  }else{
    return(FALSE)
  }
}

#' Test lits opst
#' 
#' @param test if x is list of simOptions class
#' 
#' @noRd
.isListSimOpts <- function(x){
  if ("list" %in% class(x)){
    if (length(x) > 0){
      if (.isSimOpts(x[[1]])){
        return(TRUE)
      }else{
        return(FALSE)
      }
    }else{
      return(FALSE)
    }
  }else{
    return(FALSE)
  }
}
#' Test antaresData
#' 
#' @param x if x is antaresData class
#' 
#' @noRd
.isAntaresData <- function(x){
  "antaresData" %in% class(x)
}


#' Load h5 data
#' 
#' @param sharerequest, list of mcYearh_l, tables_l and timeSteph5_l
#' @param dta, antaresdatalist or opts, if antaresdatalist do nothing, if opts load data
#' @param areas character
#' @param links character
#' @param clusters character
#' @param districts character
#' 
#' @noRd
.loadH5Data <- function(sharerequest, dta, areas = NULL, links = NULL, clusters = NULL, 
                        districts = NULL, h5requestFilter = list()){
  if (.isSimOpts(dta)){
    gc()
    if (length(sharerequest$mcYearh_l) == 0) {mcYearh2 <- NULL}else{
      if ("all" %in% sharerequest$mcYearh_l){
        mcYearh2 <- "all"
      }else{
        mcYearh2 <- as.numeric(sharerequest$mcYearh_l)
      }
    }
    if (!is.null(sharerequest$tables_l))
    {
      if ("areas" %in% sharerequest$tables_l){
        areas <- "all"
      }
      if ("links" %in% sharerequest$tables_l){
        links <- "all"
      }
      if ("clusters" %in% sharerequest$tables_l){
        clusters <- "all"
      }
      if ("districts" %in% sharerequest$tables_l){
        districts <- "all"
      }
    }
    
    
    argS <- list(areas = areas, links = links, clusters = clusters, districts = districts, mcYears = mcYearh2,
                 timeStep = sharerequest$timeSteph5_l, opts = dta, showProgress = FALSE)
    argS[names(h5requestFilter)] <- h5requestFilter
    dt <- do.call(readAntares,
                  argS)
    
    dt <- as.antaresDataList(dt)
    for (i in 1:length(dt)){
      if (all(names(dt[[i]]) %in% .idCols(dt[[i]]))){
        dt[[i]] <- NULL
      }
    }
    dt
  }else{
    dta
  }
}




#' List of h5 params
#' 
#' @param X_I, list
#' @param xyCompare, character
#' 
#' @noRd
.h5ParamList <- function(X_I, xyCompare, h5requestFilter = NULL){
  listParam <- lapply(1:length(X_I), function(i){
    x <- X_I[[i]]
    if (.isSimOpts(x)){
      tmp <- .h5Inf(x)
      h5_filter <- h5requestFilter[[i]]
      h5_tables <- c("areas", "districts", "clusters", "links")
      if (!is.null(h5_filter)){
        if (!(is.null(h5_filter$areas) & is.null(h5_filter$districts) & 
           is.null(h5_filter$links) & is.null(h5_filter$clusters))){
          h5_tables <- c("areas", "districts", "clusters", "links")
          h5_tables <- h5_tables[which(c(!is.null(h5_filter$areas), !is.null(h5_filter$districts),
                                           !is.null(h5_filter$clusters), !is.null(h5_filter$links)))]
        }
      }
      tmp$tabl <- intersect(tmp$tabl, h5_tables)
      rhdf5::H5close()
      tmp
    }else{
      mcY <- unique(unlist(lapply(x, function(y){unique(y$mcYears)})))
      timeStepS <- attributes(x)$timeStep
      tabl <- names(x)
      list(
        timeStepS = timeStepS,
        mcYearS = mcY,
        tabl = tabl
      )
    }
  })
  res <- lapply(.transposeL(listParam), function(x){
    .compareOperation(x, xyCompare)
  })
  
  res$h5requestFilter <- h5requestFilter
  res
}

#' Load information from h5 file
#' 
#' @param x, opts
#' 
#' @noRd
.h5Inf <- function(x){
  fid <- rhdf5::H5Fopen(x$h5path)
  timeStepS <- .getTimeStep(fid)
  timeStepS <- as.character(timeStepS)
  mcYearS <- x$mcYears
  tabl <- .getTableInH5(fid, timeStepS[1])
  rhdf5::H5Fclose(fid)
  xPart = list(
    timeStepS = timeStepS,
    mcYearS = mcYearS,
    tabl = tabl
  )
}


.transposeL <- function(data){
  do.call(c, apply(do.call(rbind, data), 2, list)) 
}


.testXclassAndInteractive <- function(x, interactive){
  if (!"antaresData" %in% class(x) & !interactive){
    stop("You can at moment only use no interactive mode with one no h5 antares study.")
  }
}

#' Check for ref Study optsH5 or antaresData
#' 
#' @param x list of opts or antaresData 
#' @param refStudy an opts or antaresData or a list !
#' 
#' @noRd
.compare_with_ref_study <- function(x = NULL, refStudy = NULL){
  if (!is.null(refStudy)){
    # if x and refStudy are antaresDataList 
    if (is(refStudy, "antaresDataList") & is(x, "antaresDataList")){
      x <- compare(x = refStudy, y = x, method = "diff")
      return(x)
    } else if (is.list(refStudy)){
      #if refStudy is a list because ...
      #cleanH5 return a list (and no antaresData)
      x <- .compare_with_ref_study_one_element(x = x, refStudy = refStudy[[1]])
      return(x)
    } else {
      #to work with an antaresData (no optsH5)
      x <- .compare_with_ref_study_one_element(x = x, refStudy = refStudy)
      return(x)
    }

  }
  return(x)
}

#' Check for ref Study 
#' 
#' @param x list of opts or antaresData 
#' @param refStudy an opts or antaresData  
#' 
#' @noRd
.compare_with_ref_study_one_element <- function(x = NULL, refStudy = NULL){
  if (!(is(refStudy, "antaresData") | is(refStudy, "simOptions"))) stop("'refStudy' should be an object of class 'antaresData created with readAntares()' or an opts")
  #we cannot compare instances from simOptions
  if (!("simOptions" %in% class(x))){
    if (!("antaresData" %in% class(x))){
      for (i in 1:length(x)){
        x[[i]] <- compare(x = refStudy, y = x[[i]], method = "diff")
      }
      return(x)
    }else{
      #case where refStudy is an antaresDataTable 
      if (is(refStudy, "antaresDataTable")){
        #case where x is an antaresDataTable
        if (is(x, "antaresDataTable")){
          x <- compare(x = refStudy, y = x, method = "diff")
          return(x)
        }else{
          #case where x is an antaresDataList but with one element
          if (length(names(x)) == 1 | is(x, "antaresDataList")){
            x <- compare(x = refStudy, y = x[[1]], method = "diff")
            return(x)
          }else{
            stop("no case for compare")
          }
        }
      }else{
        stop("no case for compare?")
      }
      #case where refStudy is an antaresDataList 
      if ( (is(refStudy, "antaresDataList"))){
        if (is(x, "antaresDataList")){
          x <- compare(x = refStudy, y = x, method = "diff")
          return(x)
        }else{
          stop(" no case for compare ? ")
        }
      }
    }
  }
  stop(" no case for compare ? ")
}  
