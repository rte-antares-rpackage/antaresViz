function(input, output, session) {
  
  #----------------
  # set / read data
  #----------------
  
  # observe directory 
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch
        path = choose.dir(default = readDirectoryInput(session, 'directory'))
        updateDirectoryInput(session, 'directory', value = path)
      }
    }
  )
  
  # list files in directory
  dir_files <- reactive({
    files = list.files(readDirectoryInput(session, 'directory'), full.names = T)
    data.frame(name = basename(files), file.info(files))
  })
  
  # have antares study in directory ?
  is_antares_results <- reactive({
    dir_files <- dir_files()
    is_h5 <- any(grepl(".h5$", dir_files$name))
    is_study <- all(c("output", "study.antares") %in% dir_files$name)
    list(is_h5 = is_h5, is_study = is_study)
  })
  
  output$ctrl_is_antares_study <- reactive({
    is_antares_results()$is_study
  })
  
  output$ctrl_is_antares_h5 <- reactive({
    is_antares_results()$is_h5
  })
  
  outputOptions(output, "ctrl_is_antares_study", suspendWhenHidden = FALSE)
  outputOptions(output, "ctrl_is_antares_h5", suspendWhenHidden = FALSE)
  
  # if have study, update selectInput list
  observe({
    is_antares_results <- is_antares_results()
    if(is_antares_results$is_h5 | is_antares_results$is_study){
      isolate({
        if(is_antares_results$is_study){
          files = list.files(paste0(readDirectoryInput(session, 'directory'), "/output"), full.names = T)
        } 
        if(is_antares_results$is_h5){
          files = list.files(readDirectoryInput(session, 'directory'), pattern = ".h5$", full.names = T)
        } 
        if(length(files) > 0){
          files <- data.frame(name = basename(files), file.info(files))
          choices <- rownames(files)
          names(choices) <- files$name
        } else {
          choices <- NULL
        }
        updateSelectInput(session, "study_path", "Study", choices = choices)
      })
    }
  })
  
  # init opts after validation
  opts <- reactive({
    if(input$init_sim > 0){
      opts <- setSimulationPath(isolate(input$study_path))
      if(is.null(opts$h5)){
        opts$h5 <- FALSE
      }
      opts
    } else {
      NULL
    }
  })
  
  output$current_opts_h5 <- reactive({
    opts()$h5
  })
  
  outputOptions(output, "current_opts_h5", suspendWhenHidden = FALSE)
  
  current_study_path <- reactive({
    if(input$init_sim > 0){
      rev(unlist(strsplit(isolate(input$study_path), "/")))[1]
    }
  })
  
  output$current_opts <- renderText({
    current_study_path()
  })
  
  # control : have not null opts ?
  output$have_study <- reactive({
    !is.null(opts())
  })
  
  outputOptions(output, "have_study", suspendWhenHidden = FALSE)
  
  #--------------------------------------
  # update readAntares / opts parameters
  #--------------------------------------
  observe({
    opts <- opts()
    if(!is.null(opts)){
      isolate({
        # areas
        areas <- c("all", opts$areaList)
        updateSelectInput(session, "read_areas", "Areas :", choices = areas, selected = areas[1])
        
        # links
        links <- c("all", opts$linkList)
        updateSelectInput(session, "read_links", "Links :", choices = links, selected = NULL)
        
        # clusters
        clusters <- c("all", opts$areasWithClusters)
        updateSelectInput(session, "read_clusters", "Clusters :", choices = clusters, selected = NULL)
        
        # districts
        districts <- c("all", opts$districtList)
        updateSelectInput(session, "read_districts", "Districts :", choices = districts, selected = NULL)
        
        # mcYears
        mcy <- c(opts$mcYears)
        updateSelectInput(session, "read_mcYears", "mcYears :", choices = mcy, selected = mcy[1])
        
        # select
        slt <- unique(do.call("c", opts$variables))
        updateSelectInput(session, "read_select", "Select :", choices = slt, selected = NULL)
        
      })
    }
  })
  
  #----------------
  # module
  #----------------
  
  modules <- reactiveValues(prodStack = NULL, exchangesStack = NULL, plotts = NULL, plotMap = NULL)
  
  list_data <- reactiveValues(antaresDataList = list(), params = list())
  list_data_reset <- c()
  
  ctrl_data <- reactiveValues(data_exchange = NULL, have_links = FALSE)
  
  observe({
    if(input$import_data > 0){
      isolate({
        
        if(length(list_data_reset) > 0){
          print("reset ici")
          list_data$antaresDataList[list_data_reset] <- NULL
          list_data$params[list_data_reset] <- NULL
          list_data_reset <<- c()
          gc()
        }
        
        if(!is.null(opts())){
          # not a .h5 file, so read data
          if(!opts()$h5){
            if(input$read_type_mcYears == "synthetic"){
              mcYears <- NULL
            } else if(input$read_type_mcYears == "all"){
              mcYears <- "all"
            } else {
              mcYears <- as.numeric(input$read_mcYears)
            }
            
            # import data
            data <- readAntares(areas = input$read_areas, links = input$read_links, clusters = input$read_clusters,
                                districts = input$read_districts, misc = input$read_misc, 
                                thermalAvailabilities = input$read_thermalAvailabilities,
                                hydroStorage = input$read_hydroStorage, hydroStorageMaxPower = input$read_hydroStorageMaxPower, 
                                reserve = input$read_reserve, 
                                linkCapacity = if(!is.null(input$read_links)) input$read_linkCapacity else FALSE, 
                                mustRun = input$read_mustRun, thermalModulation = input$read_thermalModulation,
                                select = input$read_select, mcYears = mcYears, timeStep = input$read_timeStep, 
                                opts = opts(), parallel = input$read_parallel,
                                simplify = TRUE, showProgress = FALSE)
            
            # save params
            params <- list(
              areas = input$read_areas, links = input$read_links, clusters = input$read_clusters,
              districts = input$read_districts, misc = input$read_misc, 
              thermalAvailabilities = input$read_thermalAvailabilities,
              hydroStorage = input$read_hydroStorage, hydroStorageMaxPower = input$read_hydroStorageMaxPower, 
              reserve = input$read_reserve, 
              linkCapacity = if(!is.null(input$read_links)) input$read_linkCapacity else FALSE, 
              mustRun = input$read_mustRun, thermalModulation = input$read_thermalModulation,
              select = input$read_select, mcYears = mcYears, timeStep = input$read_timeStep, 
              parallel = input$read_parallel
            )
            
            if(!is.null(input$read_links)){
              ctrl_data$data_exchange <- data
              ctrl_data$have_links <- TRUE
            } else {
              ctrl_data$have_links <- FALSE
            }
            
            n_list <- length(list_data$antaresDataList) + 1
            list_data$antaresDataList[[n_list]] <- data
            list_data$params[[n_list]] <- params
            names(list_data$antaresDataList)[[n_list]] <- current_study_path()
            
          } else {
            if(!is.null(input$read_links)){
              ctrl_data$data_exchange <- opts()
              ctrl_data$have_links <- TRUE
            } else {
              ctrl_data$have_links <- FALSE
            }
            
            params <- list(
              areas = input$read_areas, links = input$read_links, 
              clusters = input$read_clusters, districts = input$read_districts,
              select = input$read_select
            )
            
            # a .h5 file, so return opts...
            n_list <- length(list_data$antaresDataList) + 1
            list_data$antaresDataList[[n_list]] <- opts()
            list_data$params[[n_list]] <- params
            names(list_data$antaresDataList)[[n_list]] <- current_study_path()
            
          }
        }
      })
    }
  })
  
  # observe({
  #   print(str(list_data$antaresDataList))
  #   print(str(list_data$params))
  # })
  input_data <- reactiveValues(data = build_input_data(shared_input))
  
  # observe({
  #   input$import_data
  #   input_data$data[, last_update := NA]
  # })
  
  lapply(1:nrow(shared_input), function(i){
    observe({
      current_input_data <- isolate(input_data$data)
      current_value <- input[[current_input_data[i, input_id]]]
      if(!is.null(current_value)){
        isolate({
          # print("init")
          # print(current_value)
          input_data$data[i, last_update := as.character(Sys.time())]
          # print(input_data$data[i])
        })
      }
    })
  })
  
  observe({
    current_nav <- input[['nav-id']]
    current_input_data <- isolate(input_data$data)
    data_shared_input <- current_input_data[panel %in% current_nav]
    if(nrow(data_shared_input) > 0){
      for(i in 1:nrow(data_shared_input)){
        last_update_input <- current_input_data[!panel %in% current_nav & 
                                                  input%in%data_shared_input[i, input] & 
                                                  !is.na(last_update)][order(last_update, decreasing = TRUE)]
        if(nrow(last_update_input) >= 1){
          if(data_shared_input[i, type] %in% "dateRangeInput"){
            updateDateRangeInput(session, data_shared_input[i, input_id], 
                                 start = isolate({input[[last_update_input[1, input_id]]][1]}), 
                                 end = isolate({input[[last_update_input[1, input_id]]][2]}))
          } else if(data_shared_input[i, type] %in% "selectInput"){
            updateSelectInput(session, data_shared_input[i, input_id], 
                              selected = isolate({input[[last_update_input[1, input_id]]][1]}))
          }
        }
      }
    }
  })
  
  # observe({
  #   print(names(input))
  #   print(input_data$data)
  # })
  
  # control : have data
  output$have_data <- reactive({
    length(list_data$antaresDataList) > 0
  })
  outputOptions(output, "have_data", suspendWhenHidden = FALSE)
  
  # control : have link in data
  output$have_links <- reactive({
    ctrl_data$have_links
  })
  outputOptions(output, "have_links", suspendWhenHidden = FALSE)
  
  observe({
    if(input$update_module > 0){
      isolate({
        data <- list_data$antaresDataList
        if(length(data) > 0){
          updateNavbarPage(session, inputId = "nav-id", selected = "prodStack")
        }
      })
    }
  })
  
  length_antaresDataList <- reactiveValues(n = -1)
  
  observe({
    
    if(input$update_module > 0){
      isolate({
        print("update module")
        data <- list_data$antaresDataList
        # list_data$antaresDataList[[i]] <- NULL
        # list_data_reset <- list_data$reset
        if(length(data) > 0){
          data <- data[setdiff(1:length(data), list_data_reset)]
        }
        if(length(data) > 0){
          print("have data")
          if(length(data) != length_antaresDataList$n){
            print("new module")
            # if(is.null(modules$prodStack)){
            modules$prodStack <- prodStack(data, unit = "GWh", interactive = TRUE, .runApp = FALSE)
            # }
            # if(is.null(modules$exchangesStack)){
            if(!is.null(ctrl_data$data_exchange)){
              modules$exchangesStack <- exchangesStack(ctrl_data$data_exchange, interactive = TRUE, .runApp = FALSE)
            }
            # }
            # if(is.null(modules$plotts)){
            modules$plotts <- plot(data, interactive = TRUE, .runApp = FALSE)
            # }
            
            length_antaresDataList$n <- length(data)
            
            if(length(list_data_reset) > 0){
              print("reset")
              print(list_data_reset)
              # list_data$reset <- c()
              list_data$antaresDataList[list_data_reset] <- NULL
              list_data$params[list_data_reset] <- NULL
              list_data_reset <<- c()
              gc()
            }
            
          }
        }
      })
    }
  })
  
  
  
  # prodStack
  observe({
    # if(input$update_module > 0){
    #   isolate({
    if(!is.null(modules$prodStack)){
      mwModule(id = "prodStack",  modules$prodStack, x = isolate(reactive(list_data$antaresDataList)))
    }
    # })
    # }
  })
  
  observe({
    if(input$update_module > 0){
      isolate({
        if(!is.null(modules$exchangesStack)){
          mwModule(id = "exchangesStack",  modules$exchangesStack, x = reactive(ctrl_data$data_exchange))
        }
      })
    }
  }, priority = 1)
  
  # plotts
  observe({
    if(input$update_module > 0){
      isolate({
        if(!is.null(modules$plotts)){
          mwModule(id = "plotts",  modules$plotts, x = reactive(list_data$antaresDataList))
        }
      })
    }
  })
  
  # plotMap
  layout <- reactive({
    if(!is.null(opts())){
      readLayout(opts = opts())
    }else{
      NULL
    }
  })
  
  ml <- callModule(antaresViz:::changeCoordsServer, "ml", layout, what = reactive("areas"), 
                   map = NULL, stopApp = FALSE)
  
  observe({
    data <- data()
    ml <- ml()
    if(!is.null(data) & !is.null(ml)){
      if(is.null(modules$plotMap)){
        modules$plotMap <- plotMap(data, ml, colAreaVar = "BALANCE", interactive = TRUE, .runApp = FALSE)
      }
    }
  })
  
  observe({
    if(!is.null(modules$plotMap)){
      mwModule(id = "plotMap",  modules$plotMap, x = data, mapLayout = ml)
    }
  })
  
  observe({
    if(!is.null(input[['ml-done']])){
      if(input[['ml-done']] > 0){
        updateNavbarPage(session, inputId = "nav-id", selected = "Map")
      }
    }
  })
  
  
  #------------------
  # gestion de la liste
  #------------------
  
  output$info_list <- renderUI({
    # lancement lors de la recuperation des donnees formatees
    list_data_tmp <- list_data$antaresDataList
    if(length(list_data_tmp) > 0){
      isolate({
        # liste contenant tous les graphiques UI
        study <- lapply(1:length(list_data_tmp), function(i) {
          study_name <- paste0("list_study_", i)
          div(
            conditionalPanel(condition = paste0("input.list_study_rm", i, " < 1"),
                             h4(textOutput(study_name)), align = "center")
          )
        })
        
        
        check_list <- lapply(1:length(list_data_tmp), function(i) {
          check_name <- paste0("list_study_check", i)
          
          div(
            conditionalPanel(condition = paste0("input.list_study_rm", i, " < 1"),
                             checkboxInput(check_name, "Include study in analysis", value = TRUE), align = "center")
          )
        })
        
        params_list <- lapply(1:length(list_data_tmp), function(i) {
          btn_name <- paste0("list_study_params", i)
          
          div(conditionalPanel(condition = paste0("input.list_study_rm", i, " < 1"),
                               actionButton(btn_name, "View parameters"), align = "center")
          )
        })
        
        rm_list <- lapply(1:length(list_data_tmp), function(i) {
          btn_name <- paste0("list_study_rm", i)
          
          div(conditionalPanel(condition = paste0("input.list_study_rm", i, " < 1"),
                               actionButton(btn_name, "Remove study"), align = "center")
          )
        })
        
        # si plusieurs graphiques, structure en deux colonnes
        fluidRow(
          column(3,
                 do.call(tagList, study)
          ),
          column(3,
                 do.call(tagList, params_list)
          ),
          column(3,
                 do.call(tagList, check_list)
          ),
          column(3,
                 do.call(tagList, rm_list)
          )
        )
      })
    }else {
      # element vide si pas de donnees
      fluidRow(
        # div(h4("Veuillez sélectionner des indicateurs et valider votre choix", style = "color: darkblue;"), align = "center")
      )
    }
  })
  
  for(j in 1:10){
    local({
      l_j <- j
      observe({
        if(!is.null(input[[paste0("list_study_params", l_j)]])){
          if(input[[paste0("list_study_params", l_j)]] > 0){
            showModal(modalDialog(
              easyClose = TRUE,
              footer = NULL,
              verbatimTextOutput(paste0("list_study_params", l_j))
            ))
          }
        }
      })
      
      observe({
        if(!is.null(input[[paste0("list_study_rm", l_j)]])){
          if(input[[paste0("list_study_rm", l_j)]] > 0){
            list_data_reset <<- unique(c(list_data_reset, l_j))
          }
        }
      })
    })
  }
  
  observe({
    # lancement lors de la recuperation des donnees formatees
    list_data_tmp <- list_data$antaresDataList
    if(length(list_data_tmp) > 0){
      isolate({
        ctrl <- lapply(1:length(list_data_tmp), function(i) {
          study_name <- paste0("list_study_", i)
          study_params <- paste0("list_study_params", i)
          output[[study_name]] <- renderText({
            paste0("Study : ", names(list_data_tmp)[i])
          })
          
          output[[study_params]] <- renderPrint({
            str(list_data$params[[i]])
          })
        })
      })
    }
  })
}
