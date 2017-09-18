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
  is_antares_study <- reactive({
    dir_files <- dir_files()
    all(c("output", "study.antares") %in% dir_files$name)
  })
  
  output$ctrl_is_antares_study <- reactive({
    is_antares_study()
  })
  outputOptions(output, "ctrl_is_antares_study", suspendWhenHidden = FALSE)
  
  # ig have study, update selectInput list
  observe({
    if(is_antares_study()){
      isolate({
        files = list.files(paste0(readDirectoryInput(session, 'directory'), "/output"), full.names = T)
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
      opts <- setSimulationPath(input$study_path)
    } else {
      NULL
    }
  })
  
  # control : have not null opts ?
  output$have_study <- reactive({
    !is.null(opts())
  })
  outputOptions(output, "have_study", suspendWhenHidden = FALSE)
  
  # update readAntares / opts parameters
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
      })
    }
  })
  
  #----------------
  # module
  #----------------
  
  modules <- reactiveValues(prodStack = NULL, exchangesStack = NULL, plotts = NULL, plotMap = NULL)
  
  ctrl_data <- reactiveValues(data_exchange = NULL, have_links = FALSE)
  
  data <- reactive({
    if(input$import_data > 0){
      isolate({
        if(!is.null(opts())){
          
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
                      select = NULL, mcYears = mcYears, timeStep = input$read_timeStep, 
                      opts = opts(), parallel = input$read_parallel,
                      simplify = TRUE, showProgress = FALSE)
          
          if(!is.null(input$read_links)){
            ctrl_data$data_exchange <- data
            ctrl_data$have_links <- TRUE
          } else {
            ctrl_data$have_links <- FALSE
          }
          data
        } else {
          NULL
        }
      })
    } else {
      NULL
    }
  })
  
  
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
            print("init")
            print(current_value)
            input_data$data[i, last_update := as.character(Sys.time())]
            print(input_data$data[i])
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
    !is.null(data())
  })
  outputOptions(output, "have_data", suspendWhenHidden = FALSE)
  
  # control : have link in data
  output$have_links <- reactive({
    ctrl_data$have_links
  })
  outputOptions(output, "have_links", suspendWhenHidden = FALSE)
  
  observe({
    data <- data()
    if(!is.null(data)){
      updateNavbarPage(session, inputId = "nav-id", selected = "prodStack")
    }
  })
  
  observe({
    data <- data()
    if(!is.null(data)){
      if(is.null(modules$prodStack)){
        modules$prodStack <- prodStack(data, unit = "GWh", .runApp = FALSE)
      }
      if(is.null(modules$exchangesStack)){
        if(!is.null(ctrl_data$data_exchange)){
          modules$exchangesStack <- exchangesStack(ctrl_data$data_exchange, .runApp = FALSE)
        }
      }
      if(is.null(modules$plotts)){
        modules$plotts <- plot(data, .runApp = FALSE)
      }
    }
  })
  
  
  # prodStack
  observe({
    if(!is.null(modules$prodStack)){
      mwModule(id = "prodStack",  modules$prodStack, x = data)
    }
  })
  
  observe({
    if(!is.null(modules$exchangesStack)){
      mwModule(id = "exchangesStack",  modules$exchangesStack, x = reactive(ctrl_data$data_exchange))
    }
  }, priority = 1)
  
  # plotts
  observe({
    if(!is.null(modules$plotts)){
      mwModule(id = "plotts",  modules$plotts, x = data)
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
        modules$plotMap <- plotMap(data, ml, colAreaVar = "BALANCE", .runApp = FALSE)
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
  
}
