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

output$directory_message <- renderText({
  if(input$directory == 0){
    "Please first choose a folder with antares output"
  } else {
    "No antares output found in directory"
  }
})

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
      updateSelectInput(session, "study_path", "Select a simulation", choices = choices)
    })
  }
})

# init opts after validation
opts <- reactive({
  if(input$init_sim > 0){
    opts <- 
      tryCatch({
        setSimulationPath(isolate(input$study_path))
      }, error = function(e){
        showModal(modalDialog(
          title = "Error setting file",
          easyClose = TRUE,
          footer = NULL,
          paste("Directory/file is not an Antares study : ", e, sep = "\n")
        ))
        NULL
      })
    if(!is.null(opts)){
      if(is.null(opts$h5)){
        opts$h5 <- FALSE
      }
      # bad h5 control
      if(opts$h5){
        if(length(setdiff(names(opts), c("h5", "h5path"))) == 0){
          showModal(modalDialog(
            easyClose = TRUE,
            footer = NULL,
            "Invalid h5 file : not an Antares study."
          ))
          opts <- NULL
        }
      }
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

observe({
  if(input$init_sim > 0){
      updateTabsetPanel(session, inputId = "args", selected = "Read data")
  }
})


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
      
      # removeVirtualAreas
      updateSelectInput(session, "rmva_storageFlexibility", "storageFlexibility :", choices = opts$areaList, selected = NULL)
      updateSelectInput(session, "rmva_production", "production :", choices = opts$areaList, selected = NULL)
      
    })
  }
})

observe({
  RL <- input$read_links
  isolate({
    if(!is.null(RL)) {
      if(length(RL) == 0) {
        updateCheckboxInput(session, "read_linkCapacity", "linkCapacity", FALSE)
      }
    } else {
      updateCheckboxInput(session, "read_linkCapacity", "linkCapacity", FALSE)
    }
  })
  
})

observe({
  RC <- input$read_clusters
  opts <- opts()
  isolate({
    if(!is.null(RC)) {
      if(length(RC) == 0) {
        updateCheckboxInput(session, "read_thermalAvailabilities", "thermalAvailabilities", FALSE)
        updateCheckboxInput(session, "read_thermalModulation", "thermalModulation", FALSE)
      }
    } else {
      updateCheckboxInput(session, "read_thermalAvailabilities", "thermalAvailabilities", FALSE)
      updateCheckboxInput(session, "read_thermalModulation", "thermalModulation", FALSE)
    }
  })
  
})

observe({
  opts <- opts()
  if(!is.null(opts)) {
    isolate({
      # browser()
      if(!opts$parameters$general$`year-by-year`){
        updateRadioButtons(session, "read_type_mcYears", "mcYears :",
                           c("synthetic"), inline = TRUE)
        updateCheckboxInput(session, "read_hydroStorage", "hydroStorage", FALSE)
      } else {
        updateRadioButtons(session, "read_type_mcYears", "mcYears :",
                           c("synthetic", "all", "custom"), inline = TRUE)
      }
    })
  }
})