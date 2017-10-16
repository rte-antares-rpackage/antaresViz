function(input, output, session) {
  
  #----------------
  # set / read data
  #----------------
  source("src/server/01_set_read_data.R", local = T)
  
  #----------------
  # module
  #----------------
  
  modules <- reactiveValues(prodStack = NULL, exchangesStack = NULL, plotts = NULL, plotMap = NULL)
  # all data loaded by user, with informations
  list_data_all <- reactiveValues(antaresDataList = list(), params = list(), have_links = c(), have_areas = c())
  
  # current data for module stack, plotTS (must have areas)
  list_data_areas <- reactiveValues(antaresDataList = list(), h5requestFiltering = list())
  
  # current data for module exchange (must have link)
  list_data_links <- reactiveValues(antaresDataList = list(), h5requestFiltering = list())
  
  # current data for module exchange (must have areas and/or link)
  list_data_map <- reactiveValues(antaresDataList = list(), h5requestFiltering = list())
  
  # set of controls
  list_data_controls <- reactiveValues(have_links = FALSE, have_areas = FALSE, 
                                       n_links = -1, n_areas = -1, n_maps = -1)

  
  #-----------------
  # Importation de nouvelles donnees
  #-----------------
  observe({
    if(input$import_data > 0){
      isolate({
        if(!is.null(opts())){
          # not a .h5 file, so read data
          if(!opts()$h5){
            # Treat mcYears
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
            
            n_list <- length(list_data_all$antaresDataList) + 1
            list_data_all$antaresDataList[[n_list]] <- data
            
          } else {
            params <- list(
              areas = input$read_areas, links = input$read_links, 
              clusters = input$read_clusters, districts = input$read_districts,
              select = input$read_select
            )
            
            # a .h5 file, so return opts...
            n_list <- length(list_data_all$antaresDataList) + 1
            list_data_all$antaresDataList[[n_list]] <- opts()
          }
          
          # write params and links control
          list_data_all$params[[n_list]] <- params
          if(!is.null(input$read_links)){
            list_data_all$have_links[n_list] <- TRUE
          } else {
            list_data_all$have_links[n_list] <- FALSE
          }
          have_areas <- is.null(input$read_areas) & is.null(input$read_links) & is.null(input$read_clusters) & 
            is.null(input$read_districts) | !is.null(input$read_areas)
          if(have_areas){
            list_data_all$have_areas[n_list] <- TRUE
          } else {
            list_data_all$have_areas[n_list] <- FALSE
          }
          names(list_data_all$antaresDataList)[[n_list]] <- current_study_path()
          
        }
      })
    }
  })
  
  # control : have data
  output$have_data <- reactive({
    length(list_data_all$antaresDataList) > 0
  })
  outputOptions(output, "have_data", suspendWhenHidden = FALSE)
  
  ind_keep_list_data <- reactive({
    if(input$update_module > 0){
      isolate({
        names_input <- names(input)
        keep_input <- names_input[grepl("^list_study_check", names_input)]
        keep_input <- keep_input[as.numeric(gsub("list_study_check", "", keep_input)) <= length(list_data_all$antaresDataList)]
        if(length(keep_input) > 0){
          keep_input <- sort(keep_input)
          final_keep <- sapply(keep_input, function(x){
            input[[x]]
          })
          which(final_keep)
        } else {
          NULL
        }
      })
    } else {
      NULL
    }
  })
  
  #------------------
  # prodStack, plotTS & stackExchange
  #------------------
  observe({
    ind_keep_list_data <- ind_keep_list_data()
    isolate({
      if(input$update_module > 0){
        if(is.null(ind_keep_list_data)){
          showModal(modalDialog(
            easyClose = TRUE,
            footer = NULL,
            "No study selected"
          ))
        } else {
          # plotts and prodStack
          ind_areas <- intersect(which(list_data_all$have_areas), ind_keep_list_data)
          if(length(ind_areas) > 0){
            if(length(ind_areas) != list_data_controls$n_areas){
              # module prodStack
              modules$prodStack <- prodStack(list_data_all$antaresDataList[ind_areas], 
                                             unit = "GWh", interactive = TRUE, .runApp = FALSE)
              
              # module plotts
              modules$plotts <- plot(list_data_all$antaresDataList[ind_areas], 
                                     interactive = TRUE, .runApp = FALSE)
            }
            list_data_areas$antaresDataList <- list_data_all$antaresDataList[ind_areas]
            list_data_areas$h5requestFiltering <- list_data_all$params[ind_areas]
            list_data_controls$n_areas <- length(ind_areas)
            list_data_controls$have_areas <- TRUE
          } else {
            list_data_controls$have_areas <- FALSE
          }

          # exchange
          ind_links <- intersect(which(list_data_all$have_links), ind_keep_list_data)
          if(length(ind_links) > 0){
            if(length(ind_links) != list_data_controls$n_links){
              modules$exchangesStack <- exchangesStack(list_data_all$antaresDataList[ind_links], 
                                                       interactive = TRUE, .runApp = FALSE)
            }
            list_data_links$antaresDataList <- list_data_all$antaresDataList[ind_links]
            list_data_links$h5requestFiltering <- list_data_links$params[ind_links]
            list_data_controls$n_links <- length(ind_links)
            list_data_controls$have_links <- TRUE
          } else {
            list_data_controls$have_links <- FALSE
          }

          if(!list_data_controls$have_areas & !list_data_controls$have_links){
            showModal(modalDialog(
              easyClose = TRUE,
              footer = NULL,
              "No study with at least one area and/or link selected"
            ))
          }
        }
      }
    })
  })
  
  # prodStack
  observe({
    if(!is.null(modules$prodStack)){
      mwModule(id = "prodStack",  modules$prodStack, 
               x = reactive(list_data_areas$antaresDataList), 
               h5requestFiltering = reactive(list_data_areas$h5requestFiltering))
    }
  })
  
  # exchange
  observe({
    if(!is.null(modules$exchangesStack)){
      mwModule(id = "exchangesStack",  modules$exchangesStack, 
               x = reactive(list_data_links$antaresDataList), 
               h5requestFiltering = reactive(list_data_links$h5requestFiltering))
    }
  })
  
  # plotts
  observe({
    if(!is.null(modules$plotts)){
      mwModule(id = "plotts",  modules$plotts, 
               x = reactive(list_data_areas$antaresDataList), 
               h5requestFiltering = reactive(list_data_areas$h5requestFiltering))
    }
  })
  
  # control : have link in data
  output$have_data_links <- reactive({
    list_data_controls$have_links
  })
  outputOptions(output, "have_data_links", suspendWhenHidden = FALSE)
  
  # control : have areas in data
  output$have_data_areas <- reactive({
    list_data_controls$have_areas
  })
  outputOptions(output, "have_data_areas", suspendWhenHidden = FALSE)
  
  observe({
        if(list_data_controls$have_areas & length(list_data_areas$antaresDataList) > 0){
          updateNavbarPage(session, inputId = "nav-id", selected = "prodStack")
        } else if(list_data_controls$have_links & length(list_data_links$antaresDataList) > 0){
          updateNavbarPage(session, inputId = "nav-id", selected = "exchangesStack")
        }
  })
  
  #------------
  # plotMap
  #------------
  
  layout <- reactive({
    if(!is.null(opts())){
      readLayout(opts = opts())
    }else{
      NULL
    }
  })
  
  ml <- callModule(antaresViz:::changeCoordsServer, "ml", layout, 
                   what = reactive("areas"), stopApp = FALSE)
  
  observe({
    ind_keep_list_data <- ind_keep_list_data()
    ml <- ml()
    ml2 <<- ml
    isolate({
      if(input$update_module > 0){
        if(!is.null(ind_keep_list_data)){
          ind_areas <- intersect(which(list_data_all$have_areas), ind_keep_list_data)
          ind_links <- intersect(which(list_data_all$have_links), ind_keep_list_data)
          ind_map <- unique(sort(c(ind_areas, ind_links)))
          
          if(length(ind_map) > 0){
            if(!is.null(ml)){
              if(length(ind_map) != list_data_controls$n_maps & !is.null(ml)){
                # module plotMap
                modules$plotMap <- plotMap(list_data_all$antaresDataList[ind_map], ml, 
                                           colAreaVar = "BALANCE", interactive = TRUE, 
                                           xyCompare = "intersect", .runApp = FALSE)
              }
              list_data_map$antaresDataList <- list_data_all$antaresDataList[ind_map]
              list_data_map$h5requestFiltering <- list_data_all$params[ind_map]
              list_data_controls$n_maps <- length(ind_map)
            }
          }
        }
      }
    })
  })
  

  observe({
    if(!is.null(modules$plotMap)){
      mwModule(id = "plotMap",  modules$plotMap, x = reactive(list_data_map$antaresDataList), 
               mapLayout = ml, h5requestFiltering = reactive(list_data_map$h5requestFiltering))
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
    list_data <- list_data_all$antaresDataList
    if(length(list_data) > 0){
      isolate({
        # affichage du nom de l'etude
        study <- lapply(1:length(list_data), function(i) {
          study_name <- paste0("list_study_", i)
          div(
            # conditionalPanel(condition = paste0("input.list_study_rm", i, " < 1"),
            h4(textOutput(study_name)), style = 'height:24px', align = "center")
          # )
        })
        # checkbox de selection
        check_list <- lapply(1:length(list_data), function(i) {
          check_name <- paste0("list_study_check", i)
          div(
            # conditionalPanel(condition = paste0("input.list_study_rm", i, " < 1"),
            checkboxInput(check_name, "Include study in analysis", value = TRUE), align = "center")
          # )
        })
        # bouton pour afficher les parametres
        params_list <- lapply(1:length(list_data), function(i) {
          btn_name <- paste0("list_study_params", i)
          div(
            # conditionalPanel(condition = paste0("input.list_study_rm", i, " < 1"),
            actionButton(btn_name, "View parameters"), align = "center")
          # )
        })
        # bouton pour supprimer les donnees
        rm_list <- lapply(1:length(list_data), function(i) {
          btn_name <- paste0("list_study_rm", i)
          div(
            # conditionalPanel(condition = paste0("input.list_study_rm", i, " < 1"),
            actionButton(btn_name, "Remove study"), align = "center")
          # )
        })
        # format et retour
        fluidRow(
          column(3, do.call(tagList, study)),
          column(3, do.call(tagList, params_list)),
          column(3, do.call(tagList, check_list)),
          column(3, do.call(tagList, rm_list))
        )
      })
    }else {
      # element vide si pas de donnees
      fluidRow(
        # div(h4("Veuillez sélectionner des indicateurs et valider votre choix", style = "color: darkblue;"), align = "center")
      )
    }
  })
  
  # creation des outputs
  # - titre de l'etude
  # - print des parametres
  observe({
    # lancement lors de la recuperation des donnees formatees
    list_data_tmp <- list_data_all$antaresDataList
    if(length(list_data_tmp) > 0){
      isolate({
        ctrl <- lapply(1:length(list_data_tmp), function(i) {
          study_name <- paste0("list_study_", i)
          study_params <- paste0("list_study_params", i)
          output[[study_name]] <- renderText({
            paste0("Study : ", names(list_data_tmp)[i])
          })
          
          output[[study_params]] <- renderPrint({
            str(list_data_all$params[[i]])
          })
        })
      })
    }
  })
  
  # observe locaux pour l'affichage des parametres
  # et pour la suppression des etudes
  for(j in 1:16){
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
            isolate({
              list_data_all$antaresDataList[l_j] <- NULL
              list_data_all$params[l_j] <- NULL
              gc()
            })
          }
        }
      })
    })
  }
  
  #----------------
  # shared inputs
  #----------------
  source("src/server/03_shared_input.R", local = T)
  
}
