function(input, output, session) {
  
  #----------------
  # set / read data
  #----------------
  source("src/server/01_set_read_data.R", local = T)
  
  #----------------
  # shared parameters
  #----------------
  
  modules <- reactiveValues(prodStack = NULL, exchangesStack = NULL, plotts = NULL, plotMap = NULL)
  
  # all data loaded by user, with informations
  list_data_all <- reactiveValues(antaresDataList = list(), params = list(), 
                                  have_links = c(), have_areas = c(), opts = list())
  
  # current data for module stack, plotTS (must have areas)
  list_data_areas <- reactiveValues(antaresDataList = list(), h5requestFiltering = list())
  
  # current data for module exchange (must have link)
  list_data_links <- reactiveValues(antaresDataList = list(), h5requestFiltering = list())
  
  # current data for module exchange (must have areas and/or link)
  list_data_map <- reactiveValues(antaresDataList = list(), h5requestFiltering = list(), opts = list())
  
  # set of controls
  list_data_controls <- reactiveValues(have_links = FALSE, have_areas = FALSE, 
                                       n_links = -1, n_areas = -1, n_maps = -1)

  
  #-----------------
  # Importation de nouvelles donnees
  #-----------------
  
  source("src/server/02_load_data.R", local = T)
  
  #----------------
  # Dataset selection
  #----------------
  source("src/server/03_data_selection.R", local = T)
  
  #-----------------
  # modules
  #-----------------
  
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
              modules$prodStack <- prodStack(list_data_all$antaresDataList[ind_areas], xyCompare = "union",
                                             unit = "GWh", interactive = TRUE, 
                                             .updateBtn = TRUE, .runApp = FALSE)
              
              # module plotts
              modules$plotts <- plot(list_data_all$antaresDataList[ind_areas], xyCompare = "union",
                                     interactive = TRUE, .updateBtn = TRUE, .runApp = FALSE)
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
              modules$exchangesStack <- exchangesStack(list_data_all$antaresDataList[ind_links], xyCompare = "union",
                                                       interactive = TRUE, .updateBtn = TRUE, .runApp = FALSE)
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
      if(class(opts()) %in% "simOptions"){
        readLayout(opts = opts())
      } else {
        NULL
      }
    }else{
      NULL
    }
  })
  
  ml <- callModule(antaresViz:::changeCoordsServer, "ml", layout, 
                   what = reactive("areas"), stopApp = FALSE)
  
  observe({
    ind_keep_list_data <- ind_keep_list_data()
    ml <- ml()
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
                                           xyCompare = "union", .updateBtn = TRUE, .runApp = FALSE)
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
  

  #----------------
  # shared inputs
  #----------------
  source("src/server/04_shared_input.R", local = T)
  
  observe({
    if(input$quit > 0){
      stopApp()
    }
  })
}
