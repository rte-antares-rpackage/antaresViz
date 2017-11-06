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
        ind_areas <- ind_keep_list_data$ind_areas
        if(length(ind_areas) > 0){
          if(length(ind_areas) != list_data_controls$n_areas){
            # init / re-init module prodStack
            modules$prodStack <- prodStack(list_data_all$antaresDataList[ind_areas], xyCompare = "union",
                                           unit = "GWh", interactive = TRUE, .updateBtn = TRUE, 
                                           .updateBtnInit = TRUE, .runApp = FALSE)
            
            # init / re-init module plotts
            modules$plotts <- plot(list_data_all$antaresDataList[ind_areas], xyCompare = "union",
                                   interactive = TRUE, .updateBtn = TRUE, 
                                   .updateBtnInit = TRUE, .runApp = FALSE)
          }
          
          # save data and params
          list_data_areas$antaresDataList <- list_data_all$antaresDataList[ind_areas]
          list_data_areas$h5requestFiltering <- list_data_all$params[ind_areas]
          list_data_controls$n_areas <- length(ind_areas)
          list_data_controls$have_areas <- TRUE
        } else {
          list_data_controls$have_areas <- FALSE
        }
        
        # exchange
        ind_links <- ind_keep_list_data$ind_links
        if(length(ind_links) > 0){
          if(length(ind_links) != list_data_controls$n_links){
            # init / re-init module exchangesStack
            modules$exchangesStack <- exchangesStack(list_data_all$antaresDataList[ind_links], xyCompare = "union",
                                                     interactive = TRUE, .updateBtn = TRUE, 
                                                     .updateBtnInit = TRUE, .runApp = FALSE)
          }
          # save data and params
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

# update prodStack module
observe({
  if(!is.null(modules$prodStack)){
    mwModule(id = "prodStack",  modules$prodStack, 
             x = reactive(list_data_areas$antaresDataList), 
             h5requestFiltering = reactive(list_data_areas$h5requestFiltering))
  }
})

# update exchange module
observe({
  if(!is.null(modules$exchangesStack)){
    mwModule(id = "exchangesStack",  modules$exchangesStack, 
             x = reactive(list_data_links$antaresDataList), 
             h5requestFiltering = reactive(list_data_links$h5requestFiltering))
  }
})

# update plotts module
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

# change page
observe({
  if(list_data_controls$have_areas & length(list_data_areas$antaresDataList) > 0){
    updateNavbarPage(session, inputId = "nav-id", selected = "prodStack")
  } else if(list_data_controls$have_links & length(list_data_links$antaresDataList) > 0){
    updateNavbarPage(session, inputId = "nav-id", selected = "exchangesStack")
  }
})