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
          # init / re-init module prodStack
          modules$prodStack <- prodStack(list_data_all$antaresDataList[ind_areas], xyCompare = "union",
                                         h5requestFiltering = list_data_all$params[ind_areas],
                                         unit = "GWh", interactive = TRUE, .updateBtn = TRUE, 
                                         .updateBtnInit = TRUE, .runApp = FALSE)
          mwModule(id = "prodStack",  modules$prodStack)
          
          # init / re-init module plotts
          modules$plotts <- plot(list_data_all$antaresDataList[ind_areas], xyCompare = "union",
                                 h5requestFiltering = list_data_all$params[ind_areas],
                                 interactive = TRUE, .updateBtn = TRUE, 
                                 .updateBtnInit = TRUE, .runApp = FALSE)
          
          mwModule(id = "plotts",  modules$plotts)
          
          list_data_controls$n_areas <- length(ind_areas)
          list_data_controls$have_areas <- TRUE
        } else {
          list_data_controls$have_areas <- FALSE
        }
        
        # exchange
        ind_links <- ind_keep_list_data$ind_links
        if(length(ind_links) > 0){
          # init / re-init module exchangesStack
          modules$exchangesStack <- exchangesStack(list_data_all$antaresDataList[ind_links], xyCompare = "union",
                                                   h5requestFiltering = list_data_all$params[ind_links],
                                                   interactive = TRUE, .updateBtn = TRUE, 
                                                   .updateBtnInit = TRUE, .runApp = FALSE)
          
          mwModule(id = "exchangesStack",  modules$exchangesStack)
          
          # save data and params
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
  if(input$update_module > 0){
    if(list_data_controls$have_areas & list_data_controls$n_areas >= 1){
      updateNavbarPage(session, inputId = "nav-id", selected = "prodStack")
    } else if(list_data_controls$have_links & list_data_controls$n_links >= 1){
      updateNavbarPage(session, inputId = "nav-id", selected = "exchangesStack")
    }
  }
})