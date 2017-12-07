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
          id_prodStack <- paste0("prodStack_", round(runif(1, 1, 100000000)))
          
          # update shared input table
          input_data$data[grepl("^prodStack", input_id), input_id := paste0(id_prodStack, "-shared_", input)]
          
          output[["prodStack_ui"]] <- renderUI({
            mwModuleUI(id = id_prodStack, height = "800px", fluidRow = TRUE)
          })
          
          .compare <- input$sel_compare_prodstack
          if(input$sel_compare_mcyear){
            .compare <- unique(c(.compare, "mcYear"))
          }
          if(!is.null(.compare)){
            list_compare <- vector("list", length(.compare))
            names(list_compare) <- .compare
            # set main with study names
            if(length(ind_areas) != 1){
              list_compare$main <- names(list_data_all$antaresDataList[ind_areas])
            }
            .compare <- list_compare
          } else {
            .compare = NULL
          }
          mod_prodStack <- prodStack(list_data_all$antaresDataList[ind_areas], xyCompare = "union",
                                         h5requestFiltering = list_data_all$params[ind_areas],
                                         unit = "GWh", interactive = TRUE, .updateBtn = TRUE, 
                                         .updateBtnInit = TRUE, compare = .compare, .runApp = FALSE)
          
          if("MWController" %in% class(modules$prodStack)){
            modules$prodStack$clear()
          }
          
          modules$prodStack <- mwModule(id = id_prodStack,  mod_prodStack)
          
          # init / re-init module plotts
          id_ts <- paste0("plotts_", round(runif(1, 1, 100000000)))
          
          # update shared input table
          input_data$data[grepl("^plotts", input_id), input_id := paste0(id_ts, "-shared_", input)]

          output[["plotts_ui"]] <- renderUI({
            mwModuleUI(id = id_ts, height = "800px", fluidRow = TRUE)
          })
          
          .compare <- input$sel_compare_tsPlot
          if(input$sel_compare_mcyear){
            .compare <- unique(c(.compare, "mcYear"))
          }
          if(!is.null(.compare)){
            list_compare <- vector("list", length(.compare))
            names(list_compare) <- .compare
            # set main with study names
            if(length(ind_areas) != 1){
              list_compare$main <- names(list_data_all$antaresDataList[ind_areas])
            }
            .compare <- list_compare
          } else {
            .compare = NULL
          }
          mod_plotts <- plot(list_data_all$antaresDataList[ind_areas], xyCompare = "union",
                                 h5requestFiltering = list_data_all$params[ind_areas],
                                 interactive = TRUE, .updateBtn = TRUE, 
                                 .updateBtnInit = TRUE, compare = .compare, .runApp = FALSE)
          
          if("MWController" %in% class(modules$plotts)){
            modules$plotts$clear()
          }
          
          modules$plotts <- mwModule(id = id_ts,  mod_plotts)
          
          list_data_controls$n_areas <- length(ind_areas)
          list_data_controls$have_areas <- TRUE
        } else {
          list_data_controls$have_areas <- FALSE
        }
        
        # exchange
        ind_links <- ind_keep_list_data$ind_links
        if(length(ind_links) > 0){
          # init / re-init module exchangesStack
          id_exchangesStack  <- paste0("exchangesStack_", round(runif(1, 1, 100000000)))
          
          # update shared input table
          input_data$data[grepl("^exchangesStack", input_id), input_id := paste0(id_exchangesStack, "-shared_", input)]
          
          output[["exchangesStack_ui"]] <- renderUI({
            mwModuleUI(id = id_exchangesStack, height = "800px", fluidRow = TRUE)
          })
          
          .compare <- input$sel_compare_exchangesStack
          if(input$sel_compare_mcyear){
            .compare <- unique(c(.compare, "mcYear"))
          }
          if(!is.null(.compare)){
            list_compare <- vector("list", length(.compare))
            names(list_compare) <- .compare
            # set main with study names
            if(length(ind_links) != 1){
              list_compare$main <- names(list_data_all$antaresDataList[ind_links])
            }
            .compare <- list_compare
          } else {
            .compare = NULL
          }
          mod_exchangesStack <- exchangesStack(list_data_all$antaresDataList[ind_links], xyCompare = "union",
                                                   h5requestFiltering = list_data_all$params[ind_links],
                                                   interactive = TRUE, .updateBtn = TRUE, 
                                                   .updateBtnInit = TRUE, compare = .compare, .runApp = FALSE)
          
          if("MWController" %in% class(modules$exchangesStack)){
            modules$exchangesStack$clear()
          }
          
          modules$exchangesStack <- mwModule(id = id_exchangesStack,  mod_exchangesStack)
          
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
    
    input_data$cpt <- isolate(input_data$cpt) +1
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