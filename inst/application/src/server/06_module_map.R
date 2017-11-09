# list of opts for set layout
layout <- reactive({
  ind_keep_list_data <- ind_keep_list_data()
  isolate({
    if(!is.null(ind_keep_list_data)){
      ind_map <- unique(sort(c(ind_keep_list_data$ind_areas, ind_keep_list_data$ind_links)))
      if(length(ind_map) > 0){
        if(packageVersion("antaresRead") <= '2.0.0'){
          readLayout(opts = list_data_all$opts[ind_map][[1]])
        } else {
          readLayout(opts = list_data_all$opts[ind_map])
        }
      }else{
        NULL
      }
    } else {
      NULL
    }
  })
})

# module for set and save layout
ml <- callModule(antaresViz:::changeCoordsServer, "ml", layout, 
                 what = reactive("areas"), stopApp = FALSE)

# control : have a not null layout, and so print map module ?
print_map <- reactiveValues(value = FALSE)

observe({
  if(!is.null(ml())){
    print_map$value <- TRUE
  } else {
    print_map$value <- FALSE
  }
})

output$must_print_map <- reactive({
  print_map$value
})

outputOptions(output, "must_print_map", suspendWhenHidden = FALSE)

observe({
  ml <- ml()
  ind_keep_list_data <- ind_keep_list_data()
  isolate({
    if(input$update_module > 0){
      if(!is.null(ind_keep_list_data)){
        ind_map <- unique(sort(c(ind_keep_list_data$ind_areas, ind_keep_list_data$ind_links)))
        if(length(ind_map) > 0){
          if(!is.null(ml)){
            if(length(ind_map) != list_data_controls$n_maps & !is.null(ml)){
              # init / re-init module plotMap
              modules$plotMap <- plotMap(list_data_all$antaresDataList[ind_map], ml, 
                                         colAreaVar = "BALANCE", interactive = TRUE, 
                                         xyCompare = "union", .updateBtn = TRUE, 
                                         .updateBtnInit = TRUE, .runApp = FALSE)
            }
            # save data and params
            list_data_map$antaresDataList <- list_data_all$antaresDataList[ind_map]
            list_data_map$h5requestFiltering <- list_data_all$params[ind_map]
            list_data_controls$n_maps <- length(ind_map)
          }
        }
      }
    }
  })
})

# update map module
observe({
  if(!is.null(modules$plotMap)){
    mwModule(id = "plotMap",  modules$plotMap, x = reactive(list_data_map$antaresDataList), 
             mapLayout = ml, h5requestFiltering = reactive(list_data_map$h5requestFiltering))
  }
})

# change page
observe({
  if(!is.null(input[['ml-done']])){
    if(input[['ml-done']] > 0){
      updateNavbarPage(session, inputId = "nav-id", selected = "Map")
    }
  }
})