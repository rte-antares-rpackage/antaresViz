input_data <- reactiveValues(data = build_input_data(shared_input))


lapply(1:nrow(shared_input), function(i){
  observe({
    current_input_data <- isolate(input_data$data)
    current_value <- input[[current_input_data[i, input_id]]]
    if(!is.null(current_value)){
      isolate({
        input_data$data[i, last_update := as.character(Sys.time())]
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
                            selected = isolate({input[[last_update_input[1, input_id]]]}))
        }
      }
    }
  }
})