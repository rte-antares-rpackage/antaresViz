input_data <- reactiveValues(data = .global_build_input_data(.global_shared_input), cpt = 0)

observe({
  input_data$cpt
  current_input_data <- input_data$data
  for (ii in 1:nrow(current_input_data)){ 
    local({
      i <- ii
      observe({ 
        current_value <- input[[current_input_data[i, input_id]]]
        if(!is.null(current_value) & input$is_shared_input){
          isolate({
            current_input_data[i, last_update := as.character(Sys.time())]
            if(isolate(current_input_data$update_call[i]) != ""){
              eval(parse(text = isolate(current_input_data$update_call[i])))
              isolate(current_input_data[i, update_call := ""])
            }
          })
        }
      }) 
    })
  }
})

observe({
  current_nav <- input[['nav-id']]
  current_input_data <- isolate(input_data$data)
  data_shared_input <- current_input_data[panel %in% current_nav]
  if(nrow(data_shared_input) > 0 & input$is_shared_input){
    for(ii in 1:nrow(data_shared_input)){
      local({
        i <- ii
        last_update_input <- current_input_data[!panel %in% current_nav & 
                                                  input%in%data_shared_input[i, input] & 
                                                  !is.na(last_update)][order(last_update, decreasing = TRUE)]
        if(nrow(last_update_input) >= 1){
          if(data_shared_input[i, type] %in% "dateRangeInput"){
            if(!is.null(isolate({input[[data_shared_input[i, input_id]]]}))){
              updateDateRangeInput(session, data_shared_input[i, input_id], 
                                   start = isolate({input[[last_update_input[1, input_id]]][1]}), 
                                   end = isolate({input[[last_update_input[1, input_id]]][2]}))
            } else {
              expr <- paste0("updateDateRangeInput(session, '", data_shared_input[i, input_id], 
                             "', start = '", isolate({input[[last_update_input[1, input_id]]][1]}), 
                             "', end = '", isolate({input[[last_update_input[1, input_id]]][2]}), "')")
              isolate({
                input_data$data[input_id %in% data_shared_input[i, input_id], update_call := expr]
              })
              
            }
            
          } else if(data_shared_input[i, type] %in% "selectInput"){
            if(!is.null(isolate({input[[data_shared_input[i, input_id]]]}))){
              updateSelectInput(session, data_shared_input[i, input_id], 
                                selected = isolate({input[[last_update_input[1, input_id]]]}))
            } else {
              expr <- paste0("updateSelectInput(session, '", data_shared_input[i, input_id],
                             "', selected = '", isolate({input[[last_update_input[1, input_id]]]}), "')")
              isolate({
                input_data$data[input_id %in% data_shared_input[i, input_id], update_call := expr]
              })
            }
          }else if(data_shared_input[i, type] %in% "checkboxInput"){
            if(!is.null(isolate({input[[data_shared_input[i, input_id]]]}))){
              updateCheckboxInput(session, data_shared_input[i, input_id], 
                                  value = isolate({input[[last_update_input[1, input_id]]]}))
            } else {
              expr <- paste0("updateCheckboxInput(session, '", data_shared_input[i, input_id],
                             "', value = ", isolate({input[[last_update_input[1, input_id]]]}), ")")
              input_data$data[input_id %in% data_shared_input[i, input_id], update_call := expr]
            }
          }
        }
      })
    }
  }
})