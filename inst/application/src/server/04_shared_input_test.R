input_data <- reactiveValues(data = build_input_data(shared_input), cpt = 0)


observe({
  input_data$cpt
  current_input_data <- input_data$data
  lapply(1:nrow(current_input_data), function(i){
    observe({
      # need because input names change...
      # tmp <- names(input)
      print("observe")
      print(current_input_data[i, ])
      current_value <- input[[current_input_data[i, input_id]]]
      print(current_value)
      current_nav <- input[['nav-id']]
      if(!is.null(current_value)){
        isolate({
          input_data$data[i, last_update := as.character(Sys.time())]
          
          print("current_nav")
          print(current_nav)
          if(current_nav == input_data$data$panel[i] & input_data$data$cpt_update[i] < 2){
            print("ici")
            last_update_input <- current_input_data[!panel %in% current_nav & 
                                                      input%in%input_data$data[i, input] & 
                                                      !is.na(last_update)][order(last_update, decreasing = TRUE)]
            if(nrow(last_update_input) >= 1){
              print("last update")
              print(last_update_input[1, ])
              if(input_data$data[i, type] %in% "dateRangeInput"){
                updateDateRangeInput(session, input_data$data[i, input_id], 
                                     start = isolate({input[[last_update_input[1, input_id]]][1]}), 
                                     end = isolate({input[[last_update_input[1, input_id]]][2]}))
              } else if(input_data$data[i, type] %in% "selectInput"){
                updateSelectInput(session, input_data$data[i, input_id], 
                                  selected = isolate({input[[last_update_input[1, input_id]]]}))
              }
              
              input_data$data[i, cpt_update := cpt_update + 1]
            }
          }
        })
      }
    })
  })
})

# observe({
#   
# 
#   current_nav <- input[['nav-id']]
#   print("current_nav")
#   print(current_nav)
#   current_input_data <- isolate(input_data$data)
#   data_shared_input <- current_input_data[panel %in% current_nav]
#   print("data_shared_input")
#   print(data_shared_input)
#   if(nrow(data_shared_input) > 0){
#     for(i in 1:nrow(data_shared_input)){
#       last_update_input <- current_input_data[!panel %in% current_nav & 
#                                                 input%in%data_shared_input[i, input] & 
#                                                 !is.na(last_update)][order(last_update, decreasing = TRUE)]
#       if(nrow(last_update_input) >= 1){
#         print("update")
#         print(last_update_input[1, ])
#         if(data_shared_input[i, type] %in% "dateRangeInput"){
#           print(data_shared_input[i, input_id])
#           print(isolate({input[[last_update_input[1, input_id]]][1]}))
#           updateDateRangeInput(session, data_shared_input[i, input_id], 
#                                start = isolate({input[[last_update_input[1, input_id]]][1]}), 
#                                end = isolate({input[[last_update_input[1, input_id]]][2]}))
#           input_data$data[input_id %in% data_shared_input[i, input_id], to_update := FALSE]
#         } else if(data_shared_input[i, type] %in% "selectInput"){
#           updateSelectInput(session, data_shared_input[i, input_id], 
#                             selected = isolate({input[[last_update_input[1, input_id]]]}))
#         }
#       }
#     }
#   }
# }, priority = -1)