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
          h4(textOutput(study_name)), style = 'height:24px', align = "center")
      })
      # checkbox de selection
      check_list <- lapply(1:length(list_data), function(i) {
        check_name <- paste0("list_study_check", i)
        div(
          checkboxInput(check_name, "Include study in analysis", value = TRUE), align = "center")
      })
      # bouton pour afficher les parametres
      params_list <- lapply(1:length(list_data), function(i) {
        btn_name <- paste0("list_study_params", i)
        div(
          actionButton(btn_name, "View parameters"), align = "center")
      })
      # bouton pour supprimer les donnees
      rm_list <- lapply(1:length(list_data), function(i) {
        btn_name <- paste0("list_study_rm", i)
        div(
          actionButton(btn_name, "Remove study"), align = "center")
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
    fluidRow()
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
            # print("remove")
            # print(l_j)
            # print(object_size(list_data_all$antaresDataList))
            # print(object_size(list_data_all$antaresDataList[l_j]))
            # print(mem_change(list_data_all$antaresDataList[l_j] <- NULL))
            # print(object_size(list_data_all$antaresDataList))
            # print(object_size(list_data_all$antaresDataList[l_j]))
            list_data_all$antaresDataList[l_j] <- NULL
            list_data_all$params[l_j] <- NULL
            gc(reset = TRUE)
            
          })
        }
      }
    })
  })
}