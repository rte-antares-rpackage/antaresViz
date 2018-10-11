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
          checkboxInput(check_name, antaresViz:::.getLabelLanguage("Include study in analysis", current_language$language), value = TRUE), align = "center")
      })
      # reference selection
      ref_list <- lapply(1:length(list_data), function(i) {
        ref_name <- paste0("list_study_ref", i)
        div(
          checkboxInput(ref_name, antaresViz:::.getLabelLanguage("Choose this study as a reference", current_language$language), value = FALSE), align = "center")
      })
      # bouton pour afficher les parametres
      params_list <- lapply(1:length(list_data), function(i) {
        btn_name <- paste0("list_study_params", i)
        div(
          actionButton(btn_name, antaresViz:::.getLabelLanguage("View parameters", current_language$language)), align = "center")
      })
      # bouton pour supprimer les donnees
      rm_list <- lapply(1:length(list_data), function(i) {
        btn_name <- paste0("list_study_rm", i)
        div(
          actionButton(btn_name, antaresViz:::.getLabelLanguage("Remove study", current_language$language)), align = "center")
      })
      # format et retour
      fluidRow(
        column(3, do.call(tagList, study)),
        column(2, do.call(tagList, params_list)),
        column(2, do.call(tagList, check_list), offset = 0),
        column(3, do.call(tagList, ref_list), offset = 0),
        column(2, do.call(tagList, rm_list), offset = 0)
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
          names(list_data_tmp)[i]
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
            gc(reset = TRUE)
          })
        }
      }
    })
  })
}

# observe locaux pour selectionner une etude de reference

for(j in 1:16){
  local({
    l_j <- j
    #on ne peut avoir qu une etude de reference a la fois 
    observe({
      if(!is.null(input[[paste0("list_study_ref", l_j)]])){
        if(input[[paste0("list_study_ref", l_j)]] > 0){
          for(k in 1:16){
            if(k != l_j){
              #on ne peut avoir qu une etude de reference a la fois 
              updateCheckboxInput(session, paste0("list_study_ref", k), 
                                  label = antaresViz:::.getLabelLanguage("Choose this study as a reference", 
                                                                         current_language$language),
                                  value = FALSE)
            }else{
              #si on prend une etude en reference, on la prend dans l analyse
              updateCheckboxInput(session, paste0("list_study_check", k), 
                                  label = antaresViz:::.getLabelLanguage("Include study in analysis", 
                                                                         current_language$language),
                                  value = TRUE)
              
              #valid import
              #on doit avoir selectionner au moins deux etudes  en analyse
              #on compte le nombre d etudes en analyse
              countCheck <- 0
              for(numRef in 1:16){
                numRef_j <- numRef
                if (!is.null(input[[paste0("list_study_check", numRef_j)]])){
                  if (input[[paste0("list_study_check", numRef_j)]] > 0){
                    countCheck <- countCheck + 1
                  }
                }
              }
              if (countCheck < 2){
                
                messageToPrint <- paste0(antaresViz:::.getLabelLanguage("To use a reference, select at least two studies, number of studies selected :", 
                                                                        current_language$language),
                                         countCheck)
                showModal(modalDialog(
                  easyClose = TRUE,
                  footer = NULL,
                  messageToPrint
                ))
                #we must have more that 2 studies to be able to choose a reference study
                updateCheckboxInput(session, paste0("list_study_ref", k), 
                                    label = antaresViz:::.getLabelLanguage("Choose this study as a reference", current_language$language),
                                    value = FALSE)
              }
            }
          }
        }
      }
    })
    
    #si on l enleve de l etude de l analyse on ne peut pas la choisir comme reference
    observe({
      if (!is.null(input[[paste0("list_study_check", l_j)]])){
        if (input[[paste0("list_study_check", l_j)]] == FALSE  ){
          updateCheckboxInput(session, paste0("list_study_ref", l_j), 
                              label = antaresViz:::.getLabelLanguage("Choose this study as a reference", current_language$language),
                              value = FALSE)
        }
      }
    })
    
  })
}

# get if we have a ref Study 
.get_if_output_has_refStudy <- function(){
  for(j in 1:16){
    l_j <- j
    if(!is.null(input[[paste0("list_study_ref", l_j)]])){
      if(input[[paste0("list_study_ref", l_j)]] > 0){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

# get the name of the ref Study
.get_name_refStudy <- function(){
  list_data <- list_data_all$antaresDataList
  for(j in 1:16){
    l_j <- j
    if(!is.null(input[[paste0("list_study_ref", l_j)]])){
      if(input[[paste0("list_study_ref", l_j)]] > 0){
        refStudyName <- names(list_data)[l_j]
        return(refStudyName)
      }
    }
  }
  return(FALSE)
}

# get the name of the ref Study
.get_id_refStudy <- function(){
  list_data <- list_data_all$antaresDataList
  for(j in 1:16){
    l_j <- j
    if(!is.null(input[[paste0("list_study_ref", l_j)]])){
      if(input[[paste0("list_study_ref", l_j)]] > 0){
        return(l_j)
      }
    }
  }
  return(0)
}