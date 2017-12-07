function(input, output, session) {

  #----------------
  # Write h5
  #----------------
  source("src/server/07_write_h5.R", local = T)
  
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
  
  # launch when click on ""Launch Analysis" button
  # and get back which opts / data to keep
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
          
          # all to keep
          ind_all <- which(final_keep)
          
          # with areas
          ind_areas <- intersect(which(list_data_all$have_areas), ind_all)
          
          # with links
          ind_links <- intersect(which(list_data_all$have_links), ind_all)
          
          list(ind_all = ind_all, ind_areas = ind_areas, ind_links = ind_links)
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
  
  source("src/server/05_modules.R", local = T)
  
  #------------
  # plotMap
  #------------
  
  source("src/server/06_module_map.R", local = T)
  
  #----------------
  # shared inputs
  #----------------
  
  source("src/server/04_shared_input.R", local = T)
  
  #----------------
  # memory options
  #----------------
  observe({
    if(!is.na(input$ram_limit)){
      antaresRead::setRam(input$ram_limit)
    }
  })
  
  observe({
    if(!is.na(input$data_module)){
      limitSizeGraph(input$data_module)
    }
  })
  
  #----------------
  # quit
  #----------------
  
  # in case of classic use : 
  observe({
    if(input$quit > 0){
      stopApp(returnValue = TRUE)
    }
  })
  
  # in case of Rinno / packaging app for windows
  # (and so comment previous observe....!)
  #
  # observe({
  #   if(input$quit > 0){
  #     stopApp()
  #     q("no")
  #   }
  # })
  # 
  # session$onSessionEnded(function() {
  #   stopApp()
  #   q("no")
  # })
}
