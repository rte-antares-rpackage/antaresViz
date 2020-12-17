current_language <- reactiveValues(language = "fr")

observe({
  language <- input$language
  if(!is.null(language)){
    if(language != isolate(current_language$language)){
      current_language$language <- language
    }
  }
})


output$label_tab_data <- renderText({
  antaresViz:::.getLabelLanguage("Data", current_language$language)
})

output$label_tab_exchanges <- renderText({
  antaresViz:::.getLabelLanguage("Exchanges", current_language$language)
})

output$label_tab_tsPlot <- renderText({
  antaresViz:::.getLabelLanguage("Time Series", current_language$language)
})

output$label_tab_map_viz <- renderText({
  antaresViz:::.getLabelLanguage("Map", current_language$language)
})

output$label_tab_layout_view<- renderText({
  antaresViz:::.getLabelLanguage("Current Layout", current_language$language)
})

output$label_tab_layout_build <- renderText({
  antaresViz:::.getLabelLanguage("Layout Builder", current_language$language)
})

output$label_tab_map_menu <- renderText({
  antaresViz:::.getLabelLanguage("plotMap", current_language$language)
})

output$label_tab_parameters <- renderText({
  antaresViz:::.getLabelLanguage("Parameters", current_language$language)
})

output$label_tab_help <- renderText({
  antaresViz:::.getLabelLanguage("Help", current_language$language)
})

output$label_tab_import_data <- renderText({
  antaresViz:::.getLabelLanguage("Import Data", current_language$language)
})

output$label_tab_analysis <- renderText({
  antaresViz:::.getLabelLanguage("Analysis", current_language$language)
})

output$label_tab_help <- renderText({
  antaresViz:::.getLabelLanguage("Help", current_language$language)
})

output$title_import_data <- renderText({
  paste0(antaresViz:::.getLabelLanguage("Antares study selection", current_language$language), " : ")
})

observe({
  # button
  updateActionButton(session, "init_sim", label = antaresViz:::.getLabelLanguage("Set simulation", current_language$language))
  updateActionButton(session, "import_data", label = antaresViz:::.getLabelLanguage("Validate & import data", current_language$language))
  updateActionButton(session, "quit", label = antaresViz:::.getLabelLanguage("Quit application", current_language$language))
  updateActionButton(session, "update_module", label = antaresViz:::.getLabelLanguage("Launch Analysis", current_language$language))
  
  updateCheckboxInput(session, "sel_compare_mcyear", 
                      label = antaresViz:::.getLabelLanguage("mcYear on all modules ?", current_language$language))
  
  
  # compare
  updateSelectInput(session,"sel_compare_prodstack", label =   paste0(antaresViz:::.getLabelLanguage("Production", current_language$language), " : "))
  updateSelectInput(session,"sel_compare_exchangesStack", label =   paste0(antaresViz:::.getLabelLanguage("Exchanges", current_language$language), " : "))
  updateSelectInput(session,"sel_compare_tsPlot", label =   paste0(antaresViz:::.getLabelLanguage("Time Series", current_language$language), " : "))
  updateSelectInput(session,"sel_compare_plotMap", label =   paste0(antaresViz:::.getLabelLanguage("Map", current_language$language), " : "))

  # data selection
  length_data <- isolate({list_data_all$antaresDataList})
  for(i in 1:length(length_data)){
    updateActionButton(session, paste0("list_study_params", i), 
                       label = antaresViz:::.getLabelLanguage("View parameters", current_language$language))
    updateActionButton(session, paste0("list_study_rm", i),
                       label = antaresViz:::.getLabelLanguage("Remove study", current_language$language))
    updateCheckboxInput(session, paste0("list_study_check", i), 
                        label = antaresViz:::.getLabelLanguage("Include study in analysis", current_language$language))
    updateCheckboxInput(session, paste0("list_study_ref", i), 
                        label = antaresViz:::.getLabelLanguage("Choose this study as a reference", current_language$language))
    
  }

  cur_timeStep <- isolate({input$read_timeStep})
  choices_ts <- c("hourly", "daily", "weekly", "monthly", "annual")
  names(choices_ts) <- sapply(choices_ts, function(x){
    antaresViz:::.getLabelLanguage(x, current_language$language)
  })
  updateSelectInput(session,"read_timeStep",
                    label = paste0(antaresViz:::.getLabelLanguage("timeStep", current_language$language), " : "),
                    choices = choices_ts, selected = cur_timeStep)
  
  cur_timeStep <- isolate({input$timeSteps_h5})
  updateSelectInput(session,"timeSteps_h5",
                    label = paste0(antaresViz:::.getLabelLanguage("timeStep", current_language$language), " : "),
                    choices = choices_ts, selected = cur_timeStep)
  
  # Remove virtual Areas
  updateCheckboxInput(session, "rmva_ctrl", antaresViz:::.getLabelLanguage("enabled", current_language$language))
  
  # H5
  updateCheckboxInput(session, "enabled_write_h5", antaresViz:::.getLabelLanguage("enabled", current_language$language))
  updateCheckboxInput(session, "rmva_ctrl_h5", antaresViz:::.getLabelLanguage("enabled", current_language$language))
  updateActionButton(session,"write_h5", label = antaresViz:::.getLabelLanguage("Convert study to h5", current_language$language))
  
  # params
  updateCheckboxInput(session, "is_shared_input", antaresViz:::.getLabelLanguage("Share inputs between modules ?", current_language$language))
  
})

output$current_opts <- renderText({
  paste0(antaresViz:::.getLabelLanguage("ANTARES Simulation", current_language$language), " : ", 
         current_study_path())
  
})

output$title_readAntares <- renderText({
  antaresViz:::.getLabelLanguage("readAntares parameters", current_language$language)
})

output$title_analysis <- renderText({
  antaresViz:::.getLabelLanguage("Analysis parameters", current_language$language)
})

output$title_studies <- renderText({
  paste0(antaresViz:::.getLabelLanguage("Studies", current_language$language), " : ")
  
})

output$title_compare <- renderText({
  paste0(antaresViz:::.getLabelLanguage("Compare", current_language$language), " : ")
})

output$title_removeVirtualAreas <- renderText({
  paste0(antaresViz:::.getLabelLanguage("Remove virtual Areas", current_language$language), " : ")
})


output$title_hvdc <- renderText({
  paste0(antaresViz:::.getLabelLanguage("Hvdc areas", current_language$language), " : ")
})


output$title_removeVirtualAreas_h5 <- renderText({
  paste0(antaresViz:::.getLabelLanguage("Remove virtual Areas", current_language$language), " : ")
})

output$title_writeH5 <- renderText({
  antaresViz:::.getLabelLanguage("writeAntaresH5 parameters", current_language$language)
})

output$directory_h5 <- renderText({
  paste0(antaresViz:::.getLabelLanguage("Target directory", current_language$language), " : ")
})

output$title_h5_timeStep <- renderText({
  paste0(antaresViz:::.getLabelLanguage("timeStep", current_language$language), " : ")
})

output$title_enabled_H5 <- renderText({
  antaresViz:::.getLabelLanguage("Write study in h5 ?", current_language$language)
})

output$title_readAntaresRAM <- renderText({
  paste0(antaresViz:::.getLabelLanguage("readAntares RAM limit (in Go)", current_language$language), " : ")
})

output$title_antaresVizRAM <- renderText({
  paste0(antaresViz:::.getLabelLanguage("antaresViz data module (in Mb)", current_language$language), " : ")
})


output$no_data_1 <- renderText({
  antaresViz:::.getLabelLanguage("No data imported", current_language$language)
})

output$no_data_2 <- renderText({
  antaresViz:::.getLabelLanguage("No data imported", current_language$language)
})

output$no_data_3 <- renderText({
  antaresViz:::.getLabelLanguage("No data imported", current_language$language)
})

output$no_data_4 <- renderText({
  antaresViz:::.getLabelLanguage("No data imported", current_language$language)
})

output$no_data_5 <- renderText({
  antaresViz:::.getLabelLanguage("No data imported", current_language$language)
})

output$no_data_6 <- renderText({
  antaresViz:::.getLabelLanguage("No data imported", current_language$language)
})

output$no_data_7 <- renderText({
  antaresViz:::.getLabelLanguage("No data imported", current_language$language)
})

output$no_layout_1 <- renderText({
  antaresViz:::.getLabelLanguage("Please set or import a map layout before", current_language$language)
})

output$no_layout_2 <- renderText({
  antaresViz:::.getLabelLanguage("Please set or import a map layout before", current_language$language)
})


output$title_download_layout <- renderText({
  paste0(antaresViz:::.getLabelLanguage("Download Layout", current_language$language), " : ")
})

output$title_import_layout <- renderText({
  paste0(antaresViz:::.getLabelLanguage("Import a layout", current_language$language), " : ")
})

output$title_current_layout <- renderText({
  antaresViz:::.getLabelLanguage("Current map layout", current_language$language)
})


output$no_areas_1 <- renderText({
  antaresViz:::.getLabelLanguage("No areas imported", current_language$language)
})

output$no_areas_2 <- renderText({
  antaresViz:::.getLabelLanguage("No areas imported", current_language$language)
})

output$no_links <- renderText({
  antaresViz:::.getLabelLanguage("No links imported", current_language$language)
})