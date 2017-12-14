tabPanel("Convert to h5",
         conditionalPanel(condition = "output.have_study && output.current_opts_h5 === false", 
                          fluidRow(
                            column(12,
                                   h3("writeAntaresH5 parameters"),
                                   fluidRow(
                                     column(6,
                                            directoryInput('output_h5', label = 'Select where study will be write', 
                                                           value = getwd())),
                                     
                                     column(3,
                                            selectInput("timeSteps_h5", label = "timeStep :",
                                                        choices = c("hourly", "daily", "weekly","monthly", "annual"),
                                                        multiple = TRUE, selected = "hourly"))
                                   ),
                                   
                                   
                                   fluidRow(
                                     column(3,
                                            checkboxInput("overwrite_h5", label = "overwrite" , TRUE)),
                                     column(3,
                                            checkboxInput("writeMcAll_h5", label = "writeMcAll" , TRUE)
                                     )
                                     
                                   ),
                                   
                                   fluidRow(
                                     
                                     column(3,
                                            checkboxInput("misc_h5", label = "misc")
                                     ),
                                     column(3,
                                            checkboxInput("thermalAvailabilities_h5", label = "thermalAvailabilities")
                                     ),
                                     column(3,
                                            checkboxInput("mustRun_h5", label = "mustRun")
                                     ),
                                     column(3,
                                            checkboxInput("thermalModulation_h5", label = "thermalModulation")
                                     )
                                   )
                                   ,
                                   fluidRow(
                                     column(3,
                                            checkboxInput("hydroStorage_h5", label = "hydroStorage")
                                     ),
                                     column(3,
                                            checkboxInput("hydroStorageMaxPower_h5", label = "hydroStorageMaxPower")
                                     ),
                                     column(3,
                                            checkboxInput("reserve_h5", label = "reserve")
                                     ),
                                     column(3,
                                            checkboxInput("linkCapacity_h5", label = "linkCapacity")
                                     )
                                   ),
                                   
                                   
                                   fluidRow(
                                     column(3, 
                                            h4("removeVirtualAreas :")
                                     ),
                                     column(9, 
                                           
                                                   checkboxInput("rmva_ctrl_h5", "enabled", FALSE)
                                            
                                            ),
                                     conditionalPanel("input.rmva_ctrl_h5", 
                                     fluidRow(
                                       column(3, 
                                              selectInput("rmva_storageFlexibility_h5", "storageFlexibility :", choices = NULL, selected = NULL, multiple = TRUE)
                                       ),
                                       column(3, 
                                              selectInput("rmva_production_h5", "production :", choices = NULL, selected = NULL, multiple = TRUE)
                                       ), 
                                       
                                       column(3, 
                                              br(),
                                              checkboxInput("rmva_reassignCosts_h5", "reassignCosts", FALSE)
                                       ),
                                       
                                       column(3, 
                                              br(),
                                              checkboxInput("rmva_newCols_h5", "newCols", FALSE))
                                       
                                     )),
                                       
                                       fluidRow(
                                         column(12,
                                                div(actionButton("write_h5", "Convert study to h5", icon = icon("floppy-o")), align = "center")                                                           )
                                       )
                                     )
                            )
                            ),
                            conditionalPanel(condition = "output.have_study && output.current_opts_h5 === true", 
                                             h3("Already a .h5 study...!")
                            )
                          )
         
)