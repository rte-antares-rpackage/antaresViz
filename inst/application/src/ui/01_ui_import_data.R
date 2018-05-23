tabPanel(textOutput("label_tab_import_data"),
         h3(textOutput("title_import_data")),
         fluidRow(
           column(5, 
                  directoryInput('directory', label = '', value = 'C:\\Users\\Datastorm\\Documents\\git\\bpNumerique2018\\inst\\application_bp\\data')
           ), 
           conditionalPanel(condition = "output.ctrl_is_antares_study | output.ctrl_is_antares_h5", 
                            column(1, 
                                   div(br(), h4("Simulation : "), align = "center")
                            ), 
                            column(4, 
                                   selectInput("study_path", "", choices = NULL, selected = NULL, width = "100%")
                            ), 
                            column(2, 
                                   div(br(), 
                                       actionButton("init_sim", "Set simulation", icon = icon("check-circle")),
                                       align = "center"
                                   )
                            )
           ),
           conditionalPanel(condition = "output.ctrl_is_antares_study === false & output.ctrl_is_antares_h5 === false", 
                            column(5, 
                                   h3(textOutput("directory_message"), style = "color : red")
                            )
           )
         ), 
         
         conditionalPanel(condition = "output.have_study",
                          fluidRow(
                            column(12,
                                   hr(), 
                                   div(h3(textOutput("current_opts"), align = "center")),
                                   
                                   h3(textOutput("title_readAntares")),
                                   fluidRow(
                                     column(3, 
                                            selectInput("read_areas", "Areas :", choices = NULL, selected = NULL, multiple = TRUE)
                                     ), 
                                     column(3, 
                                            selectInput("read_links", "Links :", choices = NULL, selected = NULL, multiple = TRUE)
                                     ), 
                                     column(3, 
                                            selectInput("read_clusters", "Clusters : ", choices = NULL, selected = NULL, multiple = TRUE)
                                     ), 
                                     column(3, 
                                            selectInput("read_districts", "Districts :", choices = NULL, selected = NULL, multiple = TRUE)
                                     )
                                   ), 
                                   conditionalPanel(condition = "output.current_opts_h5 === false",
                                                    fluidRow(
                                                      column(3, 
                                                             checkboxInput("read_misc", "misc", FALSE),
                                                             checkboxInput("read_reserve", "reserve", FALSE)
                                                      ),
                                                      column(3, 
                                                             checkboxInput("read_thermalAvailabilities", "thermalAvailabilities", FALSE),
                                                             checkboxInput("read_linkCapacity", "linkCapacity", FALSE)
                                                      ),
                                                      column(3, 
                                                             checkboxInput("read_hydroStorage", "hydroStorage", FALSE),
                                                             checkboxInput("read_mustRun", "mustRun", FALSE)
                                                      ),
                                                      column(3, 
                                                             checkboxInput("read_hydroStorageMaxPower", "hydroStorageMaxPower", FALSE),
                                                             checkboxInput("read_thermalModulation", "thermalModulation", FALSE)
                                                      )
                                                    ),
                                                    fluidRow(
                                                      column(3, 
                                                             selectInput("read_timeStep", "timeStep :", choices = c("hourly", "daily", "weekly",
                                                                                                                    "monthly", "annual"))
                                                      ),
                                                      column(3, 
                                                             radioButtons("read_type_mcYears", "mcYears :",
                                                                          c("synthetic", "all", "custom"), inline = TRUE)
                                                      ), 
                                                      conditionalPanel(condition = "input.read_type_mcYears === 'custom'", 
                                                                       column(3, 
                                                                              selectInput("read_mcYears", "Choose mcYears :", choices = NULL, selected = NULL, multiple = TRUE)
                                                                       )
                                                      )
                                                      # ,column(3, 
                                                      #        checkboxInput("read_parallel", "parallel", FALSE)
                                                      # )
                                                    )
                                   ), 
                                   fluidRow(
                                     column(12, 
                                            selectInput("read_select", "Select :", choices = NULL, selected = NULL, 
                                                        width = "100%", multiple = TRUE)
                                     )
                                   ),
                                   conditionalPanel(condition = "output.current_opts_h5 === false",
                                                    fluidRow(
                                                      column(3, 
                                                             h4(textOutput("title_removeVirtualAreas"))
                                                      ),
                                                      column(3, 
                                                             checkboxInput("rmva_ctrl", "enabled", FALSE)
                                                      )
                                                    ),
                                                    conditionalPanel(condition = "input.rmva_ctrl",
                                                                     fluidRow(
                                                                       column(3, 
                                                                              selectInput("rmva_storageFlexibility", "storageFlexibility :", choices = NULL, selected = NULL, multiple = TRUE)
                                                                       ),
                                                                       column(3, 
                                                                              selectInput("rmva_production", "production :", choices = NULL, selected = NULL, multiple = TRUE)
                                                                       ), 
                                                                       
                                                                       column(3, 
                                                                              br(),
                                                                              checkboxInput("rmva_reassignCosts", "reassignCosts", FALSE)
                                                                       ),
                                                                       
                                                                       column(3, 
                                                                              br(),
                                                                              checkboxInput("rmva_newCols", "newCols", FALSE)
                                                                       )
                                                                     )
                                                    )
                                   ),
                                   div(actionButton("import_data", "Validate & import data", icon = icon("upload")), align = "center"),
                                   
                                   # convert h5
                                   conditionalPanel(condition = "output.have_study && output.current_opts_h5 === false", 
                                                    hr(),
                                                    fluidRow(
                                                      column(3, 
                                                             h4(textOutput("title_enabled_H5"))
                                                      ),
                                                      column(3, 
                                                             checkboxInput("enabled_write_h5", "enabled", FALSE)
                                                      )
                                                    ),
                                                    conditionalPanel(condition = "input.enabled_write_h5", 
                                                                     fluidRow(
                                                                       column(12,
                                                                              h3(textOutput("title_writeH5")),
                                                                              fluidRow(
                                                                                column(3, h4(textOutput("directory_h5"))),
                                                                                column(6,
                                                                                       directoryInput('output_h5', label = NULL, 
                                                                                                      value = getwd())
                                                                                ),
                                                                                column(1, h4(textOutput("title_h5_timeStep"))),
                                                                                column(2,
                                                                                       selectInput("timeSteps_h5", label = NULL,
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
                                                                                       h4(textOutput("title_removeVirtualAreas_h5"))
                                                                                ),
                                                                                column(9, 
                                                                                       
                                                                                       checkboxInput("rmva_ctrl_h5", "enabled", FALSE)
                                                                                       
                                                                                )
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
                                                                                                 
                                                                                               )
                                                                              ),
                                                                              
                                                                              fluidRow(
                                                                                column(12,
                                                                                       div(actionButton("write_h5", "Convert study to h5", icon = icon("floppy-o")), align = "center")                                                           )
                                                                              )
                                                                       )
                                                                       
                                                                     )
                                                    )
                                   )
                            )
                            
                          )
         )
)