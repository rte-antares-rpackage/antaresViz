tabPanel("Read data", 
         h3("readAntares parameters"),
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
                                   h4("removeVirtualAreas :")
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
         div(actionButton("import_data", "Validate & import data", icon = icon("upload")), align = "center")
)