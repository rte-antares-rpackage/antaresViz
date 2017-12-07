navbarMenu("plotMap", 
           tabPanel("Layout Builder", 
                    fluidRow(
                      column(12,
                             conditionalPanel(condition = "output.have_data",
                                              antaresViz:::changeCoordsUI("ml")
                             ),
                             conditionalPanel(condition = "output.have_data === false",
                                              h3("No data imported")
                             )
                             
                      )
                    )
           ),
           tabPanel("Current Layout",
                    fluidRow(
                      column(12,
                             conditionalPanel(condition = "output.must_print_map",
                                              div(h3("Current map layout"), align = "center"),
                                              leafletDragPointsOutput("current_layout", height = "700px")
                             ),
                             conditionalPanel(condition = "output.must_print_map === false",
                                              h3("Please set or import a map layout before.")
                             ),
                             hr(),
                             fluidRow(
                               column(6,
                                      
                                      conditionalPanel(condition = "output.must_print_map",
                                                       div(br(), downloadButton('download_layout', 'Download Layout'), align = "center")
                                      )
                                      
                               ),
                               column(6,
                                      div(fileInput("import_layout", "Import a layout", 
                                                    accept = c(".RDS", ".rds", ".Rds")
                                      ), align = "center")
                               )
                             )
                             
                      )
                    )
                    
           ),
           tabPanel("Map", 
                    fluidRow(
                      column(12,
                             conditionalPanel(condition = "output.have_data",
                                              conditionalPanel(condition = "output.must_print_map", 
                                                               uiOutput("plotMap_ui")
                                              ), 
                                              conditionalPanel(condition = "output.must_print_map === false", 
                                                               h3("Please set or import a map layout before.")
                                              )
                             ),
                             conditionalPanel(condition = "output.have_data === false",
                                              h3("No data imported")
                             )
                      )
                    )
           )
)