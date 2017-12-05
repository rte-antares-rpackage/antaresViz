# Define UI for antaresViz app
navbarPage(title = "antaresViz", id = "nav-id", inverse= TRUE, collapsible = TRUE, position = "fixed-top",
header = fluidRow(
  column(12, br(), br(), br(), singleton(tags$script(src = 'events.js')), 
         div(id = "import_busy", tags$img(src= "spinner.gif", height = 100, 
        style = "position: fixed;top: 50%;z-index:10;left: 48%;")), align = "center")
), windowTitle = "antaresViz",
tabPanel("Data", 
         h3("Antares study selection"),
         fluidRow(
           column(7, 
                  directoryInput('directory', label = 'Select an antares study', 
                                 value = 'C:\\Users\\Datastorm\\Desktop\\antares\\test_case')
           ), 
           conditionalPanel(condition = "output.ctrl_is_antares_study | output.ctrl_is_antares_h5", 
                            column(3, 
                                   selectInput("study_path", "Select a simulation", choices = NULL, selected = NULL)
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
                          hr(), 
                          div(fluidRow(
                            column(6, 
                                   h3("ANTARES Simulation :", align = "right")
                            ),
                            column(6, 
                                   h3(textOutput("current_opts"), align = "left")
                            )
                          )),
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
         ),
         conditionalPanel(condition = "output.have_data === true",
                          hr(),
                          uiOutput("info_list"),
                          div(actionButton("update_module", "Launch Analysis", icon = icon("upload")), align = "center")
         )
),
tabPanel("prodStack",
         fluidRow(column(12,
                         conditionalPanel(condition = "output.have_data",
                                          conditionalPanel(condition = "output.have_data_areas",
                                                           uiOutput("prodStack_ui")
                                          ),
                                          conditionalPanel(condition = "output.have_data_areas === false",
                                                           h3("No areas imported")
                                          )
                         ),
                         conditionalPanel(condition = "output.have_data === false",
                                          h3("No data imported")
                         )
         ))
),
tabPanel("exchangesStack",
         fluidRow(column(12,
                         conditionalPanel(condition = "output.have_data",
                                          conditionalPanel(condition = "output.have_data_links",
                                                           uiOutput("exchangesStack_ui")
                                          ),
                                          conditionalPanel(condition = "output.have_data_links === false",
                                                           h3("No links imported")
                                          )
                         ),
                         conditionalPanel(condition = "output.have_data === false",
                                          h3("No data imported")
                         )
         ))
),
tabPanel("tsPlot",
         fluidRow(column(12,
                         conditionalPanel(condition = "output.have_data",
                                          conditionalPanel(condition = "output.have_data_areas",
                                                           uiOutput("plotts_ui")
                                          ),
                                          conditionalPanel(condition = "output.have_data_areas === false",
                                                           h3("No areas imported")
                                          )
                         ),
                         conditionalPanel(condition = "output.have_data === false",
                                          h3("No data imported")
                         )
         ))
),
navbarMenu("plotMap", 
           tabPanel("Layout Builder", 
                    fluidRow(column(12,
                                    conditionalPanel(condition = "output.have_data",
                                                     antaresViz:::changeCoordsUI("ml")
                                    ),
                                    conditionalPanel(condition = "output.have_data === false",
                                                     h3("No data imported")
                                    )
                                    
                    ))
           ),
           tabPanel("Current Layout",
                    fluidRow(column(12,
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
                                    
                    ))
                    
           ),
           tabPanel("Map", 
                    fluidRow(column(12,
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
                    ))
           )
), 
tabPanel("Parameters", 
         fluidRow(
           column(2, checkboxInput("is_shared_input", label = "Share inputs between modules ?", value = TRUE)),
           column(2, h4("readAntares RAM limit (in Go) : ")),
           column(3, div(numericInput("ram_limit", label = NULL, 
                                      min = 1, max = 10, value = {
                                        if(!is.null(getOption("maxSizeLoad"))){
                                          getOption("maxSizeLoad")
                                        } else {10}
                                      }), align = "center")),
           column(2, h4("antaresViz data module (in Mb) : ")),
           column(3, div(numericInput("data_module", label = NULL, 
                                      min = 1, max = 10, value = {
                                        if(!is.null(getOption("antaresVizSizeGraph"))){
                                          getOption("antaresVizSizeGraph")
                                        } else {200}
                                      }), align = "center"))
           
         )
),
tabPanel("Help", 
         fluidRow(
           column(width = 12,
                  HTML(text = "For any questions, please contact <a href='mailto:rte-antares-rpackage@rte-france.com;'> RTE-ANTARES-RPACKAGE Team </a>.<br> <hr>"),
                  tabsetPanel(
                    tabPanel("R function readAntares", 
                             fluidRow(
                               column(12, includeHTML("www/readAntares.html"))
                             )
                    ),
                    tabPanel("R function removeVirtualAreas", 
                             fluidRow(
                               column(12, includeHTML("www/removeVirtualAreas.html"))
                             )
                    )
                    
                  )
           )
         )
         
),
footer = div(hr(), actionButton("quit", "Quit application", icon = icon("sign-out")), align = "center")
)




