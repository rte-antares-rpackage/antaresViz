# Define UI for antaresViz app
navbarPage("antaresViz", id = "nav-id",
           tabPanel("Data", 
                    h3("Antares study selection"),
                    fluidRow(
                      column(7, 
                             directoryInput('directory', label = 'Select an antares study', 
                                            value = '')
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
                                                        ),
                                                        column(3, 
                                                               checkboxInput("read_parallel", "parallel", FALSE)
                                                        )
                                                      )
                                     ), 
                                     fluidRow(
                                       column(12, 
                                              selectInput("read_select", "Select :", choices = NULL, selected = NULL, 
                                                          width = "100%", multiple = TRUE)
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
                    conditionalPanel(condition = "output.have_data",
                                     conditionalPanel(condition = "output.have_data_areas", 
                                                      mwModuleUI(id = "prodStack", height = "800px")
                                     ), 
                                     conditionalPanel(condition = "output.have_data_areas === false", 
                                                      h3("No areas imported")
                                     )
                    ),
                    conditionalPanel(condition = "output.have_data === false",
                                     h3("No data imported")
                    )
           ),
           tabPanel("exchangesStack",
                    conditionalPanel(condition = "output.have_data",
                                     conditionalPanel(condition = "output.have_data_links", 
                                                      mwModuleUI(id = "exchangesStack", height = "800px")
                                     ), 
                                     conditionalPanel(condition = "output.have_data_links === false", 
                                                      h3("No links imported")
                                     )
                    ),
                    conditionalPanel(condition = "output.have_data === false",
                                     h3("No data imported")
                    )
           ),
           tabPanel("tsPlot", 
                    conditionalPanel(condition = "output.have_data",
                                     conditionalPanel(condition = "output.have_data_areas", 
                                                      mwModuleUI(id = "plotts", height = "800px")
                                     ), 
                                     conditionalPanel(condition = "output.have_data_areas === false", 
                                                      h3("No areas imported")
                                     )
                    ),
                    conditionalPanel(condition = "output.have_data === false",
                                     h3("No data imported")
                    )
           ),
           navbarMenu("plotMap", 
                      tabPanel("Layout", 
                               conditionalPanel(condition = "output.have_data",
                                                antaresViz:::changeCoordsUI("ml")
                               ),
                               conditionalPanel(condition = "output.have_data === false",
                                                h3("No data imported")
                               )
                               
                      ),
                      tabPanel("Map", 
                               conditionalPanel(condition = "output.have_data",
                                                conditionalPanel(condition = "output.must_print_map", 
                                                                 mwModuleUI(id = "plotMap", height = "800px")
                                                ), 
                                                conditionalPanel(condition = "output.must_print_map === false", 
                                                                 h3("Please set a map layout before.")
                                                )
                               ),
                               conditionalPanel(condition = "output.have_data === false",
                                                h3("No data imported")
                               )
                      )
           ), 
           footer = div(hr(), actionButton("quit", "Quit application", icon = icon("sign-out")), align = "center")
)




