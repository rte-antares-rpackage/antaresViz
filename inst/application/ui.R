# Define UI for application that draws a histogram
navbarPage("antaresViz", id = "nav-id",
           tabPanel("Data", 
                    h3("Directory selection"),
                    fluidRow(
                      column(7, 
                             directoryInput('directory', label = 'Select a directory', value = 'C:\\Users\\Datastorm\\Desktop\\antares\\test_case')
                      ), 
                      conditionalPanel(condition = "output.ctrl_is_antares_study", 
                                       column(3, 
                                              selectInput("study_path", "Study", choices = NULL, selected = NULL)
                                       ), 
                                       column(2, 
                                              div(br(), 
                                                  actionButton("init_sim", "Set simulation", icon = icon("check-circle")),
                                                  align = "center"
                                              )
                                       )
                      ),
                      conditionalPanel(condition = "output.ctrl_is_antares_study === false", 
                                       column(5, 
                                              h3("No antares output found in directory", 
                                                 style = "color : red")
                                       )
                      )
                    ), 
                    hr(), 
                    conditionalPanel(condition = "output.have_study", 
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
                                     ), 
                                     hr(), 
                                     div(actionButton("import_data", "Validate & import data", icon = icon("upload")), align = "center")
                    )
           ),
           tabPanel("prodStack",
                    conditionalPanel(condition = "output.have_data",
                                     mwModuleUI(id = "prodStack", height = "800px")
                    ),
                    conditionalPanel(condition = "output.have_data === false",
                                     h3("No data imported")
                    )
           ),
           tabPanel("exchangesStack",
                    conditionalPanel(condition = "output.have_data",
                                     conditionalPanel(condition = "output.have_links", 
                                                      mwModuleUI(id = "exchangesStack", height = "800px")
                                     ), 
                                     conditionalPanel(condition = "output.have_links === false", 
                                                      h3("No links imported")
                                     )
                    ),
                    conditionalPanel(condition = "output.have_data === false",
                                     h3("No data imported")
                    )
           ),
           tabPanel("plotts", 
                    conditionalPanel(condition = "output.have_data",
                                     mwModuleUI(id = "plotts", height = "800px")
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
                                                mwModuleUI(id = "plotMap", height = "800px")
                               ),
                               conditionalPanel(condition = "output.have_data === false",
                                                h3("No data imported")
                               )
                      )
           )
)




