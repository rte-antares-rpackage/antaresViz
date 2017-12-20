tabPanel("Import Data",
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
                          tabsetPanel(id = "args",
                                      source("src/ui/02_ui_read_data.R", local = T)$value,
                                      source("src/ui/03_ui_convert_h5.R", local = T)$value
                          )
         )
)