tabPanel("Analysis",
         conditionalPanel(condition = "output.have_data === true",
                          div(h3("Analysis parameters"), align = "center"),
                          h3("Studies :"),
                          uiOutput("info_list"),
                          h3("Compare :"),
                          fluidRow(
                            
                            column(3, 
                                   selectInput("sel_compare_prodstack", "prodStack :", choices = .global_compare_prodstack, selected = NULL, multiple = TRUE)
                            ), 
                            column(3, 
                                   selectInput("sel_compare_exchangesStack", "exchangesStack :", choices = .global_compare_exchangesStack, selected = NULL, multiple = TRUE)
                            ), 
                            column(3, 
                                   selectInput("sel_compare_tsPlot", "tsPlot : ", choices = .global_compare_tsPlot, selected = NULL, multiple = TRUE)
                            ), 
                            column(3, 
                                   selectInput("sel_compare_plotMap", "plotMap :", choices = .global_compare_plotMap, selected = NULL, multiple = TRUE)
                            )
                          ), 
                          
                          checkboxInput("sel_compare_mcyear", "mcYear on all modules ?", FALSE),
                          
                          br(),
                          div(actionButton("update_module", "Launch Analysis", icon = icon("upload")), align = "center")
         ),
         conditionalPanel(condition = "output.have_data === false",
                          h3("No data imported from 'Import Data' panel", style = "color : red")
         )
)