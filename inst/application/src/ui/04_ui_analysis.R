tabPanel(textOutput("label_tab_analysis"),
         conditionalPanel(condition = "output.have_data === true",
                          div(h3(textOutput("title_analysis")), align = "center"),
                          h3(textOutput("title_studies")),
                          uiOutput("info_list"),
                          h3(textOutput("title_compare")),
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
                          
                          checkboxInput("sel_compare_mcyear", "mcYear on all modules ?", FALSE, width = "100%"),
                          
                          br(),
                          div(actionButton("update_module", "Launch Analysis", icon = icon("upload")), align = "center")
         ),
         conditionalPanel(condition = "output.have_data === false",
                          h3(textOutput("no_data_7"), style = "color : red")
         )
)