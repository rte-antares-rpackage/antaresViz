tabPanel(textOutput("label_tab_tsPlot"),
         fluidRow(
           column(12,
                  conditionalPanel(condition = "output.have_data",
                                   conditionalPanel(condition = "output.have_data_areas",
                                                    uiOutput("plotts_ui")
                                   ),
                                   conditionalPanel(condition = "output.have_data_areas === false",
                                                    h3(textOutput("no_areas_2"))
                                   )
                  ),
                  conditionalPanel(condition = "output.have_data === false",
                                   h3(textOutput("no_data_3"))
                  )
           )
         )
)