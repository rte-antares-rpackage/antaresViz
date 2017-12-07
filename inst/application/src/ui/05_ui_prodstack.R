tabPanel("prodStack",
         fluidRow(
           column(12,
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
           )
         )
)