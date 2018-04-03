tabPanel(textOutput("label_tab_exchanges"),
         fluidRow(
           column(12,
                  conditionalPanel(condition = "output.have_data",
                                   conditionalPanel(condition = "output.have_data_links",
                                                    uiOutput("exchangesStack_ui")
                                   ),
                                   conditionalPanel(condition = "output.have_data_links === false",
                                                    h3("No links imported")
                                   )
                  ),
                  conditionalPanel(condition = "output.have_data === false",
                                   h3(textOutput("no_data_2"))
                  )
           )
         )
)