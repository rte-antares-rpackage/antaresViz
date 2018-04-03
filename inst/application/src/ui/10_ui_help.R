tabPanel(textOutput("label_tab_help"), 
         fluidRow(
           column(width = 12,
                  HTML(text = "Support / Questions : <a href='mailto:rte-antares-rpackage@rte-france.com;'> RTE-ANTARES-RPACKAGE Team </a>.<br> <hr>"),
                  tabsetPanel(
                    tabPanel("readAntares", 
                             fluidRow(
                               column(12, includeHTML("www/readAntares.html"))
                             )
                    ),
                    tabPanel("removeVirtualAreas", 
                             fluidRow(
                               column(12, includeHTML("www/removeVirtualAreas.html"))
                             )
                    ),
                    tabPanel("writeAntaresH5", 
                             fluidRow(
                               column(12, includeHTML("www/writeAntaresH5.html"))
                             )
                    )
                  )
                  
           )
           
         )
)