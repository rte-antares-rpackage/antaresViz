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
                    ),
                    tabPanel("R function writeAntaresH5", 
                             fluidRow(
                               column(12, includeHTML("www/writeAntaresH5.html"))
                             )
                    )
                  )
                  
           )
           
         )
)