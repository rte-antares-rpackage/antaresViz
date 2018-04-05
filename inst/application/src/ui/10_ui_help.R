tabPanel(textOutput("label_tab_help"), 
         fluidRow(
           column(width = 12,
                  h3("Support / Questions"),
                  HTML(text = "<ul>
    <li> email : <a href='mailto:rte-antares-rpackage@rte-france.com;'> RTE-ANTARES-RPACKAGE Team </a></li>
      <li>github : <a href='https://github.com/rte-antares-rpackage/antaresViz' target = '_blank'> antaresViz </a></li>
                       </ul><hr>"),
                  tabsetPanel(
                    tabPanel("readAntares", 
                             includeHTML("www/readAntares.html")
                             
                    ),
                    tabPanel("removeVirtualAreas", 
                            includeHTML("www/removeVirtualAreas.html")
                             
                    ),
                    tabPanel("writeAntaresH5", 
                              includeHTML("www/writeAntaresH5.html")
                             
                    )
                  )
                  
           )
           
         )
)