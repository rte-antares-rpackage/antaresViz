tabPanel(textOutput("label_tab_parameters"),
         fluidRow(
           column(2, checkboxInput("is_shared_input", label = "Share inputs between modules ?", value = TRUE)),
           column(3, h4(textOutput("title_readAntaresRAM"))),
           column(2, div(numericInput("ram_limit", label = NULL, 
                                      min = 1, max = 10, value = {
                                        if(!is.null(getOption("maxSizeLoad"))){
                                          getOption("maxSizeLoad")
                                        } else {10}
                                      }), align = "center")),
           column(3, h4(textOutput("title_antaresVizRAM"))),
           column(2, div(numericInput("data_module", label = NULL, 
                                      min = 1, max = 10, value = {
                                        if(!is.null(getOption("antaresVizSizeGraph"))){
                                          getOption("antaresVizSizeGraph")
                                        } else {200}
                                      }), align = "center"))
           
         )
)