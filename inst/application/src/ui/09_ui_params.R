tabPanel("Parameters", 
         fluidRow(
           column(2, checkboxInput("is_shared_input", label = "Share inputs between modules ?", value = TRUE)),
           column(2, h4("readAntares RAM limit (in Go) : ")),
           column(3, div(numericInput("ram_limit", label = NULL, 
                                      min = 1, max = 10, value = {
                                        if(!is.null(getOption("maxSizeLoad"))){
                                          getOption("maxSizeLoad")
                                        } else {10}
                                      }), align = "center")),
           column(2, h4("antaresViz data module (in Mb) : ")),
           column(3, div(numericInput("data_module", label = NULL, 
                                      min = 1, max = 10, value = {
                                        if(!is.null(getOption("antaresVizSizeGraph"))){
                                          getOption("antaresVizSizeGraph")
                                        } else {200}
                                      }), align = "center"))
           
         )
)