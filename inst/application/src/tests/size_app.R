library(shiny)
library(manipulateWidget)
library(antaresViz)
library(pryr)

ui <- fluidPage(
  uiOutput("io"),
  actionButton("goButton", "Go!")
)


server <- function(input, output, session) {
  
  tmp_prodstack <- NULL
  
  data <- reactiveValues(opts = setSimulationPath("C:\\Users\\Datastorm\\Desktop\\antares\\20171114-1533eco-base_30mc.h5"))
  
  output$io <- renderUI({
    mwModuleUI(paste0("mod2"), height = "800px")
  })
  
  observe({

    print("object_size(session)")
    print(object_size(session))
    input$goButton
    isolate({
      prodStack <- prodStack(data$opts, xyCompare = "union",
                             unit = "GWh", interactive = TRUE, .updateBtn = TRUE, 
                             .updateBtnInit = TRUE, .runApp = FALSE)
      
      if("MWController" %in% class(tmp_prodstack)){
        tmp_prodstack$clear()
      }
      tmp_prodstack <<- mwModule(id = paste0("mod2"),  prodStack)
    })
  })

}

shinyApp(ui, server)