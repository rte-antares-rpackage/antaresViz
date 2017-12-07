# Define UI for antaresViz app
navbarPage(title = "antaresViz", id = "nav-id", inverse= TRUE, collapsible = TRUE, position = "fixed-top",
           header = fluidRow(
             column(12, 
                    br(), br(), br(), 
                    singleton(tags$script(src = 'events.js')), 
                    div(id = "import_busy", tags$img(src= "spinner.gif", height = 100, 
                                                     style = "position: fixed;top: 50%;z-index:10;left: 48%;"))
             )
           ), windowTitle = "antaresViz",
           tabPanel("Data",
                    fluidRow(
                      column(12,
                             tabsetPanel(id = "tab_data",
                                         source("src/ui/01_ui_import_data.R", local = T)$value,
                                         source("src/ui/04_ui_analysis.R", local = T)$value
                             )
                      )
                    )
           ),
           
           source("src/ui/05_ui_prodstack.R", local = T)$value,
           
           source("src/ui/06_ui_exchange.R", local = T)$value,
           
           source("src/ui/07_ui_tsplot.R", local = T)$value,
           
           source("src/ui/08_ui_map.R", local = T)$value,
           
           source("src/ui/09_ui_params.R", local = T)$value,
           
           source("src/ui/10_ui_help.R", local = T)$value,
           
           footer = div(hr(), actionButton("quit", "Quit application", icon = icon("sign-out")), align = "center")
)



