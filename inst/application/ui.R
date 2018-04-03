# Define UI for antaresViz app
navbarPage(title = "antaresViz", id = "nav-id", inverse= TRUE, collapsible = TRUE, position = "fixed-top",
           header = div(
                    br(), br(), br(), 
                    singleton(tags$script(src = 'events.js')), 
                    singleton(tags$script(src = 'is.min.js')),
                    # footer
                    div(style = "position: fixed;left: 0;bottom: 0;width: 100%;background-color: black;border-radius: 0;color: white;text-align: center;
                        z-index: 99999;", div(actionLink("quit", "Quit application", icon = icon("sign-out"), style = "color:white"), align = "center")),
                    # flag : https://www.countries-ofthe-world.com/flags-of-the-world.html
                    img(src = "img/flag-of-France.png", style = "position: fixed;
                        cursor:pointer;
                        right: 0;
                        top: 0;
                        z-index: 10000;
                        margin-right: 1cm;
                        margin-top: 0.4cm;
                        display: block;
                        height: 21px;
                        text-decoration: none;
                        overflow-x: hidden;", onclick="updateShinyLanguage('fr')"),
                    img(src = "img/flag-of-United-Kingdom.png", style = "position: fixed;
                        cursor:pointer;
                        right: 0;
                        top: 0;
                        z-index: 10000;
                        margin-right: 2.2cm;
                        margin-top: 0.4cm;
                        display: block;
                        height: 21px;
                        text-decoration: none;
                        overflow-x: hidden;", onclick="updateShinyLanguage('en')"),
                    tags$script(type="text/javascript", 'if(is.ie()){ alert("Ce site n\'est pas optimisé pour Internet Explorer");};'),
                    
                    div(id = "import_busy", tags$img(src= "spinner.gif", height = 100, 
                                                     style = "position: fixed;top: 50%;z-index:10;left: 48%;"))
                    # selectInput("language", "langue:",
                    #             c("Français" = "fr",
                    #               "English" = "en"))
           ), windowTitle = "antaresViz",
           
           tabPanel(textOutput("label_tab_data"),
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
           footer = div(br(), br())
)



