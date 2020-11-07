library(shiny)


shinyUI(fluidPage(
    
    
    titlePanel("Old Faithful Geyser Data"),
    
    tabsetPanel(tabPanel("plot",
                         h1("Graficas en shiny"),
                         plotOutput("grafica_base_r"),
                         plotOutput("grafica_ggplot")
    ),
    tabPanel("Todas las opciones",
             plotOutput("opc_clk",
                        click = 'clk',
                        dblclick = 'dblclick',
                        hover = 'hover',
                        brush = 'brush'   ),
             verbatimTextOutput("difopc")
    ),
    tabPanel('Nuevas graficas',
             plotOutput('pl2',
                        click = 'click_pl2',
                        dblclick = 'dblclck_pl2',
                        hover = 'hover_pl2',
                        brush = 'brush_pl2'
             ),
             DT::dataTableOutput('graficosnuevos')
    )
    )
    
))