library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

shinyUI(fluidPage(
    
    h3(id="h1",HTML("<b>Happiness Levels and Alcohol Consumption</b>")),
    tags$head(tags$style(HTML("#h1{color : #3366ff;}"))) ,
    tags$br(),
    
    tabsetPanel(
        
        tabPanel("Plots",
                 sidebarLayout(
                     sidebarPanel(
                         h4(HTML('<b>Plot Metrics</b>')),    
                         radioButtons("xaxis",label = "Select X axis Metric",choices = list(Beer = 7,Wine = 9,Spirits = 8 , HDI = 5)),
                         
                         radioButtons("colorby",label = "Group points By",choices = list(None = 0,Region = 2,Hemisphere =  3)),
                         
                         checkboxInput("legend",label = "Show/Hide Legend",value = TRUE),
                         checkboxInput("reglines",label = "Show/Hide Regression Lines",value = TRUE),
                         h3("Slope"),
                         #The category variables are to show the region/hemisphere the point the user hovers on belongs to, for bothe slope and intercept
                         htmlOutput("category"),
                         tags$b("Value : "),
                         htmlOutput("slope",style = "display : inline"),
                         h3("Intercept"),
                         htmlOutput("category2"),
                         tags$b("Value : "),
                         htmlOutput("intercept",style = "display : inline")), mainPanel(plotlyOutput("plot1")))
                 ),
        
        
        tabPanel("Data",
                 sidebarLayout(sidebarPanel(
                     tags$b("Select range of rows to display"),
                     sliderInput("numrows",1,122,label = "",c(min,max)),
                     sliderInput("hscore",3.069,7.526,label = "Select range of Happiness Scores",c(min,max)),
                     tags$br(),
                     selectInput(inputId = "filter",label = "Filter Data By",choices = list("None","Hemisphere" = c("North", "South","Both"), "Region" = c("Australia and New Zealand","Central and Eastern Europe",      "Eastern Asia","Latin America and Caribbean","Middle East and Northern Africa","North America","Southeastern Asia","Sub-Saharan Africa","Western Europe")), selected = "None"),
                     
                     tags$b('Description of the data set'),
                     tags$br(),tags$br(),
                     htmlOutput("datadesc"),
                     tags$head(tags$style(HTML("#datadesc{margin-left:-3em; font-size : 11px}")))
                 ),mainPanel(tableOutput("showdata")))
                 ),
        
        tabPanel("Documentation",includeCSS("App-Documentation.html"))
        
        # mainPanel(
        #     tabsetPanel(type = "tabs", id = "tabselected",
        #                 tabPanel("Plots", plotlyOutput("plot1"),value=1),
        #                 tabPanel("Data",value=2,tableOutput("showdata")),
        #                 tabPanel("Documentation",value=3,includeCSS("App-Documentation.html"))
        #                 
        #     ))
            
        )
))
