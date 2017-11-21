library(shiny)
library(plotly)
library(shinythemes)
shinythemes::themeSelector()

shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        
        wellPanel(
          selectizeInput(inputId = 'gene', label = 'Please select a gene', 
                         choices = NULL, options = list(create = FALSE)),
          uiOutput("select1"),
          uiOutput("select2"),
          actionButton("barchart", label = "Bar Chart & Box Plot"),
          downloadButton("csvdownload", label = "Download Data CSV")
        )
      )
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        tabPanel("Bar Chart", plotlyOutput("barchartExp", width = 1000, height = 600)),
        tabPanel("Box Plot", plotOutput("boxplot")),
        tabPanel("Data Table", tableOutput("showDataTable")),
        tabPanel("ANOVA", dataTableOutput("ANOVA_results"))
      )
     
    )
  )
))
