library(shiny)
library(plotly)

shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      fluidRow(
      
        wellPanel(
          selectizeInput(inputId = 'description', label = 'Please select a description', 
                         choices = NULL, options = list(create = FALSE)),
          uiOutput("select1"),
          uiOutput("select2"),
          actionButton("barchart", label = "Bar Chart & Box Plot"),
          actionButton("hmap", label = "Heatmap"),
          actionButton("hmap2", label = "Pairwise Heatmap"),
          actionButton("csvtable", label = "Update Data Table"),
          downloadButton("csvdownload", label = "Download Data CSV")
        )
      )
      
    ),
    
    mainPanel(

      tabsetPanel(
        tabPanel("Heatmap", plotlyOutput("heatmap", width = "100%", height = "100%")),
        tabPanel("Bar Chart", plotlyOutput("barchartExp", width = 1000, height = 600)),
        tabPanel("Box Plot", plotOutput("boxplot")),
        tabPanel("Data Table", tableOutput("csvTable")),
        tabPanel("ANOVA", dataTableOutput("ANOVA_results"))
      )
      # tableOutput("csvTable")
    )
  )
))