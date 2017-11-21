library(shiny)
library(gplots)
library(reshape2)
library(plotly)
library(ggplot2)


shinyServer(function(input,output,session){
  conditionReact <- reactive({
    # desc <- input$description
    conditions <- condition %>% inner_join(gene_expression, by = c('id' = 'condition_id')) %>%
      inner_join(experiment, by = c('experiment_id' = 'id')) %>%
      filter(description == input$description) %>% distinct(name)
    conditions$name
  })
  
  geneReact <- reactive({
      genes <- gene %>% inner_join(gene_expression, by = c('id' = 'gene_id')) %>%
        inner_join(experiment, by = c('experiment_id' = 'id')) %>%
        inner_join(condition, by = c('condition_id' = 'id')) %>%
        filter((description == input$description)) %>% distinct(name.x)
      genes$name.x
  })
  
  
  updateSelectizeInput(session, inputId = 'description', choices = sort(experiment$description), server = TRUE)
  
  # select input for conditions
  output$select1 <- renderUI({
    if(is.null(input$description))
      return(NULL)
    selectizeInput(inputId = 'condition', label = 'Please select conditions', 
                   choices = sort(conditionReact()), multiple = TRUE, options = list(create = FALSE))
  })
  
  # select input for genes
  output$select2 <- renderUI({
    if(is.null(input$condition))
      return(NULL)
    selectizeInput(inputId = 'gene', label = 'Please select genes', 
                   choices = sort(geneReact()), multiple = TRUE, options = list(create = FALSE))
  })
  
 
  
  getDescription_hmap <- function(){
    desc <- eventReactive(input$hmap, {
      input$description
    })
    return(desc())
  }
  
  getDescription_barChart <- function(){
    desc <- eventReactive(input$barchart, {
      input$description
    })
    return(desc())
  }
  
  getDescription_geneComp <- function(){
    desc <- eventReactive(input$geneComp, {
      input$description
    })
    return(desc())
  }
  
  # For clicking on "Show Data Table"
  getDescription_csv <- function(){
    desc <- eventReactive(input$csvtable, {
      input$description
    })
    return(desc())
  }
  # For clicking on barchart button
  getConditionsNames <- function(){
    conditions <- eventReactive(input$barchart, {
      input$condition
    })
    return(conditions())
  }
  
  # For clicking on "Show Data Table"
  getConditionsNames_csv <- function(){
    conditions <- eventReactive(input$csvtable, {
      input$condition
    })
    return(conditions())
  }
  
  getGeneNames <- function(){
    geneNames <- eventReactive(input$barchart, {
      input$gene
    })
    return(geneNames())
  }
  
  # For clicking on "Show Data Table"
  getGeneNames_csv <- function(){
    geneNames <- eventReactive(input$csvtable, {
      input$gene
    })
    return(geneNames())
  }
  
  getConditionsDat <- function(){
    conditionsNames <- getConditionsNames()
    conditions <- list()
    geneNames <- getGeneNames()
    lenOfConditionNames <- length(conditionsNames)
    
    if(length(geneNames) == 0){
      for(i in 1:lenOfConditionNames){
        cond <- experiment %>% inner_join(gene_expression, by = c("id" = "experiment_id")) %>%
          inner_join(condition, by = c("condition_id" = "id")) %>%
          inner_join(gene, by = c("gene_id" = "id")) %>%
          filter(name.x == conditionsNames[i] & description == getDescription_barChart()) %>% 
          select(expression, name.y, name.x)
        singleCond <- aggregate(expression ~ name.y, data = cond, FUN = "mean")
        conditions[[i]] <- singleCond
      }
    }else {
      for(i in 1:lenOfConditionNames){
        cond <- experiment %>% inner_join(gene_expression, by = c("id" = "experiment_id")) %>%
          inner_join(condition, by = c("condition_id" = "id")) %>%
          inner_join(gene, by = c("gene_id" = "id")) %>%
          filter(name.x == conditionsNames[i] & description == getDescription_barChart() & name.y %in% geneNames) %>% 
          select(expression, name.y, name.x)
        singleCond <- aggregate(expression ~ name.y, data = cond, FUN = "mean")
        conditions[[i]] <- singleCond
      }
    }
    
    return(conditions)
  }

  # Get all the replicated gene expressions from 1 particular gene from multiple conditions,
  # name.y is gene name, name.x is condition name
  getGeneExpression <- function(geneNames, conditionNames){
    geneExpression <- experiment %>% inner_join(gene_expression, by = c("id" = "experiment_id")) %>%
          inner_join(condition, by = c("condition_id" = "id")) %>%
          inner_join(gene, by = c("gene_id" = "id")) %>%
          filter(name.x %in% conditionNames & description == getDescription_barChart() & name.y %in% geneNames) %>% 
          select(expression, name.y, name.x)
    colnames(geneExpression) <- c("expression", "gene", "condition")
    return(geneExpression)
  }
  
  # name.x is condition, name.y is gene
  getGeneExpression_3arg <- function(geneNames, conditionNames, desc){
    geneExpression <- experiment %>% inner_join(gene_expression, by = c("id" = "experiment_id")) %>%
      inner_join(condition, by = c("condition_id" = "id")) %>%
      inner_join(gene, by = c("gene_id" = "id")) %>%
      filter(name.x %in% conditionNames & description == desc & name.y %in% geneNames) %>% 
      select(name.y, name.x, expression)
    colnames(geneExpression) <- c("gene", "condition", "expression")
    return(geneExpression)
  }
  
  getBoxPlotDat <- function(){
    conditions <- getConditionsNames()
    geneNames <- getGeneNames()
    boxPlotDat <- getGeneExpression(geneNames = geneNames, conditionNames = conditions)
    return(boxPlotDat)
    
  }
  
  # Heat map table with input of a reactive description.
  getHeatmapDat <- function(){
    hmDat <- experiment %>% inner_join(gene_expression, by = c("id" = "experiment_id")) %>% 
      inner_join(gene, by = c("gene_id" = "id")) %>% 
      inner_join(condition, by = c("condition_id" = "id")) %>% 
      filter(description == getDescription_hmap()) %>% 
      select(c("name.x", "name.y", "expression"))
    colnames(hmDat) <- c("gene", "condition", "expression")
    return(hmDat)
  }
  
  hmapTable <- function(){
    castedDat <- dcast(getHeatmapDat(), gene ~ condition, value.var = "expression", mean)
    rownames(castedDat) <- castedDat$gene
    castedDat <- castedDat[,-1]
    return(castedDat)
  }
  
  hmapTablePW <- function(){
    datList <- list()
    castedDat <- dcast(getHeatmapDat(), gene ~ condition, value.var = "expression", mean)
    rownames(castedDat) <- castedDat$gene
    castedDat <- castedDat[,-1]
    nameCombs <- combn(names(castedDat),2)
    for(i in 1:dim(nameCombs)[2]){
      pairedDat <- log2(castedDat[nameCombs[,i][1]] / castedDat[nameCombs[,i][2]])
      colnames(pairedDat) <- paste(nameCombs[,i], collapse = " / ")
      datList[[i]] <- pairedDat
    }
    return(datList)
  }
  
  # tTestResults  <- function(){
  #   geneNames <- getGeneNames()
  #   conditionNames <- getConditionsNames()
  #   results <- list()
  #   validate(
  #     need(length(conditionNames) == 2, "Please select only 2 conditions!"),
  #     need(!is.null(geneNames), "Please select at least 1 gene")
  #   )
  #   for(i in 1:length(geneNames)){
  #     result <- t.test(getGeneExpression(geneNames[i],conditionNames[1])$expression, 
  #                      getGeneExpression(geneNames[i],conditionNames[2])$expression)
  #     result$data.name <- paste(conditionNames[1]," vs ",conditionNames[2])
  #     result$gene.name <- geneNames[i]
  #     results[[i]] <- result
  #   }
  #  
  #   return(results)
  # }
  # 
  # results2table <- function(){
  #   results <- tTestResults()
  #   dfTable <- sapply(results, function(x){
  #     c(p.value = x$p.value, 
  #       ci_low = x$conf.int[1],
  #       ci_high = x$conf.int[2],
  #       conditions_names = x$data.name,
  #       gene_name = x$gene.name
  #       )
  #   })
  #   return(data.frame(t(dfTable)))
  # }
  ###### ANOVA Results & generates tables for display ######
  anovaResults <- function(){
    geneNames <- getGeneNames()
    conditionNames <- getConditionsNames()
    results <- list()
    validate(
      need(length(conditionNames) > 1, "Please select at least 1 condition!"),
      need(!is.null(geneNames), "Please select at least 1 gene")
    )
    if(length(conditionNames) == 2){
      for(i in 1:length(geneNames)){
        anovaDat <- getGeneExpression(geneNames[i], conditionNames)
        fit <- lm(anovaDat$expression ~ anovaDat$condition)
        result <- anova(fit, data = anovaDat)
        print(result)
        result$data.name <- paste(conditionNames[1]," vs ",conditionNames[2])
        result$gene.name <- geneNames[i]
        results[[i]] <- result
      }
      return(results)
    }
    if(length(conditionNames) > 2){
      for(i in 1:length(geneNames)){
        anovaDat <- getGeneExpression(geneNames[i], conditionNames)
        fit <- lm(anovaDat$expression ~ anovaDat$condition)
        result <- anova(fit, data = anovaDat)
        result$data.name <- paste(conditionNames, collapse = " vs ")
        result$gene.name <- geneNames[i]
        results[[i]] <- result
      }
      return(results)
    }
  }

  anovaResults2table <- function(){
    results <- anovaResults()
    dfTable <- sapply(results, function(x){
      c(gene_name = x$gene.name[2],
        conditions_names = x$data.name[1],
        p.value = x$`Pr(>F)`[1],
        "F value" = x$`F value`[1]
      )
    })
    return(data.frame(t(dfTable)))
  }
  
  ########## Download CSV from selected data, also show the datatable###########
  downloadCSV <- function(){
    observeEvent(input$csvtable,{
      if(is.null(input$gene) | is.null(input$condition) | is.null(input$description)){
        return(NULL)
      }
      dat <- getGeneExpression_3arg(input$gene, input$condition, input$description)
      output$csvdownload <- downloadHandler(
        filename = "data.csv",
        content = function(file){
          write.csv(dat, file)
        }
      )
      output$csvTable <- renderTable(dat, digits = 6)
    })
  }
  downloadCSV()

  ###### Plotly Heatmap ######
  
  observeEvent(input$hmap, {
    output$heatmap <- renderPlotly({
      matrixDat <- data.matrix(hmapTable())
      hmap_df <- hmapTable()
      #labels for x and y axis
      xnames <- names(hmap_df)
      ynames <- rownames(hmap_df)
      palette <- colorRampPalette(c("#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695",
                                    "#FEE090", "#FDAE61"), space = "Lab", bias = 0.5)
      hmap <- plot_ly(x = xnames, y = ynames, z = matrixDat, type = "heatmap", colors = palette(50)) %>%
        layout(xaxis = list(title = "Conditions"), yaxis = list(title = ""),
               width = 900, height = 600, margin = list(b = 160))
      
    })
  })
  
  
  ###### Plotly Pairwised Heatmaps ######
  observeEvent(input$hmap2, {
    output$heatmap <- renderPlotly({
      hmapDats <- hmapTablePW()
      plots <- list()
      for(i in 1:length(hmapDats)){
        matrixDat <- data.matrix(hmapDats[[i]])
        hmap_df <- hmapDats[[i]]
        xnames <- names(hmap_df)
        ynames <- rownames(hmap_df)
        palette <- colorRampPalette(c("#65DAFD","#FFFFFF", "#FF0000"), space = "Lab", bias = 0.5)
        hmap <- plot_ly(x = xnames, y = ynames, z = matrixDat, type = "heatmap", colors = palette(10)) %>% 
          layout(xaxis = list(title = "Conditions"), yaxis = list(title = ""),
                 width = 900, height = 1200, margin = list(b = 160))
        plots[[i]] <- hmap
      }
      hmaps <- subplot(nrows =3, plots, margin = 0.05)
    })
  })
  
 
  
  ###### Plotly Barchart ######
   observeEvent(input$barchart, {
     output$barchartExp <- renderPlotly({
       if(is.null(input$condition))
         return(NULL)
       conditionsDat <- getConditionsDat()
       
       for(i in 1:length(conditionsDat)){
         if(i == 1){
           multiBar <- plot_ly(data = conditionsDat[[i]], x = ~name.y, y = ~expression, type = 'bar', 
                               margin = c(12,10), name = getConditionsNames()[i]) %>%
             layout(yaxis = list(title = "AVG"), xaxis = list(title = ""), margin = list(b = 160))
         }
         else{
           multiBar <- add_trace(multiBar, data = conditionsDat[[i]], x = ~name.y, y = ~expression, type = 'bar', 
                                 name = getConditionsNames()[i])
         }
       }
      multiBar
     })
   })
  
 ############# Plotly BoxPlot ################
  
  observeEvent(input$barchart, {
    output$boxplot <- renderPlot({
      boxPlotDat <- getBoxPlotDat()
      colnames(boxPlotDat) <- c("expression", "gene", "conditions")
      ggplot(boxPlotDat, aes(x = gene, y = expression, fill = conditions)) +
        geom_boxplot(position=position_dodge(0.8)) + 
        geom_dotplot(binaxis = "y", stackdir = "center", position=position_dodge(0.8)) +
        theme(axis.title.x=element_blank(), axis.title.y=element_blank())
      
    })
  })

  observeEvent(input$barchart, {
    output$ANOVA_results <- renderDataTable(anovaResults2table())
  })
  
 
  #####################################################

  # Disconnect mysql
  session$onSessionEnded(function(){lapply(dbListConnections(dbDriver(drv="MySQL")), dbDisconnect)})
  
})

