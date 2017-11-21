library(shiny)
library(gplots)
library(reshape2)
library(plotly)
library(ggplot2)
# library(RColorBrewer)

shinyServer(function(input, output, session) {

  ###### reactive attributes for UI ######
  output$select1 <- renderUI({
    if(is.null(input$gene))
      return(NULL)
    selectizeInput(inputId = 'experiment', label = 'Please select experiments',
                   choices = sort(experimentReact()), multiple = TRUE, options = list(create = FALSE))
  })

  output$select2 <- renderUI({
    if(is.null(input$experiment))
      return(NULL)
    selectizeInput(inputId = 'condition', label = 'Please select conditions',
                   choices = conditionReact(), multiple = TRUE, options = list(create = FALSE))
  })

  experimentReact <- reactive({
    experiments <- experiment %>% inner_join(gene_expression, by = c('id' = 'experiment_id' )) %>%
      inner_join(gene, by = c('gene_id' = 'id')) %>%
      filter(name == input$gene)  %>% distinct(description)
    experiments$description
  })

  conditionReact <- reactive({
    getDesc_conditions(input$experiment)
  })

  updateSelectizeInput(session, inputId = 'gene', choices = sort(gene$name), server = TRUE)



  ###### Application logics ######
  # Get descriptions and conditions as list,
  # In order to select conditions that match with the description
  # The format: description-condition
  getDesc_conditions <- function(descriptions){
    conditions_list <- list()
    for(i in 1:length(descriptions)){
      conditions <- condition %>% inner_join(gene_expression, by = c('id' = 'condition_id')) %>%
        inner_join(gene, by = c('gene_id'= 'id')) %>%
        inner_join(experiment, by = c('experiment_id' = 'id')) %>%
        filter(description == descriptions[[i]] & name.y == input$gene) %>% distinct(name.x)
      conditions_list[[i]] = conditions$name.x
    }
    desc_cond_list <- rbind(descriptions, conditions_list)
    desc_cond_list <- unlist(apply(desc_cond_list, 2, function(x){paste(x$descriptions,":",x$conditions_list)}))
    return(desc_cond_list)
  }


  getExperiments <- function(){
    experiments <- eventReactive(input$barchart,{
      input$experiment
    })
    return(experiments())
  }

  ############################# input$condition - start ##################################
  # Get a list of experiments and a list of conditions from input$condition
  # Two lists have the same length, because in input$condition,
  # the format will be "condition  species disease plate  date : condition"
  # so it is like key<->value style. Then split them by ":", we get two same length lists.
  # Therefore, we can get all the conditions that match its experiment

  getSelectedDesc_cond_list <- function(){
    desc_cond_list <- eventReactive(input$barchart, {
      input$condition
    })
    return(desc_cond_list())
  }

  getSelectedExperiments <- function(){
    desc_cond_list <- getSelectedDesc_cond_list()
    descs <- unlist(lapply(desc_cond_list, function(x){sub("( : .*)","", x)}))
    return(descs)
  }

  getSelectedCondtions <- function(){
    desc_cond_list <- getSelectedDesc_cond_list()
    condtions <- unlist(lapply(desc_cond_list, function(x){sub("(.*?: )","", x)}))
    return(condtions)
  }

  # Get the conditions list correspond to the 1 particular experiment and 1 gene
  getConditionsList <- function(experiment){
    conditionsList <- list()
    selectedExperiments <- getSelectedExperiments()
    selectedConditions <- getSelectedCondtions()
    for(i in 1:length(selectedExperiments)){
      if(experiment %in% selectedExperiments[i]){
        conditionsList[[i]] <- selectedConditions[i]
      }
    }
    conditionsList <- Filter(Negate(is.null), conditionsList)
    return(conditionsList)
  }

  ############################# input$condition - end ##################################

  getBarChartDat <- function(){
    experiments <- getExperiments()
    conditions <- list()

    for(i in 1:length(experiments)){
      conditions[[i]] <- getConditionsList(experiments[i])
    }
    datList <- list()

    for(i in 1:length(experiments)){
      if(length(conditions[[i]]) == 0){
        dat <- gene_expression %>% inner_join(gene, by = c('gene_id' = 'id')) %>%
          inner_join(experiment, by = c('experiment_id' = 'id')) %>%
          inner_join(condition, by = c('condition_id' = 'id')) %>%
          filter(description == experiments[i] & name.x == input$gene) %>%
          select(expression, name.y)
        colnames(dat) <- c('expression', 'condition')
        dat <- aggregate(expression ~ condition, data = dat, FUN = 'mean')
        datList[[i]] <- dat
      }else{
        dat <- gene_expression %>% inner_join(gene, by = c('gene_id' = 'id')) %>%
          inner_join(experiment, by = c('experiment_id' = 'id')) %>%
          inner_join(condition, by = c('condition_id' = 'id')) %>%
          filter(description == experiments[i] & name.y %in% conditions[[i]] & name.x == input$gene) %>%
          select(expression, name.y)
        colnames(dat) <- c('expression', 'condition')
        dat <- aggregate(expression ~ condition, data = dat, FUN = 'mean')
        datList[[i]] <- dat
      }

    }
    return(datList)
  }


  getBoxPlotDat <- function(){
    experiments <- getExperiments()
    conditions <- list()

    for(i in 1:length(experiments)){
      conditions[[i]] <- getConditionsList(experiments[i])
    }
    datList <- list()

    for(i in 1:length(experiments)){
      dat <- gene_expression %>% inner_join(gene, by = c('gene_id' = 'id')) %>%
        inner_join(experiment, by = c('experiment_id' = 'id')) %>%
        inner_join(condition, by = c('condition_id' = 'id')) %>%
        filter(description == experiments[i] & name.y %in% conditions[[i]]  & name.x == input$gene) %>%
        select(expression, name.y, description)
      colnames(dat) <- c('expression', 'condition', 'experiment')
      datList[[i]] <- dat
    }
    boxPlotDat <- do.call("rbind", datList)
    return(boxPlotDat)
  }

############################### ANOVA - start #################################

  getANOVAdat <- function(){
    experiments <- getExperiments()
    conditions <- list()

    for(i in 1:length(experiments)){
      conditions[[i]] <- getConditionsList(experiments[i])
    }

    dataList <- list()
    anovaResults <- list()

    for(i in 1:length(experiments)){
      dat <- gene_expression %>% inner_join(gene, by = c('gene_id' = 'id')) %>%
        inner_join(experiment, by = c('experiment_id' = 'id')) %>%
        inner_join(condition, by = c('condition_id' = 'id')) %>%
        filter(description == experiments[i] & name.y %in% conditions[[i]]  & name.x == input$gene) %>%
        select(name.x, description, name.y, expression)
      colnames(dat) <- c('gene', 'experiment', 'condition', 'expression')
      if(length(conditions[[i]]) > 1){
        fit <- lm(dat$expression ~ dat$condition)
        result <- anova(fit, data = dat)
        result$condition.name <- paste(conditions[[i]], collapse = " vs ")
        result$gene.name <- input$gene
        result$experiment <- experiments[i]
        anovaResults[[i]] <- result
      }
    }

    return(anovaResults)

  }

  anovaResults2table <- function(){
    results <- getANOVAdat()
    dfTable <- sapply(results, function(x){
      c(gene_name = x$gene.name[2],
        experiment = x$experiment[2],
        conditions_names = x$condition.name[1],
        p.value = x$`Pr(>F)`[1],
        "F value" = x$`F value`[1]
      )
    })
    return(data.frame(t(dfTable)))
  }

  ############################### ANOVA - end ##################################
  getDownloadDat <- function(){
    experiments <- getExperiments()
    conditions <- list()

    for(i in 1:length(experiments)){
      conditions[[i]] <- getConditionsList(experiments[i])
    }
    datList <- list()

    for(i in 1:length(experiments)){
      dat <- gene_expression %>% inner_join(gene, by = c('gene_id' = 'id')) %>%
        inner_join(experiment, by = c('experiment_id' = 'id')) %>%
        inner_join(condition, by = c('condition_id' = 'id')) %>%
        filter(description == experiments[i] & name.y %in% conditions[[i]]  & name.x == input$gene) %>%
        select(name.x, description, name.y, expression)
      colnames(dat) <- c('gene', 'experiment', 'condition', 'expression')
      datList[[i]] <- dat
    }
    downloadDat <- do.call("rbind", datList)
    return(downloadDat)
  }

  ############ Plotting BarChart #############

  observeEvent(input$barchart, {

    output$barchartExp <- renderPlotly({
      if(is.null(input$experiment)){
        return(NULL)
      }
      barChartDat <- getBarChartDat()
      barChartList <- list()

      for(i in 1:length(barChartDat)){
        bar <- plot_ly(data = barChartDat[[i]], x = ~condition, y = ~expression, type = 'bar',
                       margin = c(12,10), name = getExperiments()[i]) %>%
          layout(yaxis = list(title = "AVG"), xaxis = list(title = ""), margin = list(b = 160), legend = list(x = 5, y = 5))
        barChartList[[i]] <- bar
      }
      barchart <- subplot(barChartList, shareY = TRUE,  which_layout = "merge", margin = 0.02, heights = 0.9)
      barchart
    })
  })

  ############ Plotting BoxPlot ##############
  observeEvent(input$barchart, {
    output$boxplot <- renderPlot({
      boxPlotDat <- getBoxPlotDat()
      boxPlotDat$exp_con <- apply(boxPlotDat[,c('experiment', 'condition')], 1, paste, collapse = " : ")
      xlabels <- levels(factor(boxPlotDat$exp_con))
      ggplot(boxPlotDat, aes(x = exp_con, y = expression, fill = experiment)) +
        geom_boxplot(position=position_dodge(0.8)) +
        geom_dotplot(binaxis = "y", stackdir = "center", position=position_dodge(0.8)) +
        # Rename the labels, take values from input$conditions as experiment:condition format,
        # and remove the experiment part.
        scale_x_discrete(labels = function(x){sub(".* : ","",x)}) +
        theme(axis.text.x = element_text(angle = 315, vjust = 0.5, size=12),
              axis.title.x=element_blank(), axis.title.y=element_blank(),
              legend.position = "bottom", legend.box = "vertical",
              legend.text=element_text(size=12))

    })
  })

  # observeEvent(input$barchart, {
  #   output$boxplot <- renderPlot({
  #     boxPlotDat <- getBoxPlotDat()
  #     boxPlotDat$exp_con <- apply(boxPlotDat[,c('experiment', 'condition')], 1, paste, collapse = " : ")
  #     xlabels <- levels(factor(boxPlotDat$exp_con))
  #     ggplot(boxPlotDat, aes(x = exp_con, y = expression, fill = experiment), group = experiment,
  #            colour = experiment) +
  #       geom_boxplot(position=position_dodge(0.8)) +
  #       geom_dotplot(binaxis = "y", stackdir = "center", position=position_dodge(0.8)) +
  #       # Rename the labels, take values from input$conditions as experiment:condition format,
  #       # and remove the experiment part.
  #       scale_x_discrete(labels = function(x){sub(".* : ","",x)}) +
  #       theme(axis.text.x = element_text(angle = 315, vjust = 0.5, size=12),
  #             legend.position = "bottom", legend.box = "vertical",
  #             legend.text=element_text(size=12),
  #             axis.title.x=element_blank(), axis.title.y=element_blank()) +
  #       scale_colour_brewer(palette = "Greens")
  #
  #
  #   })
  # })

  ############ Show Data Table and Download CSV #############
  downloadCSV <- function(){
    observeEvent(input$barchart,{
      if(is.null(input$gene) | is.null(input$condition) | is.null(input$experiment)){
        return(NULL)
      }
      dat <- getDownloadDat()
      output$csvdownload <- downloadHandler(
        filename = paste(input$gene, ".csv"),
        content = function(file){
          write.csv(dat, file)
        }
      )
      output$showDataTable <- renderTable(dat, digits = 6)
    })
  }

  downloadCSV()

  ############## Show ANOVA results ##############
  observeEvent(input$barchart, {
    output$ANOVA_results <- renderDataTable(anovaResults2table())
  })

  # Disconnect mysql
  session$onSessionEnded(function(){lapply(dbListConnections(dbDriver(drv="MySQL")), dbDisconnect)})
})
