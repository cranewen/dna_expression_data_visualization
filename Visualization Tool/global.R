library(shiny)
library(RMySQL)
library(DBI)
library(dplyr)
library(pool)

mysqldbpool <- dbPool(
  RMySQL::MySQL(),
  dbname = "b_dna",
  host = "127.0.0.1",
  username = "username",
  password = "password"
)

gene <- data.frame(mysqldbpool %>% tbl("gene"))
condition <- data.frame(mysqldbpool %>% tbl("condition"))
experiment <- data.frame(mysqldbpool %>% tbl("experiment"))
gene_expression <- data.frame(mysqldbpool %>% tbl("gene_expression"))

# gene_expression_df <- data.frame(gene_expression)
# exp1data <- gene_expression_df %>% filter(experiment_id == 1) %>% select(expression)

# globalPlotObj <- list()
