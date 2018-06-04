library("shiny")

lr <- data.frame(read.csv('log/lr.log', header=FALSE, sep='|'))
dccc <- data.frame(read.csv('log/dccc.log', header=FALSE, sep='|'))

extractChosentLogs <- function(df){
  df<-lr
  colnames(df) <- c(
    "df_name",
    "submodel_name",
    "nsets",
    "mset",
    "nfeatures",
    "train_time",
    "test_time",
    "accuracy",
    "f1_score"
  )
  df <- df[df$submodel_name =='rpart',]
  df <-  df[df$nsets == 100,]
  df <-  df[df$mset == 1000,]
  df
}

rpart <- extractChosentLogs(lr);

df = data.frame(dr$accuracy, AMK, SK, JR)

library(ggplot2)
library(dplyr)
library(tidyr)

ggplot(rpart, mapping = aes(x = nfeatures, y = accuracy) ) + geom_line()
