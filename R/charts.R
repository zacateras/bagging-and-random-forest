
library(ggplot2)
library(dplyr)
library(tidyr)

lr <- data.frame(read.csv('log/lr.log', header=FALSE, sep='|'))
dccc <- data.frame(read.csv('log/dccc.log', header=FALSE, sep='|'))

extractChosentLogs <- function(df, model_name){
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
  df <- df[df$submodel_name == model_name,]
  df <-  df[df$nsets == 100,]
  df <-  df[df$mset == 1000,]
  df
}

lr <- extractChosentLogs(lr,'rpart');
dccc <- extractChosentLogs(dccc,'rpart');
#dccc <- extractChosentLogs(lr, 'bagging');

nfeatures <- lr$nfeatures
letter_recognition <-  lr$accuracy
default_detection <-  dccc$accuracy
df = data.frame(nfeatures, letter_recognition, default_detection)

dfplot <- df %>% gather(key, value, -nfeatures)
ggplot(dfplot , mapping = aes(x = nfeatures, y = value, color = key) ) + geom_line()
p + labs( y = "accuracy", title = "bootstrap replications: 1000, features drawn: 7", x = "model count")
