lr <- data.frame(read.csv('log/lr.log', header = FALSE, sep = '|'))
dccc <- data.frame(read.csv('log/dccc.log', header = FALSE, sep = '|'))

get_best <- function(df, df_name) {
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
  df <- df[df$df_name == df_name, ]
  max_acc <- aggregate(df$accuracy, by = list(df$submodel_name), max)
  max_f1 <- aggregate(df$f1_score, by = list(df$submodel_name), max)
  #  df <- df[df$nsets == 100,]
  #  df <- df[df$mset == 1000,]
  min_train_time <- aggregate(df$train_time, by = list(df$submodel_name), max)
  min_test_time <- aggregate(df$test_time, by = list(df$submodel_name), max)
  
  write.table(
    # df_name submodel_name nsets mset nfeatures train_time test_time accuracy f1_score
    data.frame(
      df_name = c(df_name),
      submodel_name = max_acc$Group.1,
      max_acc = max_acc$x,
      max_f1 = max_f1$x,
      min_train_time = min_train_time$x,
      min_test_time = min_test_time$x
    ),
    file = "log/results.log",
    append = TRUE,
    quote = FALSE,
    sep = '|',
    col.names = FALSE,
    row.names = FALSE
  )
}
get_best(lr, 'lr')
get_best(dccc, 'dccc')