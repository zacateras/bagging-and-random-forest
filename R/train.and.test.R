train_and_test<- function(df, df_name, ycolname, submodel_name, classifier_funct, submodel_funct,
                          nrep=10, model_nset=c(1, 3, 10, 30, 100), model_mset=c(1000), model_nfeatures=c(2, 3, 5, 7)){
  log_file <- paste(c('log/', df_name, '.log'), collapse='')
  log_items <- read.table(log_file, header=FALSE, sep='|',
                          col.names=c('df_name', 'submodel_name', 'nsets', 'mset', 'nfeatures', 'train_time', 'test_time', 'accuracy', 'f1_score'))
  log_key_strings <- apply(log_items[,c('df_name', 'submodel_name', 'nsets', 'mset', 'nfeatures')], 1, function(x) gsub(" ", "", paste(x, collapse='_'), fixed = TRUE))

  train_size <- floor(.8 * nrow(df))
  train_i <- sample(seq_len(nrow(df)), size=train_size)

  train <- df[train_i, ]
  test <- df[-train_i, ]
  
  if(submodel_name == "bagging"){
    model_nfeatures <- c(0) # to avoid unnecessary loops when parameter is not used
  }
  
  for (nsets in model_nset) {
    for (mset in model_mset) {
      for (nfeatures in model_nfeatures) {
        key <- c(df_name, submodel_name, nsets, mset, nfeatures)
        key_string <- paste(key, collapse='_')

        if (key_string %in% log_key_strings) {
          next
        }

        train_time <- 0
        test_time <- 0
        accuracy <- 0
        accuracy_nan <- 0
        f1_score <- 0
        f1_score_nan <- 0
  
        xcolnames <- colnames(train)[colnames(train) != ycolname]
        formula <- as.formula(paste(ycolname, '~', paste(xcolnames, collapse='+')))
        for (i in 1:nrep) {
          print(paste('Training classifier ', key_string, ' (', i, '/', nrep, ')...', collapse=''))
          train_time <- train_time + system.time({
            classifier <- classifier_funct(train, nsets, mset, nfeatures, ycolname, xcolnames, formula, submodel_funct)
          })[['user.self']]
  
          print(paste('Testing classifier', key_string, ' (', i, '/', nrep, ')...', collapse=''))
          test_time <- test_time + system.time({
            predictions <- predict(classifier, test)
          })[['user.self']]
  
          accuracy_i <- Accuracy(test[[ycolname]], predictions)
          if (!is.nan(accuracy_i)) accuracy <- accuracy + accuracy_i
          else accuracy_nan <- accuracy_nan + 1

          f1_score_i <- F1_Score(test[[ycolname]], predictions)
          if (!is.nan(f1_score_i)) f1_score <- f1_score + f1_score_i
          else f1_score_nan <- f1_score_nan + 1
        }

        train_time <- train_time / nrep
        test_time <- test_time / nrep
        accuracy <- accuracy / (nrep - accuracy_nan)
        f1_score <- f1_score / (nrep - f1_score_nan)

        print(paste(c('Accuracy: ', accuracy, ', F1 score: ', f1_score), collapse=''))
        print('Saving...')
  
        write.table(
          # df_name submodel_name nsets mset nfeatures train_time test_time accuracy f1_score
          data.frame(
            df_name=df_name,
            submodel_name=submodel_name,
            nsets=nsets,
            mset=mset,
            nfeatures=nfeatures,
            train_time=train_time,
            test_time=test_time,
            accuracy=accuracy,
            f1_score=f1_score),
          file=log_file, append=TRUE, quote=FALSE, sep='|', col.names=FALSE, row.names=FALSE
        )
      }
    }
  }
}
