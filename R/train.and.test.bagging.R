train_and_test_bagging <- function(df, df_name, ycolname, nrep=10, model_ntree=c(1, 200, 500, 700), model_mset=c(100, 1000, 10000)) {
  library('ipred')
  log_file <- paste(c('log/', df_name, '.log'), collapse='')
  log_items <- read.table(log_file, header=FALSE, sep='|',
                          col.names=c('df_name', 'submodel_name', 'nsets', 'mset', 'nfeatures', 'train_time', 'test_time', 'accuracy', 'f1_score'))
  log_key_strings <- apply(log_items[,c('df_name', 'submodel_name', 'nsets', 'mset', 'nfeatures')], 1, function(x) gsub(" ", "", paste(x, collapse='_'), fixed = TRUE))
  
  train_size <- floor(.8 * nrow(df))
  train_i <- sample(seq_len(nrow(df)), size=train_size)
  
  train <- df[train_i, ]
  test <- df[-train_i, ]
  
  for (ntree in model_ntree) {
    for (mset in model_mset) {
        submodel_name <- 'bagging'
        key <- c(df_name, submodel_name, ntree, mset, 0)
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
        m_formula <- as.formula(paste(ycolname, '~', paste(xcolnames, collapse='+')))
        for (i in 1:nrep) {
          print(paste('Training classifier ', key_string, ' (', i, '/', nrep, ')...', collapse=''))
          train_time <- train_time + system.time({
            classifier <- bagging( data=train, 
                                    formula=m_formula, 
                                    nbagg=ntree,
                                    ns = mset
            )
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
                nsets=ntree,
                mset=mset,
                nfeatures=0,
                train_time=train_time,
                test_time=test_time,
                accuracy=accuracy,
                f1_score=f1_score),
              file=log_file, append=TRUE, quote=FALSE, sep='|', col.names=FALSE, row.names=FALSE)
    }
  }
}
