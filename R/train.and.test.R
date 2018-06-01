train_and_test_ensemble <- function(df, df_name, submodel_fun, submodel_name, ycolname,
                                    bag_nsets=c(1, 3, 10, 30, 100), bag_mset=c(100, 1000, 10000, 100000), bag_nfeatures=c(2, 3, 5, 7)) {

  log_file <- paste(c('log/', df_name, '.log'), collapse='')
  log_items <- read.table(log_file, header=FALSE, sep='|',
                          col.names=c('df_name', 'submodel_name', 'nsets', 'mset', 'nfeatures', 'train_time', 'test_time', 'accuracy', 'f1_score'))
  log_key_strings <- apply(log_items[,c('df_name', 'submodel_name', 'nsets', 'mset', 'nfeatures')], 1, function(x) gsub(" ", "", paste(x, collapse='_'), fixed = TRUE))

  train_size <- floor(.8 * nrow(df))
  train_i <- sample(seq_len(nrow(df)), size=train_size)

  train <- df[train_i, ]
  test <- df[-train_i, ]
  
  for (nsets in bag_nsets) {
    for (mset in bag_mset) {
      for (nfeatures in bag_nfeatures) {
        key <- c(df_name, submodel_name, nsets, mset, nfeatures)
        key_string <- paste(key, collapse='_')

        if (key_string %in% log_key_strings) {
          next
        }
  
        print(paste('Training classifier', key_string, '...'))
        train_time <- system.time({
          classifier <- ensemble(
            submodel_fun,
            nsets=nsets,
            mset=mset,
            nfeatures=nfeatures,
            ycolname="Cover_Type",
            data=train)
        })
  
        print(paste('Testing classifier', key_string, '...'))
        test_time <- system.time({
          predictions <- predict(classifier, test)
        })
  
        accuracy <- Accuracy(test$Cover_Type, predictions)
        f1_score <- F1_Score(test$Cover_Type, predictions)

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
            train_time=train_time[['user.self']],
            test_time=test_time[['user.self']],
            accuracy=accuracy,
            f1_score=f1_score),
          file=log_file, append=TRUE, quote=FALSE, sep='|', col.names=FALSE, row.names=FALSE)
      }
    }
  }
}
