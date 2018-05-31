predict.ensemble <- function(object, newdata, type=c('class')) {
  if (!inherits(object, 'ensemble')) stop('Not a legitimate \"rpart\" object')

  subpreds <- list()
  for (i in 1:length(object$submodels)) {
    subpreds[[i]] <- data.frame(
      predict(object$submodels[[i]]$model, newdata, type=type))
  }

  if (type == 'class') {
    res <- NULL

    # convert class predictions to one-hot encoded results
    # then sum predictions from all submodels
    for (i in 1:length(subpreds)) {
      res_i <- predict(onehot(subpreds[[i]]), subpreds[[i]])

      if (is.null(res)) { res <- res_i }
      else { res <- res + res_i }
    }

    # find ensemble predictions by finding
    # column number with maximum total count
    res <- apply(res, 1, which.max)

    # TODO add more supported types
  } else stop('Unsupported type for \"ensemble\" object')

  return(res)
}
