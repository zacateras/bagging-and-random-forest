predict.ensemble <- function(object, newdata, type='class') {
  if (!inherits(object, 'ensemble')) stop('Not a legitimate \"rpart\" object')

  subpreds <- list()
  for (i in 1:length(object$submodels)) {
    subpreds[[i]] <- data.frame(
      predict(object$submodels[[i]]$model, newdata, type=type))
  }

  res <- NULL

  if (type == 'class') {
    # convert class predictions to one-hot encoded results
    # then sum predictions from all submodels
    for (i in 1:length(subpreds)) {
      res_i <- onehot.encode(subpreds[[i]], levels=object$levels)

      if (is.null(res)) { res <- res_i }
      else { res <- res + res_i }
    }

    # find ensemble predictions by finding
    # column number with maximum total count
    # and looking it up in levels ordered list
    res <- apply(res, 1, which.max)
    res <- apply(data.frame(res), 1, function(x) object$levels[x])

    res <- factor(res, levels=object$levels)
    
    # TODO add more supported types
  } else stop('Unsupported type for \"ensemble\" object')

  return(res)
}
