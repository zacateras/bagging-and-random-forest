set.seed(1234)

ensemble <- function(model, nsets, mset, nfeatures, ycolname, data) {
  fit <- NULL

  for (i in nsets) {
    # random sample
    smpl_size <- floor(mset * nrow(data))
    smpl_i <- sample(seq_len(nrow(data)), size=smpl_size, replace=TRUE)
    smpl <- data[smpl_i, ]

    # random features
    xcolnames <- colnames(data)
    xcolnames <- xcolnames[xcolnames != ycolname]
    xcolnames <- xcolnames[sample(seq_len(length(xcolnames)), size=nfeatures)]
    formula <- as.formula(paste(ycolname, '~', paste(xcolnames, collapse="+")))

    fit <- c(fit, model(formula, smpl))
  }

  return(fit)
}

ensemble.predict <- function(object, newdata) {
  # TODO
}
