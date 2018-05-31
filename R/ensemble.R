ensemble <- function(model, nsets, mset, nfeatures, ycolname, data) {
  
  ans <- list(submodels=list())
  class(ans) <- 'ensemble'

  for (i in 1:nsets) {
    # random sample
    smpl_size <- floor(mset) # mset < nrow(data)
    smpl_i <- sample(seq_len(nrow(data)), size=smpl_size, replace=TRUE)
    smpl <- data[smpl_i, ]

    # random features
    xcolnames <- colnames(data)
    xcolnames <- xcolnames[xcolnames != ycolname]
    xcolnames <- xcolnames[sample(seq_len(length(xcolnames)), size=nfeatures)]
    formula <- as.formula(paste(ycolname, '~', paste(xcolnames, collapse='+')))

    ans$submodels[[i]] <- list(
      formula=formula,
      model=model(formula, smpl)
    )
  }

  return(ans)
}
