onehot.encode <- function(vec, levels) {
  # convert items to level indexes
  vec_idx <- data.frame(apply(vec, 1, function(x) match(x, levels)))

  # prepare ID matrix of length(levels) used for one-hot translations
  id_df <- data.frame(diag(length(levels)))

  # translate indexes to one-hot vectors
  ans <- data.frame(t(apply(vec_idx, 1, function(x) id_df[[x]])))

  colnames(ans) <- levels

  return(ans)
}
