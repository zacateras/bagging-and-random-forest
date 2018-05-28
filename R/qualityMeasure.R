F1Score <- function(y_true, y_pred) {
  
  TP = sum(y_pred %in% y_true)
  
  if(TP == 0) {
    return(0)
  }
  
  precision <- TP/length(y_pred)
  recall <- TP/length(y_true)
  
  2 * precision * recall / (precision + recall)
}

Accuracy <- function(y_true, y_pred) {
  length(y_pred[y_pred==y_true]) / length(y_pred)
}

MeasureQuality <- function(y_true, y_pred) {
  accuracy <- Accuracy(y_true, y_pred)
  fscore  <- F1Score(y_true, y_pred)
  accuracy 
  fscore
}
