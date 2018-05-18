F1Score <- function(factual, predicted) {
  
  TP = sum(predicted %in% factual)
  
  if(TP == 0) {
    return(0)
  }
  
  precision <- TP/length(predicted)
  recall <- TP/length(factual)
  
  2 * precision * recall / (precision + recall)
}

Accuracy <- function(factual, predicted){
  length( predicted[ predicted== factual ]) / length( predicted )
}

MeasureQuality <- function(factual, predicted){
  accuracy <- Accuracy(factual, predicted)
  fscore  <- F1Score(factual, predicted )
  accuracy 
  fscore
}
