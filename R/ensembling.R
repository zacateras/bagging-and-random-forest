#To get a package use: install.packages("package_name")
require(rpart)
#require(caret)
#require(e1071)
set.seed(1234)

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
  #or
  #table <- table(faktual,predicted)
  #confusionMatrix(t)
}

k=10
N=2
mydata=read.csv(".\\credit_card_clients.csv", header=TRUE, sep=";")

mydata$DEFAULT_PAY <- factor(mydata$DEFAULT_PAY, levels = c(1, 0) )
mydata$SEX <- factor(mydata$SEX, levels =c(1,2), labels = c("male","female") )
mydata$EDUCATION <- factor(mydata$EDUCATION, levels =c(1,2,3,4), labels = c("graduate school","university","high school","others") )
mydata$MARRIAGE <- factor(mydata$MARRIAGE, levels =c(1,2,3), labels = c("married","single ","others ") )
mydata[,6:11] <- lapply(mydata[,6:11], as.factor)
#To check a data structure use: str(mydata)

#align class distribution TODO: replace with probabiltity vector
neqativeRecords <- subset(mydata, DEFAULT_PAY==0)
positiveRecords <- subset(mydata, DEFAULT_PAY==1)
difference <- nrow(neqativeRecords) - nrow(positiveRecords)
recordsToDelete <- sample( 1:nrow(neqativeRecords), size = difference)
neqativeRecords <- neqativeRecords[-recordsToDelete, ]
mydata <- rbind(neqativeRecords, positiveRecords)
#end of code for replacement

accuracy=c()
fscore=c()

for (i in 1:N){
  sampleSize <- floor(0.75 * nrow(mydata))
  sampleIdx <- sample(1:nrow(mydata), size = sampleSize, replace = TRUE)
  trainingSet <- mydata[sampleIdx, ]
  testSet <- mydata[-sampleIdx, ]
  
  randomArguments <- sample(ncol(trainingSet)-1, k)
  formula <- as.formula( c("DEFAULT_PAY ~", paste( colnames(trainingSet)[randomArguments] , collapse='+' )))
  tree <- rpart( formula, trainingSet)

  #probability <- predict(tree, newdata = testSet)
  prediction <- predict(object=tree,  type = "class", newdata = testSet)
  
  accuracy[i] = Accuracy(testSet$DEFAULT_PAY, prediction)
  fscore[i] = F1Score(testSet$DEFAULT_PAY, prediction )
}
accuracy
fscore
