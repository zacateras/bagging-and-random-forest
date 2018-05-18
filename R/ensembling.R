#To get a package use: install.packages("package_name")
require(rpart)
#require(caret)
#require(e1071)
set.seed(1234)
function(dummy) {dummy}
Ensembling <- function(bootstrapSetSize, featureCount, bootstrapSetsCount, testSet, trainingSet){
  testSet$oneFrequency <- 0
  testSet$zeroFrequency <- 0
  for (i in 1:bootstrapSetsCount){
    sampleSize <- floor(bootstrapSetSize * nrow(trainingSet))
    bootstrapSet <- sample(1:nrow(trainingSet), size = sampleSize, replace = TRUE)
    randomArguments <- sample(1:ncol(trainingSet)-1,  size = featureCount)
    formula <- as.formula( c(paste(colnames(trainingSet[ncol(trainingSet)]),  "~",paste( colnames(trainingSet)[randomArguments] , collapse='+' )) ))
    tree <- rpart( formula, trainingSet )
    
    predictions <- predict(object=tree,  type = "class", newdata = testSet )
    testSet$oneFrequency[predictions == 1] <- testSet$oneFrequency[predictions == 1] + 1
    testSet$zeroFrequency[predictions == 0] <- testSet$zeroFrequency[predictions == 0] + 1
  }
  predictions<-ifelse(testSet$zeroFrequency > testSet$oneFrequency, 0, 1)
  return(predictions)
}
