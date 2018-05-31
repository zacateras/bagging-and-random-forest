source(".\\R\\ensembling.R")
source(".\\R\\qualityMeasure.R")
mydata=read.csv(".\\datasets\\credit_card_clients.csv", header=TRUE, sep=";")

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
sampleSize <- floor(0.75 * nrow(mydata))
sampleIdx <- sample(1:nrow(mydata), size = sampleSize, replace = TRUE)
train <- mydata[sampleIdx, ]
test <- mydata[-sampleIdx, ]

predictions <- Ensembling(bootstrapSetSize=0.9, featureCount=floor(sqrt(ncol(mydata))), bootstrapSetsCount=10,  testSet=test, trainingSet=train)


MeasureQuality(factual=test$DEFAULT_PAY, predicted=predictions)

