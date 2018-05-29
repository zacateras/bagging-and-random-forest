# install.packages('caret')
# install.packages('rpart')

library('caret')
library('rpart')

df = data.frame(read.csv('data/covtype.data', head=FALSE, sep=','))

# clean data and prepare for training
# consider cleaning with nearZeroVar

train_size <- floor(.99 * nrow(df))
train_i <- sample(seq_len(nrow(df)), size=train_size)

train <- df[train_i, ]
test <- df[-train_i, ]

source('R/ensemble.R')

rpart_fun <- function(formula, data) rpart(formula, data=data)
classifier_1 <- ensemble(rpart_fun, nsets=3, mset=100, nfeatures=5, ycolname='V55', data=test)
predict(classifier_1, test)