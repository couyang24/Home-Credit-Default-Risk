if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, skimr, GGally, plotly, viridis, caret, DT, data.table)



train <-fread('../input/application_train.csv', stringsAsFactors = FALSE, showProgress=F,
              data.table = F, na.strings=c("NA","NaN","?", ""))
test <-fread('../input/application_test.csv', stringsAsFactors = FALSE, showProgress=F,
             data.table = F, na.strings=c("NA","NaN","?", ""))




chr <- train[,sapply(train,is.character)]
num <- train[,sapply(train,is.numeric)]


set.seed(1)
inTrain <- createDataPartition(train$TARGET, p=.9, list = F)

train <- train[inTrain,]
valid <- train[-inTrain,]
