if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, skimr, GGally, plotly, viridis, 
               caret, DT, data.table, microbenchmark, xgboost,
               pROC, data.table)



train <-fread('application_train.csv', stringsAsFactors = FALSE, showProgress=F,
              data.table = F, na.strings=c("NA","NaN","?", ""))
test <-fread('application_test.csv', stringsAsFactors = FALSE, showProgress=F,
             data.table = F, na.strings=c("NA","NaN","?", ""))
full <- bind_rows(train,test)



Target <- train$TARGET
Id <- test$SK_ID_CURR
full[,c('SK_ID_CURR','TARGET')] <- NULL
rm(train, test)



chr <- full[,sapply(full, is.character)]
num <- full[,sapply(full, is.numeric)]

chr[is.na(chr)] <- "Not Available"

fac <- chr %>% 
  lapply(as.factor) %>% 
  as_data_frame()


full <- bind_cols(fac, num)
rm(chr, fac, num)

full[is.na(full)] <- 0

train <- full[1:length(Target),]
test <- full[(length(Target)+1):nrow(full),]

set.seed(1)
inTrain <- createDataPartition(train$TARGET, p=.9, list = F)

train <- train[inTrain,]
valid <- train[-inTrain,]


