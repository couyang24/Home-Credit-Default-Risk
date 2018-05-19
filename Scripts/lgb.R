if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, skimr, GGally, plotly, viridis, 
               caret, DT, data.table, microbenchmark, xgboost,
               pROC, data.table)



train <-fread('../input/application_train.csv', stringsAsFactors = FALSE, showProgress=F,
              data.table = F, na.strings=c("NA","NaN","?", ""))
test <-fread('../input/application_test.csv', stringsAsFactors = FALSE, showProgress=F,
             data.table = F, na.strings=c("NA","NaN","?", ""))
full <- bind_rows(train,test)
# bureau <- fread('../input/bureau.csv', stringsAsFactors = FALSE, showProgress=F,
# data.table = F, na.strings=c("NA","NaN","?", ""))
# record <- fread('../input/previous_application.csv', stringsAsFactors = FALSE, showProgress=F,
# data.table = F, na.strings=c("NA","NaN","?", ""))


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

#set.seed(1)
inTrain <- createDataPartition(Target, p=.9, list = F)

tr <- train[inTrain,]
va <- train[-inTrain,]

tr_ta <- Target[inTrain]
va_ta <- Target[-inTrain]

data.train <- xgb.DMatrix(data = data.matrix(tr), label = tr_ta)
data.valid <- xgb.DMatrix(data = data.matrix(va), label = va_ta)
data.test <- xgb.DMatrix(data = data.matrix(test))


library(lightgbm, quietly=TRUE)
lgb.train = lgb.Dataset(data.matrix(tr), label = tr_ta)
lgb.valid = lgb.Dataset(data.matrix(va), label = va_ta)
lgb.test <- lgb.Dataset(data.matrix(test))

params.lgb = list(
  objective = "binary"
  , metric = "auc"
  , min_data_in_leaf = 1
  , min_sum_hessian_in_leaf = 100
  , feature_fraction = 1
  , bagging_fraction = 1
  , bagging_freq = 0
)

# Get the time to train the lightGBM model

lgb.model <- lgb.train(
  params = params.lgb
  , data = lgb.train
  , valids = list(val = lgb.valid)
  , learning_rate = 0.1
  , num_leaves = 7
  , num_threads = 2
  , nrounds = 3000
  , early_stopping_rounds = 200
  , eval_freq = 50
)

#???print(lgb.bench)
# print(max(unlist(lgb.model$record_evals[["test"]][["auc"]][["eval"]])))

# get feature importance
lgb.feature.imp = lgb.importance(lgb.model, percentage = TRUE)

# make test predictions
lgb.test = predict(lgb.model, data = data.matrix(test), n = lgb.model$best_iter)
