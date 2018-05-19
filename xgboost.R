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


parameters <- list(
  # General Parameters
  booster            = "gbtree"      
  , silent             = 0      
  # Booster Parameters
  , eta                = 0.02              
  , gamma              = 0.7                 
  , max_depth          = 8               
  , min_child_weight   = 2            
  , subsample          = .9                 
  , colsample_bytree   = .5                
  , colsample_bylevel  = 1          
  , lambda             = 1    
  , alpha              = 0       
  # Task Parameters
  , objective          = "binary:logistic"   # default = "reg:linear"
  , eval_metric        = "auc"
  , seed               = 1               # reproducability seed
  , tree_method = "hist"
  , grow_policy = "lossguide"
)

xgb_model <- xgb.train(parameters, data.train, nrounds = 3000, list(val = data.valid), print_every_n = 50, early_stopping_rounds = 200)

xgb.importance(colnames(train), model = xgb_model) %>% kable()
xgb.imp <- xgb.importance(colnames(train), model = xgb_model)
xgb.ggplot.importance(importance_matrix = xgb.imp)

xgb_pred <- predict(xgb_model, data.test)

result <- data.frame(SK_ID_CURR = Id, TARGET = xgb_pred)

write.csv(result,"xgb_pred.csv", row.names = F)