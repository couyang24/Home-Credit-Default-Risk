library(tidyverse)
library(data.table)
library(skimr)
library(DT)
library(plotly)
library(mice)
library(e1071)


train <-fread('application_train.csv', stringsAsFactors = FALSE, showProgress=F,
              data.table = F, na.strings=c("NA","NaN","?", ""))
test <-fread('application_test.csv', stringsAsFactors = FALSE, showProgress=F,
              data.table = F, na.strings=c("NA","NaN","?", ""))
full <- bind_rows(train,test)


Target <- train$TARGET
Id <- test$SK_ID_CURR

full[,c('SK_ID_CURR','TARGET')] <- NULL
rm(train, test)

chr <- full[,sapply(full,is.character)]
int <- full[,sapply(full,is.numeric)]

fill_chr <- function(df){
  for(i in 1:ncol(df)){
    for(j in 1:nrow(df)){
      if(is.na(df[j,i])){
        df[j,i] = "Not Avaiable"
      }
    } 
  } 
  return(df)
}
chr <- fill_chr(chr)
fac <- chr %>% lapply(as.factor) %>% as_data_frame()

full <- bind_cols(fac,int)

micemod <- full %>% mice(method='rf')
full <- complete(micemod)

rm(chr,fac,int,fill_chr,micemod)

train <- full[1:length(Target),]
test<-full[(length(Target)+1):nrow(full),]

svm_model<-svm(Target~., data=train, cost = 3)
svm_pred <- predict(svm_model,newdata = test)
solution <- data.frame(SK_ID_CURR=Id,TARGET=svm_pred)
write.csv(solution,"svm_benchmark_solution.csv",row.names = F)
