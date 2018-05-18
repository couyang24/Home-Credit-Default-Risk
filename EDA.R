library(tidyverse)
library(data.table)
library(skimr)
library(DT)
library(plotly)
library(corrplot)
library(corrgram)

# <img src="https://i.imgur.com/O6QrzdG.jpg">

train <-fread('application_train.csv', stringsAsFactors = FALSE, showProgress=F,
              data.table = F, na.strings=c("NA","NaN","?", ""))
test <-fread('application_test.csv', stringsAsFactors = FALSE, showProgress=F,
             data.table = F, na.strings=c("NA","NaN","?", ""))


setdiff(names(train),names(test))
rm(test)

train %>% count(TARGET) %>% kable()

train %>% 
  count(TARGET) %>% 
  plot_ly(labels = ~TARGET, values = ~n, type = 'pie') %>%
  layout(title = 'Target Variable Distribution',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



train %>% skim() %>% kable()

train[sample(1:nrow(train),size = 1000),] %>% 
  datatable(filter = 'top', options = list(
    pageLength = 15, autoWidth = TRUE
  ))

chr <- train[,sapply(train,is.character)]
num <- train[,sapply(train,is.numeric)]

# num_acc <- num %>% 
#   sapply(function(x) length(unique(x))) %>% 
#   data.frame(count=.) %>% 
#   rownames_to_column() %>% 
#   filter(count>100) %>% 
#   select(rowname)
# 

num %>%   
  na.omit() %>% 
  cor() %>% 
  gather(key = 'compared variable', value = "Correlation", -TARGET)



num %>%
  select('AMT_INCOME_TOTAL', 'AMT_CREDIT', 'AMT_ANNUITY' ,
         'AMT_GOODS_PRICE', 'DAYS_BIRTH', 'DAYS_EMPLOYED',
         'DAYS_REGISTRATION', 'DAYS_ID_PUBLISH', 'DAYS_LAST_PHONE_CHANGE', 
         'HOUR_APPR_PROCESS_START', 'TARGET') %>% 
  na.omit() %>% 
  cor() %>% 
  corrplot.mixed(upper = "ellipse", tl.cex=.8, tl.pos = 'lt', number.cex = .8)


