library(tidyverse)
library(data.table)
library(skimr)
library(DT)
library(plotly)
library(corrplot)
library(corrgram)
library(caret)
library(gridExtra)
library(GGally)
library(viridis)


# <img src="https://i.imgur.com/O6QrzdG.jpg">

train <-fread('application_train.csv', stringsAsFactors = FALSE, showProgress=F,
              data.table = F, na.strings=c("NA","NaN","?", ""))
test <-fread('application_test.csv', stringsAsFactors = FALSE, showProgress=F,
             data.table = F, na.strings=c("NA","NaN","?", ""))


setdiff(names(train),names(test))
rm(test)

train %>% count(TARGET) %>% kable()


train %>% ncol()


train %>% 
  count(TARGET) %>% 
  plot_ly(labels = ~TARGET, values = ~n, type = 'pie') %>%
  layout(title = 'Target Variable Distribution',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


train %>% 
  count(NAME_EDUCATION_TYPE) %>% 
  plot_ly(labels = ~NAME_EDUCATION_TYPE , values = ~n) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Education Distribution",  showlegend = F,
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


# num %>%   
#   na.omit() %>% 
#   cor() %>% 
#   findCorrelation()
# 
# 
# num_matrix <- num %>%
#   na.omit() %>% 
#   cor() 
# 
# num_acc <- num_matrix[,2] %>% 
#   data.frame(cor=.) %>% 
#   rownames_to_column() %>% 
#   arrange(desc(cor)) %>% 
#   head(15) %>% 
#   select(rowname)
# 
# 
# num %>%
#   select(num_acc$rowname, -OWN_CAR_AGE) %>% 
#   corrplot.mixed(upper = "ellipse", tl.cex=.8, tl.pos = 'lt', number.cex = .8)

# corr_graph <- list()
# 
# for (i in 1:9){
# 
# corr_graph[[i]] <- num %>% na.omit() %>% 
#   select(TARGET,(i*10):(i*10+10)) %>% cor() %>% 
#   corrplot.mixed(upper = "ellipse", tl.cex=.8, tl.pos = 'lt', number.cex = .8)
#   
# }


graph <- list()

for (i in 1:10){
  
graph[[i]] <- num %>% na.omit() %>% 
    select(TARGET,((i-1)*10+1):((i-1)*10+10)) %>% 
    mutate(TARGET = factor(TARGET)) %>% 
    ggpairs(aes(col = TARGET, alpha=.4))
  
print(graph[[i]])
}


# corrgram(lower.panel=panel.shade, upper.panel=panel.ellipse)

chr %>% skim()

chr %>% na.omit() %>% 
  select(TARGET,1:5) %>% 
  mutate(TARGET = factor(TARGET)) %>% 
  ggpairs(aes(col = TARGET, alpha=.4))


chr %>% filter(NAME_CONTRACT_TYPE=='Revolving loans') %>% select(TARGET) %>% table()
chr %>% filter(NAME_CONTRACT_TYPE=='Cash loans') %>% select(TARGET) %>% table()

num %>%
  select('AMT_INCOME_TOTAL', 'AMT_CREDIT', 'AMT_ANNUITY' ,
         'AMT_GOODS_PRICE', 'DAYS_BIRTH', 'DAYS_EMPLOYED',
         'DAYS_REGISTRATION', 'DAYS_ID_PUBLISH', 'DAYS_LAST_PHONE_CHANGE', 
         'HOUR_APPR_PROCESS_START', 'TARGET') %>% 
  na.omit() %>% 
  corrgram(lower.panel=panel.shade, upper.panel=panel.ellipse)
