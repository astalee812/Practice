setwd("E:/R語言與商業分析")
install.packages("tidyverse")
library(tidyverse)
test.table<-read_csv("test_table.csv")
user.table<-read_csv("user_table.csv")

head(test.table)
head(user.table)

#test_table中不重複的user_id=19871
length(unique(test.table$user_id))
#user_table中不重複的user_id=20000
length(unique(user.table$user_id))
#重複出現在user_table與test_table中不重複使用者數量=19871
ss<-inner_join(select(test.table,user_id),select(user.table,user_id),by="user_id")
length(unique(ss$user_id))

#了解實驗組數量(50012)與對照組數量(49988)
sum(test.table$test==1)
sum(test.table$test==0)

#合併資料:test資料全部都要有
test.data<-left_join(test.table,user.table,by="user_id")
head(test.data)

#變數轉換:日期格式調整
test.data$date<-as.Date(test.data$date,format = "%Y/%m/%d")
test.data$device<-as.factor(test.data$device)
test.data$test<-as.factor(test.data$test)
test.data$gender<-as.factor(test.data$gender)
test.data$service<-as.factor(test.data$service)
test.data$country<-as.factor(test.data$country)
head(test.data)
summary(test.data)

#實驗組與對照組的差異，計算購物次數的平均值
test.data%>%
  group_by(test) %>%
  summarise(mean_purchase_amount=mean(purchase_amount))

#設備之間的差異
test.data %>%
  group_by(device) %>%
  summarise(mean_purchase_amount=mean(purchase_amount))

#性別之間的差異
test.data %>%
  group_by(gender) %>%
  summarise(mean_parchase_amount=mean(purchase_amount))

#服務類型的差異


