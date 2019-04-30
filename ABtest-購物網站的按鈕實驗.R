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
test.data %>%
  group_by(service) %>%
  summarise(mean_parchase_amount=mean(purchase_amount))
#國家的差異
test.data %>%
  group_by(country) %>%
  summarise(mean_purchase_amount=mean(purchase_amount))


#檢驗實驗組與對照組是否有不同的結果=使用獨立樣本t檢定
t.test(test.data[test.data$test==1,]$purchase_amount,
       test.data[test.data$test==0,]$purchase_amount,
       alternative = "greater")

#影響購物金額的原因=使用單因子變異數分析(ANOVA)
#目前有的因子:test,country,device,gender,service
aov.model<-aov(
  purchase_amount~test+country+device+gender+service,
  test.data
)
summary(aov.model)

#交互作用影響:test跟國家之間有交互作用
interaction.model<-aov(
  purchase_amount~test*country+test*device+test*service,
  test.data)
summary(interaction.model)

#事後檢定=Tukey事後檢定
#用來了解平均購買金額差異為多少
TukeyHSD(interaction.model, "test")
TukeyHSD(interaction.model, "country")
plot(TukeyHSD(interaction.model, "country"))

#結果呈現，開始將結果做視覺化
#每日銷售差異
daily.purchase <- test.data %>%
  group_by(date, test) %>%
  summarise(purchase_amount = mean(purchase_amount))

ggplot(daily.purchase, aes(x = date, y = purchase_amount, colour = test)) + 
  geom_point() + geom_line() +
  xlab("Date") + ylab("Purchase Amount") + ylim(c(30, 50)) +
  ggtitle("Time Series Plot of Purchase Amount: Test versus Control") +
  theme_bw()

#分兩族群來看差異
ggplot(test.data, aes(purchase_amount, fill = test, colour = test)) +
  geom_density(alpha = 0.3) +
  xlab("Purchase Amount") + ylab("Density") +
  ggtitle("Density Plot of Purchase Amount: Test versus Control") +
  theme_bw()

#哪些因素會影響使用者的消費金額
ggplot(test.data, aes(x = country, y = purchase_amount)) +
  geom_boxplot() +
  xlab("Country") + ylab("Purchase Amount") +
  ggtitle("Boxplot of Purchase Amount by Country") +
  theme_bw()

#哪些因素存在交互作用
ggplot(test.data, aes(x = country, y = purchase_amount, colour = test)) +
  geom_boxplot() +
  xlab("Country") + ylab("Purchase Amount") +
  ggtitle("Boxplot of Purchase Amount by Country: Test versus Control") +
  theme_bw()