setwd("E:/R語言與商業分析")
install.packages("tidyverse")
library(tidyverse)

#knitr package可以協助畫出更好看的圖表
install.packages("knitr")
library(knitr)

#使用read_csv，三張表格都會是tibble的形式
SalesTable<-read_csv("E:/R語言與商業分析/SalesTable.csv")
ClientTable<-read_csv("E:/R語言與商業分析/ClientTable.csv")
ProductTable<-read_csv("E:/R語言與商業分析/ProductTable.csv")

install.packages("magrittr")
library(magrittr)

#把三個表結合起來
SalesTableNew<-SalesTable %>%
  inner_join(ClientTable,by="Client_ID") %>%
  inner_join(ProductTable,by="Product_ID")

kable(SalesTableNew[1:10,])

SalesTableNew$Agency <- as.factor(SalesTableNew$Agency)
SalesTableNew$Product_ID <- as.factor(SalesTableNew$Product_ID)
SalesTableNew$Client_ID <- as.factor(SalesTableNew$Client_ID)


#計算商品單價
SalesTablePrice <- SalesTableNew %>%
  mutate( Unit_Price = Sales / Sales_Amount)
#畫張圖吧! 發現公司賣的都是低單價的東西
#theme_bw()表示我再背景做設計，讓背景是白色的
ggplot(data = SalesTablePrice,
       aes( x = Unit_Price,
            y = Sales_Amount))+
  geom_point(color = 'red',
             alpha = 0.5) + theme_bw()

#來看看不同客戶之間的銷售模式，使用boxplot來看個客戶的銷售分配
#LL這位客戶每一次的銷售金額都很高
ggplot(SalesTableNew)+geom_boxplot( aes( x = factor(Client_Name),
                                         y = Sales,
                                         colour = Client_Name))+
  labs( x = 'Client',
        title = 'Sales Distribution by Client') + theme_bw()

#來看看一整個月下來，哪個客戶貢獻最多，可以發現CC這個客戶貢獻極大
SalesTableSum <- SalesTableNew %>%
  group_by( Client_Name) %>%
  summarise( Sales_Sum = sum(Sales)) %>%
  arrange(desc(Sales_Sum))

ggplot( data = SalesTableSum,
        aes( x = Client_Name,
             y = Sales_Sum,
             fill = Client_Name)) + 
  geom_bar( stat = 'identity') +
  
  scale_x_discrete(limits = SalesTableSum$Client_Name) +
  
  labs(title = 'Total Sales by Client',
       x = 'Client',
       y = 'Sales in total',
       fill = 'Client_Name') + theme_bw()

#看看這一個月的每個客戶的單價分配，感覺沒什麼差別
ggplot( data = SalesTablePrice) + 
  geom_boxplot(aes( x = as.factor(Client_Name),
                    y = Unit_Price,
                    colour = Client_Name)) +
  
  labs(title = 'Unit_Price by Client',
       x = 'Client',
       y = 'Unit_Price in total',
       fill = 'Client_Name') + theme_bw()
