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

#不同產品的銷售模式，用boxplot看個產品銷售的分配
#H的單價是最高的，可以它的變異狀況也很高
ggplot( data = SalesTableNew) + 
  geom_boxplot(aes( x = Product_Name,
                    y = Sales,
                    colour = Product_Name)) +
  labs( x = 'Product',
        title = 'Sales Distribution by Product') + theme_bw()

#過去一個用中這些產品的總銷售量狀況
#銷售總量再H商品上是最高的
SalesTableAmount <- SalesTableNew %>%
  group_by( Product_Name) %>%
  summarise( Amount_Sum = sum(Sales_Amount)) %>%
  arrange(desc(Amount_Sum))

ggplot( data = SalesTableAmount) + 
  geom_bar( aes( x = Product_Name,
                 y = Amount_Sum,
                 fill = Product_Name),
            stat = 'identity') +
  
  scale_x_discrete(limits = SalesTableAmount$Client_Name) +
  
  labs(title = 'Total Sales_Amount by Product',
       x = 'Product',
       y = 'Sales_Amount in total',
       fill = 'Product_Name') + theme_bw()

#合併在一起起來看，知道每個顧客都主要購買什麼產品
#facet_wrap是有幾個客戶就做幾張圖
SalesTableClient <- SalesTableNew %>%
  group_by(Client_Name, Product_Name) %>%
  summarise( Sales = sum(Sales))

ggplot( data = SalesTableClient) +
  geom_bar( aes( x = Product_Name,
                 y = Sales),
            stat = 'identity') +
  facet_wrap( ~ Client_Name)

#各經銷商的銷售模型
SalesTableAgency <- SalesTableNew %>%
  group_by(Agency, Product_Name) %>%
  summarise( Sales = sum(Sales))

ggplot( data = SalesTableAgency) +
  geom_bar( aes( x = Product_Name,
                 y = Sales),
            stat = 'identity') +
  facet_wrap( ~ Agency)

#開始製作多為資料視覺化
#客戶間的產品銷售比，堆疊長條圖，可以發現有點混亂，因為factor有太多level
Product <- SalesTableNew %>%
  group_by(Client_Name, Product_Name) %>%
  summarise(Sales = sum(Sales)) %>%
  mutate( Propor = round(Sales / sum(Sales),1) * 100)


ggplot( data = Product) + 
  geom_bar( aes( x = Client_Name, 
                 y = Sales,
                 fill = Product_Name, label = paste(Propor,'%', sep='')),stat = 'identity', alpha = 0.8) + 
  geom_text( aes( x = Client_Name, 
                  y = Sales,
                  fill = Product_Name, label = paste(Propor,'%', sep='')),position = position_stack( vjust = 0.5), size = 2) + theme_bw()

#重新整理圖表
Product <- SalesTableNew %>%
  group_by(Client_Name, Product_Name) %>%
  summarise(Sales = sum(Sales))
#將長的資料攤平! 變得比較寬的table
ClientProductTable <- Product %>%
  spread( key = Product_Name, 
          value = Sales) %>%
  data.frame()

#block函數把寬的table吃進去
Block <- function(ClientProductTable){
  ClientProductTable$x_Percentage <- c()

#各個欄位的加總去除以整個table中有sale值的加總
for (i in 1:nrow(ClientProductTable)) {
  ClientProductTable$x_percentage[i] <- rowSums(ClientProductTable[i,-1], na.rm = T) / sum(rowSums(ClientProductTable[,-1], na.rm = T))
}

#cumsum為累計加總，計算上界下界
ClientProductTable$x_max <- cumsum(ClientProductTable$x_percentage)
ClientProductTable$x_min <- ClientProductTable$x_max - ClientProductTable$x_percentage
ClientProductTable$x_percentage <- NULL

#再把寬的table轉回去長的table
Percentage <- ClientProductTable %>%
  gather( key =  Product_Name,
          value = Sales,
          -c(Client_Name, x_min,x_max))

Percentage[,5] <- ifelse(Percentage[,5] %in% NA, 0, Percentage[,5])
colnames(Percentage)[5] <- 'Sales'

#開始製作Y值的比例
Percentage <- Percentage %>%
  group_by( Client_Name) %>%
  mutate( y_max = round(cumsum(Sales) / sum(Sales) * 100)) %>%
  mutate( y_min = round((y_max - Sales/ sum(Sales) * 100)))

#文字的位子
Percentage <- Percentage %>%
  mutate( x_text = x_min + (x_max - x_min)/2, 
          y_text = y_min + (y_max - y_min)/2)

Percentage <- Percentage %>%
  group_by( Client_Name) %>%
  mutate( Proportion = round( Sales / sum(Sales),2) * 100)

#開始做圖
ggplot(Percentage, aes(ymin = y_min, ymax = y_max,
                       xmin = x_min, xmax = x_max, fill = Product_Name)) +
  geom_rect(colour = I("grey"), alpha = 0.9) + 
  
  geom_text( aes(x = x_text, y = y_text,
                 label = ifelse( Client_Name %in% levels(factor(Client_Name))[1] & Proportion != 0, 
                                 paste(Product_Name," - ", Proportion, "%", sep = ""),
                                 ifelse(Proportion != 0, paste( Proportion,"%", sep = ""), paste(NULL)))), size = 2.5) + 
  geom_text(aes(x = x_text, y = 103,
                label = paste(Client_Name)), size = 3) + 
  labs( title = 'Sales Distribution by Client & Product',
        x = 'Client',
        y = 'Product') + theme_bw()
}

Block(ClientProductTable)


#資料取捨時間，找出自己想要討論的資料就好
ClientMiddle <- Product %>%
  filter( Client_Name %in% 'BB' | Client_Name %in% 'DD' | Client_Name %in% 'HH')

ClientProductTable <- ClientMiddle %>%
  spread( key = Product_Name, 
          value = Sales) %>%
  data.frame()

Block(ClientProductTable)

#價格、銷售、銷量、毛利個個資料一起來!
MarginTable <- read_csv('SalesTable_WithCost.csv')
MarginTable$Product_ID <- MarginTable$Product_ID %>% as.factor()
MarginTable$Margin_Rate <- MarginTable$Margin_Rate %>% round(3)

SalesTableMargin <- SalesTableNew %>%
  inner_join(MarginTable, by = 'Product_ID')

ProductSalesTable <- SalesTableMargin %>%
  group_by(Product_Name) %>%
  summarise( Sales = sum(Sales),
             Sales_Amount = sum(Sales_Amount),
             Margin_Rate = mean(Margin_Rate)) %>%
  mutate( Price = Sales/Sales_Amount,
          Margin_Group = ifelse( Margin_Rate > 0.7, 'Top',
                                 ifelse( Margin_Rate >= 0.5 & Margin_Rate < 0.7, 'Normal', 'Bad'))) %>%
  arrange(desc(Sales))

#畫圖時間!!!
ggplot( data = ProductSalesTable,
        aes( x = Sales_Amount,
             y = Price,
             colour = Margin_Group)) + 
  geom_point(alpha = 0.9) +
  geom_point( aes(size = Sales))+
  geom_text( aes( label = Product_Name), vjust = -3, size = 2, colour = 'black') + 
  geom_vline( aes( xintercept = mean(Sales_Amount))) + 
  geom_hline( aes( yintercept = mean(Price))) + 
  
  labs( title = 'Price, Sales_Amount, Sales and Margin') + 
  
  theme_bw()