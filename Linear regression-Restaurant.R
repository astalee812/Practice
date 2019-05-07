library(tidyverse)
library(knitr)

#EDA，region=A為觀光區，region=B為商業區，type=AA為百貨門市，type=BB為獨立門市
SalesData <- read.csv('Restaurant_Sales.csv')
head(SalesData)

SalesData$Store_Name<-as.factor(SalesData$Store_Name)

ggplot(data = SalesData)+
  geom_boxplot(aes(x=Store_Name,y=Sales,colour=Store_Name))+
  labs(x="store",y="sales",title = "sales distribution by store")

ggplot(data = SalesData)+
  geom_boxplot(aes(x=Region,y=Sales,colour=Region))+
  labs(x="Region",y="sales",title = "sales distribution by Region")

ggplot(data = SalesData)+
  geom_boxplot(aes(x=Type,y=Sales,colour=Type))+
  labs(x="Type",y="sales",title = "sales distribution by Type")


#觀看每家分店的銷售狀況，facet_wrap是可以將資料變成多個圖型
ggplot(data = SalesData)+
  geom_boxplot(aes(x=Weekday,y=Sales,colour=Weekday))+
  facet_wrap(~Store_Name)+
  labs(x="weekday",y="sales",title = "sales distribution by weekday")

SalesData$Month <- factor(SalesData$Month, levels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))

SalesDatabyMonth <- SalesData %>% 
  group_by( Store_Name,Month) %>%
  summarise( SalesMean = mean(Sales))

ggplot(data = SalesDatabyMonth,aes( x = Month, y = SalesMean)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~ Store_Name) +
  labs( x = 'Month',y = 'Sales',title = 'Sales Distribution by Month') 


#開始做迴歸分析，我們要先把區域分開，因為區域之間差異蠻大的，我們先抽掉這個變項，探討其他的
DataA <- SalesData %>%
  filter( Region %in% 'A')
DataB <- SalesData %>%
  filter( Region %in% 'B')

#驗證假說一: 門市類型的差異
Model.A.Type <- lm( Sales ~ Type, data = DataA)
Model.B.Type <- lm( Sales ~ Type, data = DataB)
summary(Model.A.Type)
summary(Model.B.Type)

#驗證假說二: 假日的差異
Model.A.Week <- lm( Sales ~ Weekday, data = DataA)
Model.B.Week <- lm( Sales ~ Weekday, data = DataB)
summary(Model.A.Week)
summary(Model.B.Week)

#驗證假說三:月份的差異
Model.A.Month <- lm( Sales ~ Month, data = DataA)
Model.B.Month <- lm( Sales ~ Month, data = DataB)
summary(Model.A.Month)
summary(Model.B.Month)

#結合上述顯著變數並檢視目前模型的解釋能力
Model.A <- lm( Sales ~ Type + Weekday + Month, data = DataA)
Model.B <- lm( Sales ~ Type + Month, data = DataB)
summary(Model.A)
summary(Model.B)

#用視覺化檢視模型目前的結果，發現目前模型並沒辦法掌握道其他極端的資料
#店1營收
ggplot( data = DataA[1:365,]) + 
  geom_point( aes(x = c(1:365),y = Sales)) + 
  geom_line( aes( x = c(1:365),y = Model.A$fitted.values[1:365],colour = 'red')) +
  labs( x = 'Day1 to Day365',y = 'Sales',title = 'Store1 Sales: Actual vs Predicted')

ggplot( data = DataB[1:365,]) + 
  geom_point( aes(x = c(1:365),y = Sales)) + 
  geom_line( aes( x = c(1:365),y = Model.B$fitted.values[1:365],colour = 'red'))  +
  labs( x = 'Day1 to Day365',y = 'Sales',title = 'Store2 Sales: Actual vs Predicted')

#加入幾項新的變數看看是不是有關聯並有更好的解釋力
SalesDataRenew <- read.csv('Restaurant_Sales_Renew.csv')
head(SalesDataRenew)
Model.Holiday <- lm( Sales ~ Holiday, data = SalesDataRenew)
Event <- ifelse( SalesDataRenew$Store1_Event %in% 1 | SalesDataRenew$Store2_Event %in% 1, 1, 0)
Model.Event <- lm( SalesDataRenew$Sales ~ Event)
summary(Model.Holiday)
summary(Model.Event)

#整合模型，將全部的資烙做建模
Model.All <- lm( Sales ~ ., data = SalesDataRenew[,-1])
summary(Model.All)


#再次用視覺化來檢查模型的成果
#店1的營收
ggplot( data = SalesDataRenew[1:365,]) + 
  geom_point( aes(x = c(1:365),y = Sales)) + 
  geom_line( aes( x = c(1:365),y = Model.All$fitted.values[1:365],colour = 'red')) +
  labs( x = 'Day1 to Day365',y = 'Sales',title = 'Store1 Sales: Actual vs Predicted')

#店2的營收，這邊模型會有高估的狀況
ggplot( data = SalesDataRenew[366:730,]) + 
  geom_point( aes(x = c(1:365),y = Sales)) + 
  geom_line( aes( x = c(1:365),y = Model.All$fitted.values[366:730],colour = 'red')) +
  labs( x = 'Day1 to Day365',y = 'Sales',title = 'Store2 Sales: Actual vs Predicted')

#四家店營收
ggplot( data = SalesDataRenew) + 
  geom_point( aes(x = c(1:1460),y = Sales)) + 
  geom_line( aes( x = c(1:1460),y = Model.All$fitted.values,colour = 'red')) +
  labs( x = 'Day',y = 'Sales',title = 'Store Sales: Actual vs Predicted')

#因為模型有高估狀況，把beta ceofficient拉出來探討
#Beta coefficient: 把coefficent拉出來
#intercept有強烈的影響，而regionB跟TypeBB會有負面影響，如果將所有資料混在一起做預測，會很混亂
#所以分區域去預測會比較準確
Beta <- cbind(names(Model.All$coefficients),Model.All$coefficients) %>% 
  as.data.frame() 

colnames(Beta) <- c('Name', 'Value')
Beta$Value <- Beta$Value %>% as.character() %>% as.numeric()

Beta <- Beta %>%arrange(desc(Value))

ggplot( data = Beta) + 
  geom_bar(aes( x = factor(Name, levels = as.character(Beta$Name)), 
                y = Value, 
                fill = Name),stat = 'identity') + 
  labs( x = 'Variable',
        y = 'Beta Coefficient',
        title = 'Beta Coefficient of Model.All') + coord_flip() + theme_bw()


DataRenewA <- SalesDataRenew %>% filter( Region %in% 'A')
DataRenewB <- SalesDataRenew %>% filter( Region %in% 'B')

# "."表示除了sales的所有變項都當作解釋變數，再加上門市類型跟星期的交互作用
Model.A.All <- lm( Sales ~ .+ Type * Weekday, data = DataRenewA[,-c(1,2)])
Model.B.All <- lm( Sales ~ .+ Type * Weekday , data = DataRenewB[,-c(1,2)])
summary(Model.A.All)
summary(Model.B.All)

#regionA
ggplot( data = DataRenewA) + 
  geom_point( aes(x = c(1:730),y = Sales)) + 
  geom_line( aes( x = c(1:730),y = Model.A.All$fitted.values,colour = 'red'))

#regionB
ggplot( data = DataRenewB) + 
  geom_point( aes(x = c(1:730),y = Sales)) + 
  geom_line( aes( x = c(1:730),y = Model.B.All$fitted.values,colour = 'red'))


#誤差：以RMSE衡量(Root-Mean-Square Error)
RMSE <- function( predict, actual){
  result <- sqrt(mean((predict - actual) ^ 2))
  return(result)
}
cat('RegionA模型的RMSE：\n',RMSE(Model.A.All$fitted.values, DataRenewA$Sales),'\n',sep = '')
cat('RegionB模型的RMSE：\n',RMSE(Model.B.All$fitted.values, DataRenewB$Sales),'\n',sep ='')



January <- tibble( 'Day' = c(1:31),
                   'Sales_Upperbound' = Model.A.All$fitted.values[1:31] * 1.08,
                   'Sales_Lowerbound' = Model.A.All$fitted.values[1:31] * 0.92)

ggplot( data = January)  +
  geom_segment(aes(x=Day, xend=Day, 
                   y=Sales_Upperbound, yend=Sales_Lowerbound)) +
  geom_point(  aes( x = Day,
                    y = Sales_Upperbound,
                    colour = "Upper")) +
  geom_point( aes( x = Day, 
                   y = Sales_Lowerbound,
                   colour = 'Lower')) + 
  labs( x = 'Day in January',
        y = 'Prediction Interval',
        title = 'Predicted Sales in January')