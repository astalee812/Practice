library(tidyverse)
library(knitr)

setwd("E:/R語言與商業分析")
GameLog <- read.csv('Game_Log.csv')
UserTable <- read.csv('User_Table.csv')
str(GameLog)

#開始合併兩個資料，使用userid做合併，因為每個id都有五筆資料作為五天的資料
#使用五天平均值作為數值
GameTable <- GameLog %>%
  group_by(User_Id) %>%
  summarise(
    Min_Aft = mean(Min_Aft),
    Min_Eve = mean(Min_Eve),
    Min_Mid = mean(Min_Mid),
    Buy_Coin = mean(Buy_Coin),
    Buy_Dia = mean(Buy_Dia),
    Buy_Car = mean(Buy_Car)
  ) %>%
  inner_join(UserTable, by = 'User_Id')

GameTable %>% summary()

#這邊可以發現資料的尺度並不相同，我們開始將所有變數做標準化
#現在我們先來針對數值變數做標準化，將數值放到[1,0]的範圍之中
GameTable <- GameTable[,2:7] %>%
  mutate(
    Aft = (Min_Aft - min(Min_Aft)) / (max(Min_Aft)-min(Min_Aft)),
    Eve = (Min_Eve - min(Min_Eve)) / (max(Min_Eve)-min(Min_Eve)),
    Mid = (Min_Mid - min(Min_Mid)) / (max(Min_Mid)-min(Min_Mid)),
    Coin = (Buy_Coin - min(Buy_Coin)) / (max(Buy_Coin)-min(Buy_Coin)),
    Dia = (Buy_Dia - min(Buy_Dia)) / (max(Buy_Dia)-min(Buy_Dia)),
    Car = (Buy_Car - min(Buy_Car)) / (max(Buy_Car)-min(Buy_Car))
  ) %>% cbind(
    GameTable[,c(8,9)]
  )

#類別變數是使用model.matrix做轉換dummy
#model.matrix使用波浪後將要轉換的資料寫上
DummyTable <- model.matrix( ~ Identity + Telecom, data = GameTable)
head(DummyTable)

#將未轉換的資料刪除以及dummy中的intercept刪除，然後再進行結合
GameTable <- cbind(
  GameTable[, -c(1:6,13,14)],
  DummyTable[, -1]
)
head(GameTable)

#使用correlation做變數間的相關性，melt函數是為了得到更清楚的資料格式
library(reshape2)
CorMatrix <- GameTable %>% cor() %>% melt()
head(CorMatrix[ 1:5,])

#使用ggplot來繪製熱密度圖(angle=轉角度)
ggplot( data = CorMatrix) +
  geom_tile(aes(Var1, Var2,fill = value), colour = "white") + 
  scale_fill_gradient2(low = "firebrick4", high = "steelblue") +
  guides(fill=guide_legend(title="Correlation")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())



#階層式集群分析，用來做資料探索，使用歐式距離
set.seed(500)
Distance <- dist(GameTable, method = 'euclidean')
#method=complete，是使用最遠距離
#使用階層式集群分析後，可以看到有3or4個大群
hclust(Distance, method = 'complete') %>% plot()


#Kmeans演算法，先算3群，一定要set.seed讓隨機抽樣的結果都相同
set.seed(500)#remove the random effect
K <- kmeans(GameTable,3)
#把原本gametable資料跟K裡面的分群做結合
ClusterResult <- cbind(GameTable,K$cluster)%>%as.data.frame()
colnames(ClusterResult)[ncol(ClusterResult)] <- 'Cluster'
table(ClusterResult$Cluster)

#連續變數分配，為了將資料轉換成整齊的格式，要使用gather函數，會將所有變數放在同一個欄位中
#key=欄位名字，value=標準化過的值，然後把類別變數的欄位扣除
ClusterResultForPlot <- ClusterResult %>%
  gather( key = Continuous_Variable,
          value = Normalized_Value,
          - c(IdentityNovice, IdentityVeteran, Telecomother, Cluster))

#將數值變數的continuous variable變成factor，並給予level名稱
ClusterResultForPlot$Continuous_Variable <- ClusterResultForPlot$Continuous_Variable %>% factor( levels = c('Mid','Aft','Eve','Coin','Dia','Car'))
#畫圖!!!圖中可以發現特徵還是太發散
ggplot( data = ClusterResultForPlot) + 
  geom_boxplot( aes( x = Continuous_Variable,
                     y = Normalized_Value),
                size = 0.7) +
  facet_wrap( ~ Cluster)


#使用Kmeans分4群做計算
set.seed(500)
K <- kmeans(GameTable,4)
ClusterResult <- cbind(GameTable,K$cluster) %>% as.data.frame()
colnames(ClusterResult)[ncol(ClusterResult)] <- 'Cluster'
table(ClusterResult$Cluster)
#第一群：上線時間多集中於下午，其他行為較不明顯
#第二群：上線時間多集中於晚上，有穩定購買金幣的行為
#第三群：上線時間較分散且較長，有穩定購買卡片的行為
#第四群：上線時間集中於半夜，有穩定購買鑽石的行為


ClusterResultForPlot <- ClusterResult %>%
  gather( key = Continuous_Variable,
          value = Normalized_Value,
          - c(IdentityNovice, IdentityVeteran, Telecomother, Cluster))

ClusterResultForPlot$Continuous_Variable <- ClusterResultForPlot$Continuous_Variable %>% factor( levels = c('Aft','Eve','Mid','Coin','Dia','Car'))

ggplot( data = ClusterResultForPlot) + 
  geom_boxplot( aes( x = Continuous_Variable,
                     y = Normalized_Value),
                size = 0.7) +
  facet_wrap( ~ Cluster)

#處理一下類別變數，這邊是[1,0]範圍的數值，要將轉換過及未轉換的數值做合併
#以方便之後想知道真實數值的狀況
GameTableResult <-  GameLog %>%
  group_by(User_Id) %>%
  summarise(
    Min_Aft = mean(Min_Aft),
    Min_Eve = mean(Min_Eve),
    Min_Mid = mean(Min_Mid),
    Buy_Coin = mean(Buy_Coin),
    Buy_Dia = mean(Buy_Dia),
    Buy_Car = mean(Buy_Car)
  ) %>%
  inner_join(UserTable, by = 'User_Id') %>%
  cbind( K$cluster) %>% 
  as.data.frame()

colnames(GameTableResult)[ncol(GameTableResult)] <- 'Cluster'

#畫圖了解玩家身分與電信公司的狀況
ggplot( data = GameTableResult) +
  geom_bar( aes( x = Identity)) + 
  facet_wrap( ~ Cluster)

ggplot( data = GameTableResult) +
  geom_bar( aes( x = Telecom)) + 
  facet_wrap( ~ Cluster)

#使用ggfortify來做分群結果的視覺化，也會納入主成分分析
library(ggfortify)
set.seed(500)
autoplot(kmeans(GameTable[,1:6], 4), data  = GameTable)

ggplot( data = ClusterResultForPlot) + 
  geom_boxplot( aes( x = Continuous_Variable,
                     y = Normalized_Value),
                size = 0.7) +
  facet_wrap( ~ Cluster)