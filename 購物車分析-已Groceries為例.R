install.packages("arules")
library(arules)

#來看一下資料Groceries的資料型態
data("Groceries")
summary(Groceries)
str(Groceries)
head(Groceries)

#看Groceries的維度
dim(Groceries)

#來看看Groceries前5項的交易資料長怎樣
inspect(Groceries[1:5])

#看看每個交易的大小
size(Groceries)

#各商品出現的頻率，但是跑出來的結果好亂喔
itemFrequency(Groceries)


#我們來使用一下itemfrequencyplot來作圖，跑出來的圖爆炸詭異
itemFrequencyPlot(Groceries)

#加入topN=10來找出最高的10個品項
itemFrequencyPlot(Groceries,topN = 10)

#type=absolute表示顯示絕對值，若沒寫就會默認相對值
itemFrequencyPlot(Groceries,topN = 10,type = "absolute")

#horiz=T是表示將表格從長條變成橫條，然後加上表頭跟X軸名稱
itemFrequencyPlot(Groceries,topN = 10,horiz = T,
                  main = "Item Frequency",xlab = "Relative Frequency")

#視覺化檢視前20最熱賣的產品
itemFrequencyPlot(Groceries, topN=20, type="absolute",col = "dark red", cex=0.8)

#這個圖表是將支持度Support放入，表示支持度不到0.1的不會列入圖表中
itemFrequencyPlot(Groceries,support = 0.1,
                  main = "Item Frequency with Support = 0.1",ylab = "Relative Frequency")

#將前10項產品之稀鬆矩陣用視覺化圖型顯示
image(Groceries[1:10])

#隨機選擇100項交易紀錄，並利用視覺化圖型檢視
image(sample(Groceries, 100))

#找出品類名稱，會找到兩個level，我用level1的有55個品類
item<-itemInfo(Groceries)
str(item)

#把品類跟Groceries資料結合在一起且分類(aggregate)
Groceries2<-aggregate(Groceries,itemInfo(Groceries)[["level2"]])
print(dim(Groceries2)[1])  #表示有9835筆資料
print(dim(Groceries2)[2])  #表示有55個品類

#先把先前的圖案洗掉，不然圖案會跑不出來
dev.off()

#畫圖囉!
itemFrequencyPlot(Groceries2,support = 0.025, type = "relative", xlab = paste("Proportion of Market Baskets Containing Item", "\n(Item Relative Frequency or Support)"))


install.packages("arulesViz")
library(arulesViz)


#support=「規則」在資料內具有普遍性，也就是這些 A 跟 B 同時出現的機率多少。
#confidence=「規則」要有一定的信心水準，也就是當購買 A 狀態下，也會購買 B 的條件機率。
#rhs 代表買左邊也會買右邊的意思
#lift=1.5 > 1，表示了這個規則相當具有正相關

#第一次的關聯性規則，但是跑出來的條件太多了，做多次一點
rule1<-apriori(Groceries2,parameter = list(support=0.001,confidence=0.05))
rule2<-apriori(Groceries2,parameter = list(support=0.025,confidence=0.05))
rule3<-apriori(Groceries2,parameter = list(support=0.05,confidence=0.05))
print(summary(rule2))

#來個互動圖表
plot(rule2,colors=c("red","green"),engine="htmlwidget",
     marker=list(opacity=.6,size=8))

#再來另外一個圖表
rule22<-sort(rule2,by="support")
plot(rule22[1:20],method = "graph")



