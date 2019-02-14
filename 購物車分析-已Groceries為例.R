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

#這個圖表是將支持度Support放入，表示支持度不到0.1的不會列入圖表中
itemFrequencyPlot(Groceries,support = 0.1,
                  main = "Item Frequency with Support = 0.1",ylab = "Relative Frequency")

install.packages("arulesViz")
library(arulesViz)


#support=「規則」在資料內具有普遍性，也就是這些 A 跟 B 同時出現的機率多少。
#confidence=「規則」要有一定的信心水準，也就是當購買 A 狀態下，也會購買 B 的條件機率。
#rhs 代表買左邊也會買右邊的意思
#lift=1.5 > 1，表示了這個規則相當具有正相關
rule<-apriori(Groceries,parameter = list(support=0.005,confidence=0.5))
inspect(rule[1:5])
