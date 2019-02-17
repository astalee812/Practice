load("titanic.raw.rdata")
str(titanic.raw)
library(arules)

#使用apriori的方法，資料跑出來再rhs的部分會有很多東西
rule2<-apriori(titanic.raw,parameter=list(minlen=3, supp=0.1, conf=0.7))
inspect(rule2)

#多加點規則，讓rhs只有跑出Survived的數值出現
rule<-apriori(titanic.raw,parameter=list(minlen=3, supp=0.1, conf=0.7),  
              appearance = list(default="lhs",
                                rhs=c("Survived=No", "Survived=Yes")))
inspect(rule)

#用lift排序一下，看看規則是如何
#第一個關聯規則：「若身分是成人女性 => 則會存活」，lift=2.3 > 1，表示這個規則相當具有正相關
inspect(sort(rule,by="lift"))

#來畫個圖
library(arulesViz)
plot(rule,method = "graph"