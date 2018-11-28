install.packages("arulesViz") #關聯法則分析結果以視覺化呈現的套件
install.packages('arules') #關聯法則分析套件
#傻眼!沒辦法裝arules，來檢查一下版本
R.version
#怪怪!更新一下R好了
install.packages("installr")
library(installr)
updateR()
install.packages('arules') #原來版本要3.4以上才可以裝
install.packages("arulesViz")
library(arules)
data("Groceries")
Groceries
summary(Groceries)
apply(Groceries@data[,1:20],2,function(r)paste(Groceries@itemInfo[r,"lables"],collapse = ","))
#可可惡!這個跑出來的結果都是空白的

#開始分析關聯規則-第一次
itemset<-apriori(Groceries,parameter = list(minlen=1,maxlen=1,support=0.02,target="frequent itemsets"))
inspect(head(sort(itemset,by="support"),10))

#開始分析關聯規則-第二次
itemset2<-apriori(Groceries,parameter = list(minlen=2,maxlen=2,support=0.02,target="frequent itemsets"))

#開始分析關聯規則-第三次
itemset3<-apriori(Groceries,parameter = list(minlen=3,maxlen=3,support=0.02,target="frequent itemsets"))

#每次分析規則都掃描一次好累，把maxlen刪除讓程式自己跑到收斂
itemset4<-apriori(Groceries,parameter = list(minlen=3,support=0.02,target="frequent itemsets"))


