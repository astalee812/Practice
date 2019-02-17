Train<-read.csv("C:/Users/ASUS/Desktop/titanic/train.csv")
Test<-read.csv("C:/Users/ASUS/Desktop/titanic/test.csv")

#檢查一下有沒有NA值，會發現ge部分爆炸多NA值
colSums(is.na(full))

#處理NA值的時候到了
Train$Age[is.na(Train$Age)] = mean(Train$Age, na.rm = TRUE)
Test$Age[is.na(Test$Age)] = mean(Test$Age, na.rm = TRUE)

#train跟test的欄位不同，無法用rbind做合併，則使用bind_rows做合併
install.packages("dplyr")
library(dplyr)
full<-bind_rows(Train, Test)

#再檢查一次NA值
colSums(is.na(full))

#了解一下合併的資料長怎樣
str(full)
summary(full)

#設定factor
full$Survived=as.factor(full$Survived)
full$Sex=as.factor(full$Sex)
full$Pclass=as.factor(full$Pclass)
full$Embarked=as.factor(full$Embarked)

str(full)

install.packages("installr")
require(installr)
updateR()

#畫圖時間
install.packages("ggplot2")
library(ggplot2)

ggplot(Train,aes(x=Sex,fill=Survived))+geom_bar()
