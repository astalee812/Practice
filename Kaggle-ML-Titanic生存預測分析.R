Train<-read.csv("C:/Users/ASUS/Desktop/titanic/train.csv")
Test<-read.csv("C:/Users/ASUS/Desktop/titanic/test.csv")

#檢查一下有沒有NA值，會發現ge部分爆炸多NA值
colSums(is.na(full))

#處理NA值的時候到了
Train$Age[is.na(Train$Age)] = mean(Train$Age, na.rm = TRUE)
Test$Age[is.na(Test$Age)] = mean(Test$Age, na.rm = TRUE)
full$Embarked[full$Embarked==""]="C"

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
update.packages()
install.packages("devtools")
devtools::install_github("tidyverse/ggplot2")

#畫圖時間
install.packages("ggplot2")
library(ggplot2)

#看看性別跟生存與否之間有沒有關係，看來快男人死光光
LT=dim(Train)[1]
ggplot(data = full[1:LT,],aes(x=Sex,fill=Survived))+geom_bar()

#看看Embarked跟生存與否之間有沒有關係
ggplot(data = full[1:LT,],aes(x=Embarked,fill=Survived))+geom_bar(position="fill")+ylab("Frequency")

#看看Pclss跟生存與否之間有沒有關係，等級越高生存率越高
ggplot(data = full[1:LT,],aes(x=Pclass,fill=Survived))+geom_bar(position="fill")+ylab("Frequency")

#看看Pclss跟Embarked與生存與否之間有沒有關係
ggplot(data = full[1:LT,],aes(x=Embarked,fill=Survived))+geom_bar(position="fill")+facet_wrap(~Pclass)

#看看年紀跟生存會有什麼差別
full$Age[is.na(full$Age)] <- mean(full$Age,na.rm=T)
sum(is.na(full$Age))
ggplot(full[1:LT,],aes(x=Age,fill=Survived))+geom_histogram(binwidth =3)

#看看Fare跟生存會有什麼差別
full$Fare[is.na(full$Fare)] <- mean(full$Fare,na.rm=T)
ggplot(data = full[1:LT,],aes(x=Fare,fill=Survived))+geom_histogram(binwidth =20, position="fill")


