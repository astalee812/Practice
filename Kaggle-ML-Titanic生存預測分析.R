Train<-read.csv("C:/Users/ASUS/Desktop/titanic/train.csv")
Test<-read.csv("C:/Users/ASUS/Desktop/titanic/test.csv")

#train跟test的欄位不同，無法用rbind做合併，則使用bind_rows做合併
install.packages("dplyr")
library(dplyr)
full<-bind_rows(Train, Test)

