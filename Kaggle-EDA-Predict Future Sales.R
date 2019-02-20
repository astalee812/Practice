setwd("C:/Users/ASUS/Desktop/Predict Future Sales")
sales_train<-read.csv("C:/Users/ASUS/Desktop/Predict Future Sales/sales_train_v2.csv")
testdata<-read.csv("C:/Users/ASUS/Desktop/Predict Future Sales/test.csv")
shop<-read.csv("C:/Users/ASUS/Desktop/Predict Future Sales/shops.csv")
item<-read.csv("C:/Users/ASUS/Desktop/Predict Future Sales/items.csv")
item_categories<-read.csv("C:/Users/ASUS/Desktop/Predict Future Sales/item_categories.csv")

head(sales_train)
head(testdata)
head(shop)
head(item)
head(item_categories)

salesdata<-merge(sales_train,shop,by="shop_id")
salesdata2<-merge(salesdata,item,by="item_id")
salesdata3<-merge(salesdata2,item_categories,by="item_category_id")

str(salesdata3)
summary(salesdata3)
