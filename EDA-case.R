setwd("E:/R語言與商業分析")
install.packages("tidyverse")
library(tidyverse)

#knitr package可以協助畫出更好看的圖表
install.packages("knitr")
library(knitr)

#使用read_csv，三張表格都會是tibble的形式
SalesTable<-read_csv("E:/R語言與商業分析/SalesTable.csv")
ClientTable<-read_csv("E:/R語言與商業分析/ClientTable.csv")
ProductTable<-read_csv("E:/R語言與商業分析/ProductTable.csv")

install.packages("magrittr")
library(magrittr)

#把三個表結合起來
SalesTableNew<-SalesTable %>%
  inner_join(ClientTable,by="Client_ID") %>%
  inner_join(ProductTable,by="Product_ID")