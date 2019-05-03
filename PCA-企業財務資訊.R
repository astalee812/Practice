setwd("E:/R語言與商業分析")
install.packages("tidyverse")
library(tidyverse)
financial.data <- read_csv("2017_financial index_163 comp.csv")

head(financial.data,5)
summary(financial.data)

#找出變數之間的相關性並畫圖
financial.cor<-cor(financial.data[,2:ncol(financial.data)])
install.packages("corrplot")
library(corrplot)
corrplot.mixed(financial.cor)

#另一種方式畫圖
library(reshape2)
head(melt(cor(financial.data[, 2:ncol(financial.data)])), 5)


