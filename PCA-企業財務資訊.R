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

ggplot(melt(cor(financial.data[, 2:ncol(financial.data)])),aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

#將資料進行標準化，scale=T，找出可以解釋變數80%的主成分，取到PC6
pca.model<-prcomp(financial.data[,2:ncol(financial.data)],scale. = T)
names(pca.model)
summary(pca.model)

#開始看主成分的係數，方便解釋主成分內容，使用melt讓資料變成tidy格式，發現做圖出來難解釋
head(pca.model$rotation,5)

ggplot(melt(pca.model$rotation[, 1:6]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

#換個方式來解釋主成分，非負稀疏主成份分析
#k = 非0係數個數，通常是(每個主成份期待非0係數個數)x 變數個數
#nneg = 是否希望所有係數都非負，TRUE 代表有非負限制
#取到第八個主成分
set.seed(1234)
install.packages("nsprcomp")
library(nsprcomp)
nspca.model <- nscumcomp(financial.data[,2:17],k=90, nneg=T,scale. = T)
summary(nspca.model)

ggplot(melt(nspca.model$rotation[, 1:8]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "white", high = "steelblue") +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

#主成份 1 重點為「股東權益獲利與成長能力」
#主成份 2 重點為「資產獲利能力」
#主成份 3 重點為「毛利與週轉率」