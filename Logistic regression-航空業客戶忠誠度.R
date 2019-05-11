setwd("E:/R語言與商業分析")
install.packages("tidyverse")
library(tidyverse)
internal.data <- read_csv("internal_data.csv")
survey.data <- read_csv("survey_data.csv")
internal.data$credit_card_vendor <- as.factor(internal.data$credit_card_vendor)
head(internal.data, 5)
head(survey.data, 5)

complete.data <- merge(survey.data, internal.data,by = "user_id")
head(complete.data, 5)

complete.data$user_id <- as.character(complete.data $user_id)
complete.data$credit_card_bonus <- as.factor(complete.data$credit_card_bonus)
complete.data$register_method <- as.factor(complete.data $register_method) 
complete.data$class <- as.factor(complete.data$class)

#做dummy variable
install.packages("dummies")
library(dummies)
complete.data.new <- dummy.data.frame(complete.data,
                                      sep = "_",
                                      dummy.classes = 'factor')
head(complete.data.new, 5)

#了解各個變相之間的相關程度
cor(complete.data.new[, 2:ncol(complete.data.new)])[1:5, 1:5]

library(reshape2)
head(melt(cor(complete.data.new[, 2:ncol(complete.data.new)])), 5)

ggplot(melt(cor(complete.data.new[, 2:ncol(complete.data.new)])),
       aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

#製作回歸模型
marketing.model <- glm(is_loyal ~ dm_message + dm_post + dm_email +              
                         credit_card_vendor + credit_card_bonus + 
                         tv_ad + youtube_ad_1 + youtube_ad_2 + youtube_ad_3, 
                       data = complete.data, family = binomial(link="logit"))
summary(marketing.model)

#利用predict來得到模型預估
predict.prob <- predict(marketing.model, complete.data, type = "response")

#利用 InformationValue 套件中計算模糊矩陣
install.packages("InformationValue")
library("InformationValue")
opt.cutoff <- optimalCutoff(complete.data$is_loyal, predict.prob)[1] 
confusionMatrix(complete.data$is_loyal, predict.prob, threshold = opt.cutoff)

misClassError(complete.data$is_loyal, predict.prob, threshold = opt.cutoff)
precision(complete.data$is_loyal, predict.prob, threshold = opt.cutoff)
sensitivity(complete.data$is_loyal, predict.prob, threshold = opt.cutoff)
specificity(complete.data$is_loyal, predict.prob, threshold = opt.cutoff)

#利用 plotROC 套件繪製分類模型的 ROC 曲線
install.packages("plotROC")
library("plotROC")
predict.table <- data.frame(true_label = complete.data$is_loyal,
                            predict_prob = predict.prob)

basic.plot <- ggplot(predict.table, aes(d = true_label, m = predict.prob)) +
  geom_roc(n.cuts = 3, labelsize = 3, labelround = 2)
basic.plot + style_roc() +
  annotate("text", x = .75, y = .25, size = 5,
           label = paste("AUC =", round(calc_auc(basic.plot)$AUC, 3)))

#考量「服務品質因素」的邏輯迴規模型
service.model <- glm(is_loyal ~
                       depart_on_time + arrive_on_time +
                       register_method + register_rate +
                       class + seat_rate + meal_rate +
                       flight_rate + package_rate,
                     data=complete.data, family=binomial(link="logit"))
summary(service.model)

predict.prob <- predict(service.model, complete.data, type="response")
opt.cutoff <- optimalCutoff(complete.data$is_loyal, predict.prob)[1] 
misClassError(complete.data$is_loyal, predict.prob, threshold = opt.cutoff)

precision(complete.data$is_loyal, predict.prob, threshold = opt.cutoff)
sensitivity(complete.data$is_loyal, predict.prob, threshold = opt.cutoff)
specificity(complete.data$is_loyal, predict.prob, threshold = opt.cutoff)

predict.table <- data.frame(true_label = complete.data$is_loyal,
                            predict_prob = predict.prob)

basic.plot <- ggplot(predict.table, aes(d = true_label, m = predict.prob)) +
  geom_roc(n.cuts = 3, labelsize = 3, labelround = 2)
basic.plot + style_roc() +
  annotate("text", x = .75, y = .25, size = 5,
           label = paste("AUC =", round(calc_auc(basic.plot)$AUC, 3)))

#包含「行銷活動」與「服務品質」
full.model <- glm(is_loyal ~ depart_on_time + arrive_on_time +
                    register_method + register_rate +
                    class + seat_rate + meal_rate +
                    flight_rate + package_rate +
                    dm_message + dm_post + dm_email +
                    credit_card_vendor + credit_card_bonus +
                    tv_ad + youtube_ad_1 + youtube_ad_2 + youtube_ad_3,
                  data = complete.data,
                  family = binomial(link="logit"))

summary(full.model)

predict.prob <- predict(full.model, complete.data, type="response")
opt.cutoff <- optimalCutoff(complete.data$is_loyal, predict.prob)[1] 
misClassError(complete.data$is_loyal, predict.prob, threshold = opt.cutoff)

confusionMatrix(complete.data$is_loyal, predict.prob, threshold = opt.cutoff)
sensitivity(complete.data$is_loyal, predict.prob, threshold = opt.cutoff)
specificity(complete.data$is_loyal, predict.prob, threshold = opt.cutoff)

predict.table <- data.frame(true_label = complete.data$is_loyal,
                            predict_prob = predict.prob)
basic.plot <- ggplot(predict.table, aes(d = true_label, m = predict.prob)) +
  geom_roc(n.cuts = 3, labelsize = 3, labelround = 2)
basic.plot + style_roc() +
  annotate("text", x = .75, y = .25, size = 5,
           label = paste("AUC =", round(calc_auc(basic.plot)$AUC, 3)))



# Get the coefficient table
summary.table <- data.frame(var_name = names(coefficients(full.model)),
                            coefficient = coefficients(full.model))
# Filter marketing-related variables
summary.table <- summary.table %>%
  filter(var_name %in% names(coefficients(marketing.model)) &
           var_name != "(Intercept)")
# Sort the table by the size of coefficients
summary.table <- summary.table[sort(summary.table$coefficient, index.return = T)$ix, ]
# Set correct variable type
summary.table$var_name <- factor(summary.table$var_name,
                                 levels = summary.table$var_name)
# Visualize the bar chart
ggplot(data = summary.table,
       aes(x = var_name, y = coefficient)) +
  geom_bar(aes(fill = var_name),
           position = "dodge",
           stat = "identity",
           show.legend = FALSE) +
  theme_bw(base_size = 14) +
  labs(title = "Direct marketing approach is not that useful ...",
       x = "Marketing Strategy", y = "Impact on Cusomer Loyalty") +
  coord_flip() 


# Get the coefficient table
summary.table <- data.frame(var_name = names(coefficients(marketing.model)),
                            coefficient = coefficients(marketing.model))
summary.table <- summary.table %>%
  filter(var_name != "(Intercept)")
# Sort the table by the size of coefficients
summary.table <- summary.table[sort(summary.table$coefficient, index.return = T)$ix, ]
# Set correct variable type
summary.table$var_name <- factor(summary.table$var_name,
                                 levels = summary.table$var_name)
# Visualize the bar chart
ggplot(data = summary.table,
       aes(x = var_name, y = coefficient)) +
  geom_bar(aes(fill = var_name),
           position = "dodge",
           stat = "identity",
           show.legend = FALSE) +
  theme_bw(base_size = 14) +
  labs(title = "While most strategies are effective, we should
          reconsider Youtube Ad 1 and text message.",
       x = "Marketing Strategy", y = "Impact on Cusomer Loyalty") +
  coord_flip() 



# Get the summary table for dm strategies
var.names <- c("dm_post", "dm_email", "dm_message")
summary.table <- complete.data %>%
  group_by_(treatment = var.names[1]) %>%
  summarize(num_member = length(user_id),
            num_loyal = sum(is_loyal))
summary.table$var_name <- var.names[1] 
for(i in 2:3){
  temp <- complete.data %>%
    group_by_(treatment = var.names[i]) %>%
    summarize(num_member = length(user_id),
              num_loyal = sum(is_loyal))
  temp$var_name <- var.names[i] 
  summary.table <- rbind(summary.table, temp)
}
summary.table$proportion <- summary.table$num_loyal / summary.table$num_member
# Set the correct variavle type
summary.table$treatment <- as.factor(summary.table$treatment)
summary.table$var_name <- as.factor(summary.table$var_name)
# Plot the result
ggplot(data = summary.table,
       aes(x = var_name, y = proportion)) +
  geom_bar(aes(fill = treatment),
           position = "dodge",
           stat = "identity") +
  theme_bw(base_size = 14) +
  labs(x = "DM Campaign", y = "Proportion of Loyal Members",
       title = "While DM is useful, mobile message is not a good channel.")



# Get the summary table for credit card vendors
summary.table <- complete.data %>%
  group_by(credit_card_vendor) %>%
  summarize(num_member = length(user_id),
            num_loyal = sum(is_loyal))
summary.table$proportion <- summary.table$num_loyal / summary.table$num_member
# Visualize the result
ggplot(data = summary.table, aes(x = credit_card_vendor, y = proportion)) +
  geom_bar(aes(fill = credit_card_vendor), position = "dodge", stat = "identity") +
  theme_bw(base_size = 14) +
  labs(x = "Credit Card Vendor", y = "Proportion of Loyal Members",
       title = "While credit card vendors seems to be important ...")


# Get the summary table for credit card vendors and bonus levels
summary.table <- complete.data %>%
  group_by(credit_card_vendor, credit_card_bonus) %>%
  summarize(num_member = length(user_id),
            num_loyal = sum(is_loyal))
summary.table$proportion <- summary.table$num_loyal / summary.table$num_member
# Set correct data type
summary.table$credit_card_bonus <- as.factor(summary.table$credit_card_bonus)
# Visualize the result
ggplot(data = summary.table, aes(x = credit_card_vendor, y = proportion)) +
  geom_bar(aes(fill = credit_card_bonus), position = "dodge", stat = "identity") +
  theme_bw(base_size = 14) +
  labs(x = "Credit Card Vendor", y = "Proportion of Loyal Members",
       title = "... the root cause is bonus level.")



# Get the summary table for dm strategies
var.names <- c("tv_ad", "youtube_ad_1", "youtube_ad_2", "youtube_ad_3")
summary.table <- complete.data %>%
  group_by_(treatment = var.names[1]) %>%
  summarize(num_member = length(user_id),
            num_loyal = sum(is_loyal))
summary.table$var_name <- var.names[1] 
for(i in 2:4){
  temp <- complete.data %>%
    group_by_(treatment = var.names[i]) %>%
    summarize(num_member = length(user_id),
              num_loyal = sum(is_loyal))
  temp$var_name <- var.names[i] 
  summary.table <- rbind(summary.table, temp)
}
summary.table$proportion <- summary.table$num_loyal / summary.table$num_member
# Set the correct variavle type
summary.table$treatment <- as.factor(summary.table$treatment)
summary.table$var_name <- as.factor(summary.table$var_name)
# Plot the result
ggplot(data = summary.table,
       aes(x = var_name, y = proportion)) +
  geom_bar(aes(fill = treatment),
           position = "dodge",
           stat = "identity") +
  theme_bw(base_size = 14) +
  labs(x = "Advertisement", y = "Proportion of Loyal Members",
       title = "TV Ad is very strong, but Youtube Ad1 is problematic.")