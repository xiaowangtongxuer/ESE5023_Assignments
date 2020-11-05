#设定工作目录，加载安装包
setwd('D:/ESEHW#3')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
data3 <- read.csv("data3.csv", header=T)
data3 <- data3 %>% #整理数据
  select(women,pregnant,vegetarians,zinc.status)
ggplot(data3, aes(x = women, y = zinc.status, fill = women)) +
  geom_boxplot() +
  theme_classic()
data3 %>%
  group_by(women) %>%
  summarise(
    count = n(),
    mean_Zn = mean(zinc.status, na.rm = TRUE),
    sd_Zn = sd(zinc.status, na.rm = TRUE)
  )
ggplot(data3, aes(x = women, y = zinc.status, fill = women)) +
  geom_boxplot() +
  theme_classic()

anova_one_way <- aov(zinc.status~ women, data = data3)
summary(anova_one_way)
anova_two_way <- aov( zinc.status~ pregnant + vegetarians, data = data3)
summary(anova_two_way)


