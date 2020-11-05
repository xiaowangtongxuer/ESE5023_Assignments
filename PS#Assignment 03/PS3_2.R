#设定工作目录，加载安装包
setwd('D:/ESEHW#3')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
#2
data2 <- read.csv("data2.csv", header=T)
ggplot(data2, aes(x = BONE, y = OIC, fill = BONE)) + #做箱型图分析均值和分布情况
  geom_boxplot() +
  theme_classic()
data2 %>%  #查看不同骨头的OIC均值和方差
  group_by(BONE) %>%
  summarise(
    count = n(),
    mean_OIC = mean(OIC, na.rm = TRUE),
    sd_OIC = sd(OIC, na.rm = TRUE)
  )
anova_one_way <- aov(OIC ~ BONE, data = data2) # 单因素方差分析
summary(anova_one_way)
