#设定工作目录，加载安装包
setwd('D:/ESEHW#3')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
#1.1
data1 <- read.csv("data1.csv", header=T)#读取文件并处理数据
data1 <- data1 %>%
  select(type,prcp)
ggplot(data1, aes(x = type, y = prcp, fill = type)) + #作出箱型图
  geom_boxplot() +
  theme_classic()

data1 %>% #展示type的类型以及降雨量的均值和方差
  group_by(type) %>%
  summarise(
    count = n(),
    mean_prcp = mean(prcp, na.rm = TRUE),
    sd_prcp = sd(prcp, na.rm = TRUE)
  )

#1.2
data_seeded <- data1 %>% #分别获得相应的数据
  filter(type == "seeded")
data_unseeded <- data1 %>%
  filter(type == "unseeded")

data1 %>% #展示type的类型以及降雨量的均值和方差
  group_by(type) %>%
  summarise(
    count = n(),
    mean_prcp = mean(prcp, na.rm = TRUE),
    sd_prcp = sd(prcp, na.rm = TRUE)
  )

hist(data_seeded$prcp) 
hist(data_unseeded$prcp)  #各自的降雨不满足正态分布
anova_one_way <- aov(prcp ~ type, data = data1) #单因素方差分析
summary(anova_one_way)

t.test(data_seeded$prcp,data_unseeded$prcp)
