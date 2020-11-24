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

# @MingYANG recommended：
# share my code with you blow ↓↓↓
# Unseeded <- c(1202.6, 830.1, 372.4, 345.5, 321.2, 244.3, 163.0, 147.8, 95.0, 87.0, 81.2, 68.5, 47.3, 41.1, 36.6, 29.0, 28.6, 26.3, 26.0, 24.4, 21.4, 17.3, 11.5, 4.9, 4.9, 1.0)
# Seeded <- c(2745.6, 1697.1, 1656.4, 978.0, 703.4, 489.1, 430.0, 334.1, 302.8, 274.7, 274.7, 255.0, 242.5, 200.7, 198.6, 129.6, 119.0, 118.3, 115.3, 92.4, 40.6, 32.7, 31.4, 17.5, 7.7, 4.1)
# rainfall <- cbind(Unseeded,Seeded)      # you may find "cbind" and "rbind" useful!
# data <- data.frame(rainfall)
# boxplot(rainfall,width=c(1,2),col=c(2,7),border=c("purple","black"))
# t.test(Unseeded,Seeded)             # returns the same value of p-value with anova analysis
# the end
