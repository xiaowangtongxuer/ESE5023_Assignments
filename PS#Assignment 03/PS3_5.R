setwd('D:/ESEHW#3')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
data5 <- read.csv("data5.csv", header=T)
data5 <- data5 %>% #整理数据
  select(Nebula,Velocity,Distance)
#5.1
plot(x=data5$Velocity,y=data5$Distance,  #作出散点图进行分析
     ylab = 'Distance(megaparsecs)',
     xlab = 'Velocity(km/s)',
     main = "Distance vs Velocity",
     pch = 20,cex = 2,col = "orange")

#5.2
fit2 <- lm(data5$Distance~data5$Velocity) #进行线性拟合，并且在散点图上添加拟合直线
abline(fit2, lwd = 5, col = "blue")

#5.3
fit3 <- lm(data5$Distance~data5$Velocity-1) #为了使得拟合直线过原点，使用lm(y~x-1)函数
plot(x=data5$Velocity,y=data5$Distance,  #作出散点图，并添加处理后的拟合直线
     ylab = 'Distance(megaparsecs)',
     xlab = 'Velocity(km/s)',
     main = "Distance vs Velocity",
     pch = 20,cex = 2,col = "orange")
abline(fit3, lwd = 5, col = "green")
points(x=0,y=0,pch='*',cex=3 ,col='blue')
summary(fit3) #查看处理后的拟合结果
coefficients(fit3)