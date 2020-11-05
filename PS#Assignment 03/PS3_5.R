setwd('D:/ESEHW#3')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
data5 <- read.csv("data5.csv", header=T)
data5 <- data5 %>% #��������
  select(Nebula,Velocity,Distance)
#5.1
plot(x=data5$Velocity,y=data5$Distance,  #����ɢ��ͼ���з���
     ylab = 'Distance(megaparsecs)',
     xlab = 'Velocity(km/s)',
     main = "Distance vs Velocity",
     pch = 20,cex = 2,col = "orange")

#5.2
fit2 <- lm(data5$Distance~data5$Velocity) #����������ϣ�������ɢ��ͼ���������ֱ��
abline(fit2, lwd = 5, col = "blue")

#5.3
fit3 <- lm(data5$Distance~data5$Velocity-1) #Ϊ��ʹ�����ֱ�߹�ԭ�㣬ʹ��lm(y~x-1)����
plot(x=data5$Velocity,y=data5$Distance,  #����ɢ��ͼ�������Ӵ���������ֱ��
     ylab = 'Distance(megaparsecs)',
     xlab = 'Velocity(km/s)',
     main = "Distance vs Velocity",
     pch = 20,cex = 2,col = "orange")
abline(fit3, lwd = 5, col = "green")
points(x=0,y=0,pch='*',cex=3 ,col='blue')
summary(fit3) #�鿴���������Ͻ��
coefficients(fit3)