setwd('D:/ESEHW#3')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
data4 <- read.csv("data4.csv", header=T)
data_lm_test <- data4 %>% #�������ݣ���λ���㣬m -> km
  mutate(x = Ele / 1000,y = Tem) %>%
  select(x,y)
fit1 <- lm(data_lm_test$y~data_lm_test$x) #�������
plot(x=data_lm_test$x,y=data_lm_test$y,
     ylab = 'Temperature(degreea C)',
     xlab = 'Elevation(km)',
     main = "Temperature vs Elevation",
     pch = 20,cex = 2,col = "orange")
abline(fit1, lwd = 5, col = "blue") #��ͼ����
points(mean(data_lm_test$x), mean(data_lm_test$y), pch = "+", cex = 3)
summary(fit1) #�鿴��Ͻ��