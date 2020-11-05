#�趨����Ŀ¼�����ذ�װ��
setwd('D:/ESEHW#3')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
#1.1
data1 <- read.csv("data1.csv", header=T)#��ȡ�ļ�����������
data1 <- data1 %>%
  select(type,prcp)
ggplot(data1, aes(x = type, y = prcp, fill = type)) + #��������ͼ
  geom_boxplot() +
  theme_classic()

data1 %>% #չʾtype�������Լ��������ľ�ֵ�ͷ���
  group_by(type) %>%
  summarise(
    count = n(),
    mean_prcp = mean(prcp, na.rm = TRUE),
    sd_prcp = sd(prcp, na.rm = TRUE)
  )

#1.2
data_seeded <- data1 %>% #�ֱ�����Ӧ������
  filter(type == "seeded")
data_unseeded <- data1 %>%
  filter(type == "unseeded")

data1 %>% #չʾtype�������Լ��������ľ�ֵ�ͷ���
  group_by(type) %>%
  summarise(
    count = n(),
    mean_prcp = mean(prcp, na.rm = TRUE),
    sd_prcp = sd(prcp, na.rm = TRUE)
  )

hist(data_seeded$prcp) 
hist(data_unseeded$prcp)  #���ԵĽ��겻������̬�ֲ�
anova_one_way <- aov(prcp ~ type, data = data1) #�����ط������
summary(anova_one_way)

t.test(data_seeded$prcp,data_unseeded$prcp)