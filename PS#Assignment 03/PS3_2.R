#�趨����Ŀ¼�����ذ�װ��
setwd('D:/ESEHW#3')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
#2
data2 <- read.csv("data2.csv", header=T)
ggplot(data2, aes(x = BONE, y = OIC, fill = BONE)) + #������ͼ������ֵ�ͷֲ����
  geom_boxplot() +
  theme_classic()
data2 %>%  #�鿴��ͬ��ͷ��OIC��ֵ�ͷ���
  group_by(BONE) %>%
  summarise(
    count = n(),
    mean_OIC = mean(OIC, na.rm = TRUE),
    sd_OIC = sd(OIC, na.rm = TRUE)
  )
anova_one_way <- aov(OIC ~ BONE, data = data2) # �����ط������
summary(anova_one_way)