setwd('D:/ESEHW#3')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
data7 <- read.csv("data7.csv", header=T)
unite_data <- unite(data7, "monthy", year, month, sep = "-",
                    remove = FALSE) #����monthy���������ƽ������
# 7.1 �Ƚ�2000���2019�����ƽ���¶�
box_data <- unite_data %>% #�������ݣ�����õ�2000���2019�������ƽ������
  #filter(year==2000 | year == 2019) %>%
  group_by(monthy) %>%
  summarise(sum_prcp = sum(prcp),mean_temp = mean(temp),type = mean(year)) %>%
  mutate(date = ifelse(type==2000,'2000','2019'))
ggplot(box_data, aes(x = date , y = mean_temp, fill =date )) + #������ͬ�����ƽ���¶ȵ�����ͼ
  geom_boxplot() +
  theme_classic()


box_data2000 <- box_data %>% 
  filter(type==2000)
box_data2019 <- box_data %>%
  filter(type==2019)
hist(box_data2000$mean_temp)
hist(box_data2019$mean_temp) #�鿴2000���2019����Եķֲ�
t.test(box_data2000$mean_temp,box_data2019$mean_temp) #����t-test���飬�鿴2000���2019����ƽ���¶����ݲ������

#7.2 ��������Ϊ����ʹ���ʱ���Ե���Ŀ���ʪ�ȵ�Ӱ�죬dara7�е�humidityΪ�վ�����ʪ��
data_diff <- data7 %>% #�������ݣ����ݽ��������ٷֱ���Ϊ�����������
  select(prcp,humidity) %>%
  filter(prcp< 50 & prcp >= 10) %>%
  mutate(prcp_type = ifelse(prcp < 25 , 'mid_prcp','big_prcp'))
ggplot(data_diff, aes(x = prcp_type, y = humidity, fill = prcp_type)) + #������ͬ�������͵�����ͼ
  geom_boxplot() +
  theme_classic()
anova_one_way <- aov(humidity ~ prcp_type, data = data_diff) #���е����ط��������̽���ڲ�ͬ�������Կ���ʪ�ȵ�Ӱ��
summary(anova_one_way)

#7.3 ̽�����½��������Ը��������ƽ��������Ӱ��
data_liner <- unite_data %>%  #�������ݺ�õ����µĽ���������ƽ���������ݣ�
  group_by(monthy) %>%
  summarise(sum_prcp=sum(prcp),mean_q=mean(Q))

plot(x=data_liner$sum_prcp,y=data_liner$mean_q , #�������½���������ƽ��������ɢ��ͼ
     ylab = 'Monthy Q',
     xlab = 'Prcp(mm/mon)',
     main = "Prcp vs Monthy Q",
     pch = 20,cex = 2,col = "orange")
fit4 <- lm(data_liner$mean_q~data_liner$sum_prcp) #ʹ�ü��������
abline(fit4, lwd = 5, col = "green")
points(mean(data_liner$sum_prcp), mean(data_liner$mean_q), pch = "*", cex = 3,col='red')
summary(fit4) #�鿴��Ͻ�������������Ƿ����������ع�ϵ













