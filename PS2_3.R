setwd('D:/ESEHW#2')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
data3 <- read.csv(file = 'HW2_3Temperature.csv',header=TRUE)

#���ȼ������

temp_data <- data3 %>% 
  select(year.mon,p1) %>%
  mutate(Temperature = p1 * 0.1) #�¶ȵ�λΪ0.1�棬תΪ1�档
range(temp_data[3]) #����¶ȷ�Χ��������ֵ��[-8.2,33.6]֮�䣬�����ϸ�
  
temp_data %>% #����2000��~2020��֮����ƽ���¶ȵı仯���
  select(year.mon,Temperature) %>%
  ggplot(aes(x=as.Date(year.mon,format='%Y/%m/%d'), y=Temperature)) +
  labs(x='year',y='mean_temperature(��)') +
  geom_point(size=2) +
  geom_line(size=1)
#��ʱ�Ѿ�������ҵ1-7����ͬʱ������ͼ
#����Ϊ���ⲹ����ͼ

data3 %>% #��ͼ�۲�20��ÿ����ͬ�·ݵ�ƽ���¶ȱ仯���
  # Make the plot
  ggplot(aes(x=year, y=p1 * 0.1, color=mon)) + 
  labs(x='year',y='mean_temperature(��)') +
  scale_color_gradient(low = "red",high = "green") +
  geom_point() +  
  geom_line() +
  facet_wrap(~ mon)


data_years <- data3 %>%  #��ͼ�۲�2000��2007��2013��2019������ƽ���¶ȵı仯
  filter(year == 2000 | year == 2007 | year == 2013 | year == 2019) %>%
  mutate(tem = p1 * 0.1) %>%
  select(year,mon, tem) 

ggplot(data=data_years, aes(x=factor(mon), y=factor(tem), #factor ת��Ϊ��ɢ��
      group=factor(year), colour=factor(year),
      shape = factor(year))) +
  labs( x='Month',y='Mean Temperature(��)') +
  geom_line(size = 1) +
  theme(legend.title = element_blank())+  #ȥ��ͼ������
  geom_point(size = 4)







  