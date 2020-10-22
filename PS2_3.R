setwd('D:/ESEHW#2')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
data3 <- read.csv(file = 'HW2_3Temperature.csv',header=TRUE)

#首先检查数据

temp_data <- data3 %>% 
  select(year.mon,p1) %>%
  mutate(Temperature = p1 * 0.1) #温度单位为0.1℃，转为1℃。
range(temp_data[3]) #检查温度范围，发现最值在[-8.2,33.6]之间，质量合格
  
temp_data %>% #做出2000年~2020年之间月平均温度的变化情况
  select(year.mon,Temperature) %>%
  ggplot(aes(x=as.Date(year.mon,format='%Y/%m/%d'), y=Temperature)) +
  labs(x='year',y='mean_temperature(℃)') +
  geom_point(size=2) +
  geom_line(size=1)
#此时已经重新作业1-7的相同时间序列图
#后续为另外补充作图

data3 %>% #作图观察20年每年相同月份的平均温度变化情况
  # Make the plot
  ggplot(aes(x=year, y=p1 * 0.1, color=mon)) + 
  labs(x='year',y='mean_temperature(℃)') +
  scale_color_gradient(low = "red",high = "green") +
  geom_point() +  
  geom_line() +
  facet_wrap(~ mon)


data_years <- data3 %>%  #作图观察2000，2007，2013，2019各年月平均温度的变化
  filter(year == 2000 | year == 2007 | year == 2013 | year == 2019) %>%
  mutate(tem = p1 * 0.1) %>%
  select(year,mon, tem) 

ggplot(data=data_years, aes(x=factor(mon), y=factor(tem), #factor 转化为离散型
      group=factor(year), colour=factor(year),
      shape = factor(year))) +
  labs( x='Month',y='Mean Temperature(℃)') +
  geom_line(size = 1) +
  theme(legend.title = element_blank())+  #去除图例名字
  geom_point(size = 4)







  