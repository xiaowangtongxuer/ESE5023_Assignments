setwd('D:/ESEHW#3')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
data7 <- read.csv("data7.csv", header=T)
unite_data <- unite(data7, "monthy", year, month, sep = "-",
                    remove = FALSE) #生成monthy便于求解月平均数据
# 7.1 比较2000年和2019年的月平均温度
box_data <- unite_data %>% #处理数据，计算得到2000年和2019年的逐月平均数据
  #filter(year==2000 | year == 2019) %>%
  group_by(monthy) %>%
  summarise(sum_prcp = sum(prcp),mean_temp = mean(temp),type = mean(year)) %>%
  mutate(date = ifelse(type==2000,'2000','2019'))
ggplot(box_data, aes(x = date , y = mean_temp, fill =date )) + #做出不同年份月平均温度的箱线图
  geom_boxplot() +
  theme_classic()


box_data2000 <- box_data %>% 
  filter(type==2000)
box_data2019 <- box_data %>%
  filter(type==2019)
hist(box_data2000$mean_temp)
hist(box_data2019$mean_temp) #查看2000年和2019年各自的分布
t.test(box_data2000$mean_temp,box_data2019$mean_temp) #经行t-test检验，查看2000年和2019年月平均温度数据差异如何

#7.2 当降雨量为中雨和大雨时，对当天的空气湿度的影响，dara7中的humidity为日均空气湿度
data_diff <- data7 %>% #处理数据，根据降雨量多少分别标记为：大雨和中雨
  select(prcp,humidity) %>%
  filter(prcp< 50 & prcp >= 10) %>%
  mutate(prcp_type = ifelse(prcp < 25 , 'mid_prcp','big_prcp'))
ggplot(data_diff, aes(x = prcp_type, y = humidity, fill = prcp_type)) + #做出不同降雨类型的箱线图
  geom_boxplot() +
  theme_classic()
anova_one_way <- aov(humidity ~ prcp_type, data = data_diff) #进行单因素方差分析，探究在不同降雨量对空气湿度的影响
summary(anova_one_way)

#7.3 探究逐月降雨总量对该流域的月平均流量的影响
data_liner <- unite_data %>%  #处理数据后得到逐月的降雨总量和平均流量数据，
  group_by(monthy) %>%
  summarise(sum_prcp=sum(prcp),mean_q=mean(Q))

plot(x=data_liner$sum_prcp,y=data_liner$mean_q , #作出以月降雨量和月平均流量的散点图
     ylab = 'Monthy Q',
     xlab = 'Prcp(mm/mon)',
     main = "Prcp vs Monthy Q",
     pch = 20,cex = 2,col = "orange")
fit4 <- lm(data_liner$mean_q~data_liner$sum_prcp) #使用简单线性拟合
abline(fit4, lwd = 5, col = "green")
points(mean(data_liner$sum_prcp), mean(data_liner$mean_q), pch = "*", cex = 3,col='red')
summary(fit4) #查看拟合结果，分析二者是否存在线性相关关系














