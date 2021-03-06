setwd('D:/ESEHW#2') 
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
#2
data2 <- read.csv(file = '2281305.csv',header=TRUE)#读入数据
colnames(data2)
#将WND分裂开，得到不同的数据列
data2 <- separate(data = data2, col = WND, into = c("direction", 
                  "direction_quality","type","speed","speed_quality"), sep = ",") 
#日期时间列分割开
data2 <- separate(data = data2, col = DATE, into = c("date", "time"), sep = "T") 
#分为年月日，便于求解月平均风速
data2 <- separate(data = data2, col = date, into = c("year", "month","day"), sep = "-") 

colnames(data2)
wind_data <- data2 %>%
  select(year,month,day,speed,speed_quality)
typeof(wind_data)

wind_data_day <- wind_data %>%
  mutate(day_mean = 15)
wind_data <- unite(wind_data_day, "monthy", year, month,day_mean, sep = "-", remove = FALSE)
wind_data  %>%
  select(year,monthy,speed,speed_quality) %>%
  filter(speed_quality == '0' | speed_quality == '1' | speed_quality == '4' | speed_quality == '5' ) %>%
  group_by(monthy) %>%
  summarize(mean_speed = mean(as.numeric(speed))) %>%
  ggplot(aes(x=as.Date(monthy,format='%Y-%m-%d'), y=as.numeric(mean_speed))) + 
  labs(x='Year',y='Mean_Wind(m/s)') +
  geom_point() +
  geom_line()

# @MingYANG recommended：
# try this method:
# a_Data       <- read.csv(file = "2281305.csv", header = T)
# a_Data <- as_tibble(a_Data)
# a_Data%>% 
#  select(WND,DATE) %>% 
#  filter(substr(WND,1,3)!="999") %>% 
#  filter(substr(WND,9,12)!="9999") %>% 
#  filter(substr(WND,5,7)=="1,N")%>%
#  filter(substr(WND,14,14)=="1")%>%
#  mutate(Month=as.character(paste0(substr(DATE,1,4),"-",substr(DATE,6,7)))) %>%
#  mutate(wind_speed=as.numeric(substr(WND,9,12)))%>%
#  select(Month,wind_speed) %>% 
#  group_by(Month) %>% 
#  summarise(windspeed_month=0.1*mean(wind_speed,na.rm = T)) %>% 
#  mutate(month = as.Date(paste0(Month,"-","15"))) %>%
#  ggplot(aes(x=month, y=windspeed_month)) + 
#  geom_line()
# the end
