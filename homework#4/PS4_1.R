# Set working directory
setwd('D:/ESEHW#4')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
#准备数据
data <- read.csv(file = 'xinxiangdata.csv',header=TRUE)
data <- separate(data = data, col = date,
                  into = c("year", "month","day"), sep = "/",remove = FALSE) 
data1 <- data %>% 
  dplyr::select(date,year,month,day,prcp,temp)
#1.1 Box plot 查看近2019年不同月份温度的分布
data1 %>%
  filter(year == 2019) %>%
  #设置图例以及横坐标的名称和顺序
  ggplot(aes(x=factor(month,level = c('1','2','3','4','5','6','7',
                                  '8','9','10','11','12')),
             y=temp, 
             fill = factor(month,level = c('1','2','3','4','5','6','7',
                                          '8','9','10','11','12')))) + #做箱型图分析均值和分布情况
  stat_boxplot(geom='errorbar',width=0.5) +  
  geom_boxplot(outlier.shape=23, outlier.size=3 ,
               notch=FALSE,notchwidth=0.5) + #标注异常值
  geom_point() +
  theme_classic() +
  theme(axis.title.y = element_text(size = 15),#设置图例位置，主标题和副标题的位置
        axis.title.x = element_text(size = 15),
        panel.border = element_rect(color ="black",fill = NA,size = 1.5),
        plot.title = element_text(hjust = 0,size = 25 ),
        plot.subtitle = element_text(hjust = 0.5 , size = 15),
        plot.caption = element_text(size=12),
        legend.position = c(0.99, 0.99),
        legend.justification = c(0.99, 0.99)) + 
  stat_summary(fun='mean',geom='point',shape=21,size=3,fill='orange') + #给箱型图添加均值
  labs(title = 'Boxplot', #标注标题以及坐标轴名称，添加数据来源信息
       subtitle="Temperature distribution chart in different months in 2019",
       caption="Source:  http://data.cma.cn",fill = 'Month',
       x="Month",y="Temperature(℃)")  +
  ylim(-10,40)

#1.2 time series 近十年月平均平均气温的变化情况（时间序列）
data_temp <- read.csv(file = 'temperature.csv',header=TRUE)
data_temp  <- separate(data = data_temp, col = date, into = c("year", "month","day"), sep = "/")
data_temp <- data_temp %>%
  dplyr::select(year,month,xinxiang) %>%
  mutate(year = as.numeric(year),month = as.numeric(month)) %>% #防止group_by时乱序
  group_by(year,month) %>%
  summarise(xinxiang_temp = mean(xinxiang))
temp <- ts(data_temp$xinxiang_temp,frequency=12,start=c(2010,1))
plot(temp, type = "o", main = "Temperature change in different years",col = 'orange',xlab = 'Year', ylab = 'Temperature(℃)')








#1.3 histogram  近十年的降雨总次数以及大中小暴雨各自占比
data1 %>%
  filter(prcp > 0) %>%
  mutate(class = ifelse(prcp < 10, "Light rain", ifelse(prcp < 25, "Mederate rain", ifelse(prcp < 50,'Heavy rain','Rainstorm')))) %>%
  #group_by(year) %>%
  ggplot(aes(year)) + 
  scale_fill_brewer(palette="PiYG") +
  geom_bar(aes(fill=factor(class,level = list('Rainstorm','Heavy rain','Mederate rain','Light rain'))), width = 0.5) + 
  geom_text(aes(label=..count..),stat = 'count',vjust=-0.5) +
  theme(axis.text.x = element_text(angle=0, vjust=0.6,size = 12),
        axis.text.y = element_text(angle=0, vjust=0.5 ,size = 12)) + 
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        panel.border = element_rect(color ="black",fill = NA,size = 1.2),
        plot.title = element_text(hjust = 0,size = 25 ),
        plot.subtitle = element_text(hjust = 0.5 , size = 15),
        plot.caption = element_text(size=12),
        legend.position = c(0.99, 0.99),
        legend.justification = c(0.99, 0.99)) +
  labs(title = 'Histogram',
       subtitle="The total numbers of different types of rainfall in different years",
       caption="Source: http://data.cma.cn",fill = 'Type',
       x="Year",y="Count") +
  ylim(0,122)

data1 %>%
  filter(prcp > 0) %>%
  mutate(class = ifelse(prcp < 10, "Light rain", 
                        ifelse(prcp < 25, "Mederate rain", 
                               ifelse(prcp < 50,'Heavy rain','Rainstorm')))) %>%
  group_by(year,class) %>%
  summarise(total_prcp = sum(prcp)) %>%
  ggplot(aes(x=year,y=total_prcp, 
        fill=factor(class,level = list('Rainstorm',
                          'Heavy rain','Mederate rain','Light rain')))) +
  geom_bar(stat="identity", width = 0.5,aes(fill=factor(class,
                                  level = list('Rainstorm','Heavy rain',
                                        'Mederate rain','Light rain')))) +
  scale_fill_brewer(palette="PiYG") +
  geom_text(aes(label = total_prcp), position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_text(vjust=0.5)) + 
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        panel.border = element_rect(color ="black",fill = NA,size = 1.2),
        plot.title = element_text(hjust = 0,size = 25 ),
        plot.subtitle = element_text(hjust = 0.5 , size = 15),
        plot.caption = element_text(size=12),
        legend.position = c(0.99, 0.99),
        legend.justification = c(0.99, 0.99)) +
  labs(title = 'Histogram',
       subtitle="The total rainfall of different types in different years",
       caption="Source: http://data.cma.cn",fill = 'Type',
       x="Year",y="Precipitation(mm/year)") 

#1.4  scatter plot 两个不同气象站年降雨总量的不同，大小表示降雨总量
data <- read.csv(file = 'xinxiangdata.csv',header=TRUE)
data <- separate(data = data, col = date,
                 into = c("year", "month","day"), sep = "/") 
xinxiang <- data %>% 
  dplyr::select(year,month,day,prcp,temp) %>%
  group_by(year) %>%
  summarise(prcp = sum(prcp),type = 'Xin xiang')
data <- read.csv(file = 'anyangdata.csv',header=TRUE)
data <- separate(data = data, col = date,
                 into = c("year", "month","day"), sep = "/") 
anyang <- data %>% 
  dplyr::select(year,month,day,prcp,temp) %>%
  group_by(year) %>%
  summarise(prcp = sum(prcp),type = 'An yang')
both_data <- rbind(anyang,xinxiang)
both_data %>%
  ggplot( aes(x=year, y=prcp, color=type, shape=type ,size = prcp) ) + 
  geom_point() + 
  labs(title="Scatter plot", x="Year", 
       y="Precipatation(mm/year)",shape = 'Weather station',size = 'Precipatation',
       subtitle="The scatter plot of the total annual rainfall of two weather stations",
       caption="Source: http://data.cma.cn") +
  theme(axis.text.x = element_text(vjust=0.5)) + 
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        panel.border = element_rect(color ="black",fill = NA,size = 1.2),
        plot.title = element_text(hjust = 0,size = 25 ),
        plot.subtitle = element_text(hjust = 0.5 , size = 15),
        legend.position = c(0.99, 0.99),
        legend.justification = c(0.99, 0.99)) +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=15), 
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20),
        plot.caption = element_text(size=12)) + 
  scale_color_discrete(name="Color")

#1.5 image plot RS NDVI data
library(sp)
library(rgdal)
library(raster)
library(viridis)
# Read tiff file
NDVI <- raster("0101.tif")
# Look at the raster attributes
Crop_box <- c(0,208,0,183)
NDVI <- crop(NDVI, Crop_box)
plot(NDVI,  main="NDVI values in Feb", col = viridis(6), 
     breaks = c(0,0.005,0.1,0.2,0.25,0.3,1),
      xlab = "E-W", ylab="N-S",xlim=c(0,208) , ylim=c(0,183))



