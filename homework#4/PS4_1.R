# Set working directory
setwd('D:/ESEHW#4')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
#׼������
data <- read.csv(file = 'xinxiangdata.csv',header=TRUE)
data <- separate(data = data, col = date,
                  into = c("year", "month","day"), sep = "/",remove = FALSE) 
data1 <- data %>% 
  dplyr::select(date,year,month,day,prcp,temp)
#1.1 Box plot �鿴��2019�겻ͬ�·��¶ȵķֲ�
data1 %>%
  filter(year == 2019) %>%
  #����ͼ���Լ�����������ƺ�˳��
  ggplot(aes(x=factor(month,level = c('1','2','3','4','5','6','7',
                                  '8','9','10','11','12')),
             y=temp, 
             fill = factor(month,level = c('1','2','3','4','5','6','7',
                                          '8','9','10','11','12')))) + #������ͼ������ֵ�ͷֲ����
  stat_boxplot(geom='errorbar',width=0.5) +  
  geom_boxplot(outlier.shape=23, outlier.size=3 ,
               notch=FALSE,notchwidth=0.5) + #��ע�쳣ֵ
  geom_point() +
  theme_classic() +
  theme(axis.title.y = element_text(size = 15),#����ͼ��λ�ã�������͸������λ��
        axis.title.x = element_text(size = 15),
        panel.border = element_rect(color ="black",fill = NA,size = 1.5),
        plot.title = element_text(hjust = 0,size = 25 ),
        plot.subtitle = element_text(hjust = 0.5 , size = 15),
        plot.caption = element_text(size=12),
        legend.position = c(0.99, 0.99),
        legend.justification = c(0.99, 0.99)) + 
  stat_summary(fun='mean',geom='point',shape=21,size=3,fill='orange') + #������ͼ���Ӿ�ֵ
  labs(title = 'Boxplot', #��ע�����Լ����������ƣ�����������Դ��Ϣ
       subtitle="Temperature distribution chart in different months in 2019",
       caption="Source:  http://data.cma.cn",fill = 'Month',
       x="Month",y="Temperature(��)")  +
  ylim(-10,40)

#1.2 time series ��ʮ����ƽ��ƽ�����µı仯�����ʱ�����У�
data_temp <- read.csv(file = 'temperature.csv',header=TRUE)
data_temp  <- separate(data = data_temp, col = date, into = c("year", "month","day"), sep = "/")
data_temp <- data_temp %>%
  dplyr::select(year,month,xinxiang) %>%
  mutate(year = as.numeric(year),month = as.numeric(month)) %>% #��ֹgroup_byʱ����
  group_by(year,month) %>%
  summarise(xinxiang_temp = mean(xinxiang))
temp <- ts(data_temp$xinxiang_temp,frequency=12,start=c(2010,1))
plot(temp, type = "o", main = "Temperature change in different years",col = 'orange',xlab = 'Year', ylab = 'Temperature(��)')








#1.3 histogram  ��ʮ��Ľ����ܴ����Լ�����С�������ռ��
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

#1.4  scatter plot ������ͬ����վ�꽵�������Ĳ�ͬ����С��ʾ��������
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


