# Set working directory
setwd('D:/ESEHW#4')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
library(forecast)
#2.1 生成所需时间序列
data2 <- read.csv(file = '2281305.csv',header=TRUE)
data2 <- data2 %>%
  dplyr::select(DATE,TMP)
data2 <- separate(data = data2, col = TMP, into = c("value","quality"), sep = ",") 
data2 <- separate(data = data2, col = DATE, into = c("date", "time"), sep = "T") 
data2 <- data2 %>%
  filter(date < as.Date("2020-09-01"))
data2 <- separate(data = data2, col = date, into = c("year", "month","day"), sep = "-") 
data2 <- separate(data = data2, col = value, into = c("str","value"), sep = 1) #数据前处理

ts_data <- data2 %>% #质量筛选以及求出实际月平均温度
  filter(quality == 1 ) %>%
  mutate(temp = ifelse(str == '+',
              as.numeric(value)* 0.1 , -0.1 * as.numeric(value))) %>%
  dplyr::group_by(year,month) %>%
  summarise(mean_temp = mean(temp))
data_ts <- ts(ts_data$mean_temp,frequency=12,start=c(2010,1)) #截取开始时间
plot(data_ts, type = "o", main = "Time series of temperature",xlab = 'Year', ylab = 'Mean Temperature(℃)')


#2.2  
#查看时间序列的组成部分
temp_components <- decompose(data_ts)
plot(temp_components) 

#查看时间序列的误差分量的分布
hist(temp_components$random, prob=TRUE,xlim=c(-3,3) , ylim=c(0,0.5),xlab="Random",ylab="Frequency",
     main = "The histogram of temperature compoments random")
curve(dnorm(x, mean=mean(temp_components$random,na.rm=T),
            sd=sd(temp_components$random,na.rm=T)),
      add=TRUE, col="red")

#2.3  拟合ARIMA(p,d,q) 时间序列模型

tsdisplay(data_ts,main= "Temperature") #查看原始数据的ACF和PACF

#去除周期性影响后的ACF和PACF（减去周期性的分量）
data_adjusted<-data_ts-temp_components$seasonal
plot(data_adjusted,main= "The data after masking the seasonal component")
tsdisplay(data_adjusted,main= "The data after masking the seasonal component",ylab="Temperature")

data_D1=diff(data_ts,12) #周期为12个月，进行差分（）差分法
tsdisplay(data_D1,main= "The data after diffing")
temp_components <- decompose(data_D1)
plot(temp_components) #展示差分后的时间序列组成部分，判断周期性影响是否被去除

#手动拟合模型，根据AIC最小来确定最优模型
model <- Arima(data_ts,order=c(0,0,0),seasonal=c(1,1,1))
aicmin = model$aic
pbest = 0
qbest = 0
for (p in 0:3){
  for(q in 0:3){
    model <- Arima(data_ts,order=c(p,0,q),seasonal=list(order=c(1,1,1),period=12))
    print(model$aic)
    if(model$aic < aicmin){
      aicmin = model$aic
      pbest = p
      qbest = q
    }
  }
}
model <- arima(data_ts,order=c(pbest,0,qbest),seasonal=list(order=c(1,1,1),period=12))
#自动拟合模型结果
model_auto <- auto.arima(data_ts,trace=T)
#比较后得到：
best_model <- model_auto

#2.4 预测以及验证
days_forecast  <- 12
days_in_plot   <- 36
plot(forecast(best_model, days_forecast), include = days_in_plot, xlab="Time", 
     ylab="Temperature",type="o",lwd=2) 

dataobs <- read.csv(file = '2281305obs.csv',header=TRUE)
dataobs <- dataobs %>%
  dplyr::select(DATE,TMP)
dataobs <- separate(data = dataobs, col = TMP, into = c("value","quality"), sep = ",") 
#日期时间列分割开
dataobs <- separate(data = dataobs, col = DATE, into = c("date", "time"), sep = "T") 
dataobs <- dataobs %>%
  filter(date < as.Date("2020-12-01"))
dataobs <- separate(data = dataobs, col = date, into = c("year", "month","day"), sep = "-") 
dataobs <- separate(data = dataobs, col = value, into = c("str","value"), sep = 1)
dataobs <- dataobs %>%
  filter(quality == 1 ) %>%
  mutate(temp = ifelse(str == '+',
                       as.numeric(value)* 0.1 , -0.1 * as.numeric(value))) %>%
  dplyr::group_by(year,month) %>%
  summarise(mean_temp = mean(temp))

forecast(best_model, 2)




