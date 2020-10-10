#7.1 导入文件河北漳卫河流域2000~2019年逐月平均温度
setwd("D:/R-HW1")
tdata <- read.csv(file = "Temperature.csv", header = T)
colnames(tdata) 
year <- tdata$year.mon
t1  <- tdata$p1 #探究河北漳卫河流域P1处温度的变化情况
#clean data
t1  <- t1*0.1 #发现数据中温度的单位为0.1℃，将单位变为1℃
t1[which(t1 > 40)]  <- NA
t1[which(t1 < -20)]  <- NA#clean possible data points with missing values or bad quality.
#7.2 绘制p1点近年来温度的时间序列图
year <- as.POSIXlt(year)
plot_year <- function(n){#定义一个函数可以plot任意年份的年度温度变化图
  i1=1+(n-2000)*12
  i2=12+(n-2000)*12
  plot(mon[i1:i2],t1[i1:i2],type = 'o',xlab = n,ylab = 'T(℃)')
}

plot_year(2000)

plot(year,t1,type = 'l',xlab = 'year',ylab = 'T(℃)')

#7.3 分析数据的均值，范围等
check_year <- function(n){#定义一个检查数据的函数
  i1=1+(n-2000)*12
  i2=12+(n-2000)*12
  m1=max(t1[i1:i2])
  m2=min(t1[i1:i2])
  m3=round(mean(t1[i1:i2]),2)
  m4=round(median(t1[i1:i2]),)
  m5 <- range(t1[i1:i2])
  print(paste0(n,'年最高温度:',m1,'℃，最低温度:',m2,'℃'))
  print(paste0('平均温度:',m3,'℃，中值温度:',m4,'℃，温度范围:',m5[1],'~',m5[2],'℃。'))
}

check_year(2012)
#test
for(i in 2000:2019){#分别检查2016~2019年的数据，并输出
  check_year(i)
}

#报告发现
