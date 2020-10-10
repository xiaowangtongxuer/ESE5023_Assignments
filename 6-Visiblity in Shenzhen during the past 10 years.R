#6.1 绘制时间序列图表
#read csv
setwd("D:/R-HW1")
hwdata <- read.csv(file = "HW#6.csv", header = T)
colnames(hwdata) 
#load data
x <- hwdata$DATE.T
x <- as.POSIXlt(x)#将x转化为日期格式数据
y <- hwdata$VIS
#clean data
y <- strsplit(y,split = ',') #将导入y的数据按照逗号分割为列表
y <- do.call(rbind,y) #将列表转化为更好操作的字符矩阵
nrow(y)
for(i in 1:nrow(y)){#整理数据，去除不合格的数据
  if (y[i,3] != 'N' || y[i,2] != '1' || y[i,4] != '1')
  {
    y[i,1] <- NA
  }
}
y=apply(y,2,as.numeric)#将处理后的字符矩阵转换为数值矩阵
y[,1][which(y[,1] > 160000)]  <- NA#将y[,1]中的所有大于160000的值剔除
# plot
plot(x,y[,1],type='l',xlab="Year",ylab="Visibility [m]")
#因为在2013年之后数据不符合要求，都被筛掉了！

#6.2 分析可见度的变化趋势
#分辨率变为每天，对数据再次处理
setwd("D:/R-HW1")
hwdata <- read.csv(file = "HW#6.csv", header = T)
x <- data$DATE.T
x <- as.POSIXlt(x)#转化为日期数据
xx <- as.Date(x,"%Y%m%d")#再次统一格式
#xx
day <- as.numeric(xx)-14610#将格式转化为数值型，并且把数据起始日期作为第一天
#day

y2 <- hwdata$VIS

#clean data
y2 <- strsplit(y2,split = ",") #将导入y的数据按照逗号分割为列表
y2 <- do.call(rbind,y2) #将列表转化为更好操作的字符矩阵
y2=apply(y2,2,as.numeric)#将处理后的字符矩阵转换为数值矩阵
y2[,1][which(y2[,1] > 160000)]  <- 0 #将y[,1]中的所有大于160000的值剔除

j=1
day_m = 1 #标注单天最大值
day_max <- vector()
day_max #标注为单日可见度最大的向量，和天数长度一致

y2[,1]
#2010年内，从1.2到12.31，共364天
for(i in 1:111984)
{
  if(day[i]==j && i !=111984)
  {
    if(y2[i,1] > day_m)
    {
      day_m = y2[i,1]
    }
    next
  }
  if (day[i] != j)
  {
    
    day_max[j] = day_m
    j=j+1
    day_m =0
  }
  if(i == 111984){
    #print(j)  #测试j与天数是否相等，
    #print(day_max[j-1])  从而验证数据的准确性
    j=1
  }
}

##运行时发现上面循环没有定义最后一天的数据，重新赋值
max <- function(ma){#max函数定义为求解输入的向量中的最大值
  t=0
  len=length(ma)
  print(len)
  for(i in 1:len)
  {
    if(ma[i] > t){
      t = ma[i]
    }
  }
  return(t)
}

day_max[3906] = max(y2[111963:111984,1])
day_max[3906]


#定义一个打印不同可见度天数的函数
show_day <- function(year,start_day,end_day){ #参数分别是年份，开始天，结束天
  t1=t2=t3=t4=t5=t6=t7=0#分别记录一年中不同能见度的天数
  for(i in start_day:end_day){
    if(day_max[i]< 5000){
      t1 = t1+1
    }
    else if(day_max[i]>=5000 && day_max[i] < 10000){
      t2 = t2+1
    }
    else if(day_max[i]>=10000 && day_max[i] < 15000){
      t3 = t3+1
    }
    else if(day_max[i]>=15000 && day_max[i] < 20000){
      t4 = t4+1
    }
    else if(day_max[i]>=20000 && day_max[i] < 25000){
      t5 = t5+1
    }
    else if(day_max[i]>=25000 && day_max[i] < 30000){
      t6 = t6+1
    }
    else if(day_max[i]>=30000 ){
      t7 = t7+1
    }
    if(i==end_day){
      t8=t1+t2+t3+t4+t5+t6+t7
      print(paste0(year,'年共记录了',t8,'天的可见度数据'))
      print('其中各个可见度的天数分别为：')
      print(paste0('[ 0km, 5km) : ',t1,' days'))
      print(paste0('[ 5km,10km) : ',t2,' days'))
      print(paste0('[10km,15km) : ',t3,' days'))
      print(paste0('[15km,20km) : ',t4,' days'))
      print(paste0('[20km,25km) : ',t5,' days'))
      print(paste0('[25km,30km) : ',t6,' days'))
      print(paste0('[30km,99km) : ',t7,' days'))
      print(paste0(year,"年能见度大于20km的天数：",t5+t6+t7))
      t1=t2=t3=t4=t5=t6=t7=0
    }
  }
}

  {show_day(2010,1,364)
  show_day(2011,365,729)
  show_day(2012,730,1095)
  show_day(2013,1096,1460)
  show_day(2014,1461,1825)
  show_day(2015,1826,2190)
  show_day(2016,2191,2556)
  show_day(2017,2557,2921)
  show_day(2018,2922,3286)
  show_day(2019,3287,3651)
  show_day(2020,3652,3906)}
