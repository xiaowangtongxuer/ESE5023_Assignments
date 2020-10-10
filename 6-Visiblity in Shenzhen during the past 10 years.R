#6.1 ����ʱ������ͼ��
#read csv
setwd("D:/R-HW1")
hwdata <- read.csv(file = "HW#6.csv", header = T)
colnames(hwdata) 
#load data
x <- hwdata$DATE.T
x <- as.POSIXlt(x)#��xת��Ϊ���ڸ�ʽ����
y <- hwdata$VIS
#clean data
y <- strsplit(y,split = ',') #������y�����ݰ��ն��ŷָ�Ϊ�б�
y <- do.call(rbind,y) #���б�ת��Ϊ���ò������ַ�����
nrow(y)
for(i in 1:nrow(y)){#�������ݣ�ȥ�����ϸ������
  if (y[i,3] != 'N' || y[i,2] != '1' || y[i,4] != '1')
  {
    y[i,1] <- NA
  }
}
y=apply(y,2,as.numeric)#����������ַ�����ת��Ϊ��ֵ����
y[,1][which(y[,1] > 160000)]  <- NA#��y[,1]�е����д���160000��ֵ�޳�
# plot
plot(x,y[,1],type='l',xlab="Year",ylab="Visibility [m]")
#��Ϊ��2013��֮�����ݲ�����Ҫ�󣬶���ɸ���ˣ�

#6.2 �����ɼ��ȵı仯����
#�ֱ��ʱ�Ϊÿ�죬�������ٴδ���
setwd("D:/R-HW1")
hwdata <- read.csv(file = "HW#6.csv", header = T)
x <- data$DATE.T
x <- as.POSIXlt(x)#ת��Ϊ��������
xx <- as.Date(x,"%Y%m%d")#�ٴ�ͳһ��ʽ
#xx
day <- as.numeric(xx)-14610#����ʽת��Ϊ��ֵ�ͣ����Ұ�������ʼ������Ϊ��һ��
#day

y2 <- hwdata$VIS

#clean data
y2 <- strsplit(y2,split = ",") #������y�����ݰ��ն��ŷָ�Ϊ�б�
y2 <- do.call(rbind,y2) #���б�ת��Ϊ���ò������ַ�����
y2=apply(y2,2,as.numeric)#����������ַ�����ת��Ϊ��ֵ����
y2[,1][which(y2[,1] > 160000)]  <- 0 #��y[,1]�е����д���160000��ֵ�޳�

j=1
day_m = 1 #��ע�������ֵ
day_max <- vector()
day_max #��עΪ���տɼ�����������������������һ��

y2[,1]
#2010���ڣ���1.2��12.31����364��
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
    #print(j)  #����j�������Ƿ���ȣ�
    #print(day_max[j-1])  �Ӷ���֤���ݵ�׼ȷ��
    j=1
  }
}

##����ʱ��������ѭ��û�ж������һ������ݣ����¸�ֵ
max <- function(ma){#max��������Ϊ�������������е����ֵ
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


#����һ����ӡ��ͬ�ɼ��������ĺ���
show_day <- function(year,start_day,end_day){ #�����ֱ�����ݣ���ʼ�죬������
  t1=t2=t3=t4=t5=t6=t7=0#�ֱ��¼һ���в�ͬ�ܼ��ȵ�����
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
      print(paste0(year,'�깲��¼��',t8,'��Ŀɼ�������'))
      print('���и����ɼ��ȵ������ֱ�Ϊ��')
      print(paste0('[ 0km, 5km) : ',t1,' days'))
      print(paste0('[ 5km,10km) : ',t2,' days'))
      print(paste0('[10km,15km) : ',t3,' days'))
      print(paste0('[15km,20km) : ',t4,' days'))
      print(paste0('[20km,25km) : ',t5,' days'))
      print(paste0('[25km,30km) : ',t6,' days'))
      print(paste0('[30km,99km) : ',t7,' days'))
      print(paste0(year,"���ܼ��ȴ���20km��������",t5+t6+t7))
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