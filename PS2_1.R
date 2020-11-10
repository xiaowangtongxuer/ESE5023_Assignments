#设定工作目录，加载安装包
setwd('D:/ESEHW#2')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
#1.1 Read .txt文件
data1 <- read.table(file = 'signif.txt',sep='\t',header=TRUE,quote='')
Sig_Eqs <- as_tibble(data1) #将其转化为表格
class(Sig_Eqs)
colnames(data1)
#显示表头，并理解各列含义
#国家： COUNTRY  地震等级:  EQ_PRIMARY
#地震死亡总数 ：DEATHS  地震次数 FLAG_TSUNAMI  
#地震日期 YEAR MONTH DAY 

#1.2 计算每个国家的死亡总数，并输出排名前十国家及各自死亡总数
Sig_Eqs[is.na(Sig_Eqs)] <- 0 #将NA值都变为0，方便求和 
# @MingYANG noticed:
# if you want to get the summation of data except NA, use "na.rm=T" to remove it :
# filter(TotalDeath=sum(DEATHS,na.rm=T))
# the end
every_country_totaldeaths <- Sig_Eqs %>%
  select(COUNTRY, DEATHS) %>%
  group_by(COUNTRY) %>% #根据国家分组
  summarize(DT = sum(DEATHS)) %>% #分别求和，求出各个国家的地震死亡总数
  arrange(desc(DT)) #按死亡总数降序排列
every_country_totaldeaths
every_country_totaldeaths[1:10,]

#1.3 计算每年全球范围内地震等级大于6.0的地震总数，并绘图
YEAR_times <- Sig_Eqs %>% #筛选出地震等级大于6的年份，对每年地震次数加和
  select(FLAG_TSUNAMI, YEAR, COUNTRY, EQ_PRIMARY) %>%
  mutate(times = ifelse(EQ_PRIMARY>=6.0,1,0)) %>%#新建times列用来记数
# @MingYANG recommended:
# three ways will do you some help for counting the qualified data:
# (1) nrow()
# (2) n()
# (3) count()
# the end
  select(YEAR,EQ_PRIMARY,times) %>%
  group_by(YEAR) %>%
  #mutate(total_times = sum(times)) %>%
  summarize(total_times = sum(times)) %>%
  select(YEAR,total_times)

every_year <- data.frame(YEAR=-2150:2020,c=0) #补充没有地震年份的天数的数据，
all_years <- merge(YEAR_times,every_year,by="YEAR",all=TRUE)
all_years$c <- NULL
all_years[is.na(all_years)] <- 0 #将NA值变为0
#将没有记录的年份地震次数记为零，再作图
all_years %>%
  ggplot(aes(x=YEAR, y=total_times)) + 
  geom_line() +
  geom_point(size=1)

#绘制近500年每年地震总数，进一步观察年地震总数变化趋势
plot(all_years$YEAR[3750:4250],all_years$total_times[3750:4250],type = 'o')

#1.4 编写函数
Sig_Eqs[is.na(Sig_Eqs)] <- 0 #将NA值赋值为0
total_times_country <- Sig_Eqs %>% ##得到每个国家地震总数，并按照总数大小排序
  select(FLAG_TSUNAMI, YEAR, MONTH, DAY, COUNTRY, DEATHS, EQ_PRIMARY) %>%
  group_by(COUNTRY) %>%
  mutate(times = 1) %>%
  summarize(total_2150BC = sum(times)) %>%
  arrange(desc(total_2150BC))
total_times_country
maxEq_country <- Sig_Eqs %>% ##得到了每个国家最大地震等级数据
  select(FLAG_TSUNAMI, YEAR, MONTH, DAY, COUNTRY, DEATHS, EQ_PRIMARY) %>%
  group_by(COUNTRY) %>%
  summarize(MAX_Eq = max(EQ_PRIMARY)) 
maxEq_country
all_data_country <- Sig_Eqs %>% ##得到所有会用到的数据 （国家，年月日，地震等级）
  select( YEAR, MONTH, DAY, COUNTRY,EQ_PRIMARY)
all_data_country

#利用上述生成的数据表格，自定义函数，以实现题目要求
CountEq_LargestEq <- function(country_name){
  for(i in 1:nrow(total_times_country)){#第一个for循环，遍历国家
    if (total_times_country[i,1] == country_name ){#找到该国对应的地震总数
      T_time <- total_times_country[i,2]
      print(paste0('地震总数：',T_time))
    }
    if(maxEq_country[i,1] == country_name ){#找到该国家对应的最大地震级数：maxEq_country[i,2]
      for(j in 1:nrow(all_data_country)){#第二个for循环，遍历国家，找最大地震级数对应的日期
        if(all_data_country[j,4] == country_name && #第四五列分别是国家名字和地震级数
           all_data_country[j,5] == maxEq_country[i,2]) #国家和地震级数相等时。输出日期即可
          {
          print(paste0('最大地震级数：',maxEq_country[i,2],',' ,'地震日期：',
                       all_data_country[j,1],'年',all_data_country[j,2],'月',
                       all_data_country[j,3],'日'))
        }
      }
    }
  }
}

CountEq_LargestEq("CHINA") #成功实现

#test
for(i in 1:10){ #遍历地震总数前10的国家，输出各自的地震总数，最大地震等级及其发生的日期
  #若想遍历所有国家，将 for(i in 1:10) 中的10改为国家总数即可
  test_name <- total_times_country[i,1]
  print(paste0(i,'.',test_name,': '))
  CountEq_LargestEq(test_name)
}
# @MingYANG recommended：
# using this way for briefly coding
# unique_country<-unique(Sig_Eqs$COUNTRY)
# for(i in unique_country){
#   a<-as.numeric(CountEq_LargestEq(i)[1])
# }






