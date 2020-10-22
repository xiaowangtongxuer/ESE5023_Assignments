#�趨����Ŀ¼�����ذ�װ��
setwd('D:/ESEHW#2')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
#1.1 Read .txt�ļ�
data1 <- read.table(file = 'signif.txt',sep='\t',header=TRUE,quote='')
Sig_Eqs <- as_tibble(data1) #����ת��Ϊ����
class(Sig_Eqs)
colnames(data1)
#��ʾ��ͷ����������к���
#���ң� COUNTRY  ����ȼ�:  EQ_PRIMARY
#������������ ��DEATHS  ������� FLAG_TSUNAMI  
#�������� YEAR MONTH DAY 

#1.2 ����ÿ�����ҵ��������������������ǰʮ���Ҽ�������������
Sig_Eqs[is.na(Sig_Eqs)] <- 0 #��NAֵ����Ϊ0���������
every_country_totaldeaths <- Sig_Eqs %>%
  select(COUNTRY, DEATHS) %>%
  group_by(COUNTRY) %>% #���ݹ��ҷ���
  summarize(DT = sum(DEATHS)) %>% #�ֱ���ͣ�����������ҵĵ�����������
  arrange(desc(DT)) #������������������
every_country_totaldeaths
every_country_totaldeaths[1:10,]

#1.3 ����ÿ��ȫ��Χ�ڵ���ȼ�����6.0�ĵ�������������ͼ
YEAR_times <- Sig_Eqs %>% #ɸѡ������ȼ�����6����ݣ���ÿ���������Ӻ�
  select(FLAG_TSUNAMI, YEAR, COUNTRY, EQ_PRIMARY) %>%
  mutate(times = ifelse(EQ_PRIMARY>=6.0,1,0)) %>%#�½�times����������
  select(YEAR,EQ_PRIMARY,times) %>%
  group_by(YEAR) %>%
  #mutate(total_times = sum(times)) %>%
  summarize(total_times = sum(times)) %>%
  select(YEAR,total_times)

every_year <- data.frame(YEAR=-2150:2020,c=0) #����û�е�����ݵ����������ݣ�
all_years <- merge(YEAR_times,every_year,by="YEAR",all=TRUE)
all_years$c <- NULL
all_years[is.na(all_years)] <- 0 #��NAֵ��Ϊ0
#��û�м�¼����ݵ��������Ϊ�㣬����ͼ
all_years %>%
  ggplot(aes(x=YEAR, y=total_times)) + 
  geom_line() +
  geom_point(size=1)

#���ƽ�500��ÿ�������������һ���۲�����������仯����
plot(all_years$YEAR[3750:4250],all_years$total_times[3750:4250],type = 'o')

#1.4 ��д����
Sig_Eqs[is.na(Sig_Eqs)] <- 0 #��NAֵ��ֵΪ0
total_times_country <- Sig_Eqs %>% ##�õ�ÿ�����ҵ���������������������С����
  select(FLAG_TSUNAMI, YEAR, MONTH, DAY, COUNTRY, DEATHS, EQ_PRIMARY) %>%
  group_by(COUNTRY) %>%
  mutate(times = 1) %>%
  summarize(total_2150BC = sum(times)) %>%
  arrange(desc(total_2150BC))
total_times_country
maxEq_country <- Sig_Eqs %>% ##�õ���ÿ������������ȼ�����
  select(FLAG_TSUNAMI, YEAR, MONTH, DAY, COUNTRY, DEATHS, EQ_PRIMARY) %>%
  group_by(COUNTRY) %>%
  summarize(MAX_Eq = max(EQ_PRIMARY)) 
maxEq_country
all_data_country <- Sig_Eqs %>% ##�õ����л��õ������� �����ң������գ�����ȼ���
  select( YEAR, MONTH, DAY, COUNTRY,EQ_PRIMARY)
all_data_country

#�����������ɵ����ݱ����Զ��庯������ʵ����ĿҪ��
CountEq_LargestEq <- function(country_name){
  for(i in 1:nrow(total_times_country)){#��һ��forѭ������������
    if (total_times_country[i,1] == country_name ){#�ҵ��ù���Ӧ�ĵ�������
      T_time <- total_times_country[i,2]
      print(paste0('����������',T_time))
    }
    if(maxEq_country[i,1] == country_name ){#�ҵ��ù��Ҷ�Ӧ������������maxEq_country[i,2]
      for(j in 1:nrow(all_data_country)){#�ڶ���forѭ�����������ң�������������Ӧ������
        if(all_data_country[j,4] == country_name && #�������зֱ��ǹ������ֺ͵�����
           all_data_country[j,5] == maxEq_country[i,2]) #���Һ͵��������ʱ��������ڼ���
          {
          print(paste0('����������',maxEq_country[i,2],',' ,'�������ڣ�',
                       all_data_country[j,1],'��',all_data_country[j,2],'��',
                       all_data_country[j,3],'��'))
        }
      }
    }
  }
}

CountEq_LargestEq("CHINA") #�ɹ�ʵ��

#test
for(i in 1:10){ #������������ǰ10�Ĺ��ң�������Եĵ���������������ȼ����䷢��������
  #����������й��ң��� for(i in 1:10) �е�10��Ϊ������������
  test_name <- total_times_country[i,1]
  print(paste0(i,'.',test_name,': '))
  CountEq_LargestEq(test_name)
}






