setwd('D:/ESEHW#3')
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
library(MASS)
data(cpus)
str(cpus)
#6.1
sample_index <- sample(nrow(cpus),nrow(cpus)*0.80) #数据分集
cpus_train <- cpus[sample_index,]
cpus_test  <- cpus[-sample_index,]
model_1 <- lm(perf ~ syct+ mmin + mmax + cach +
                    chmin + chmax, data=cpus_train) #最佳子集回归方法
model_2=lm(perf ~ 1, data=cpus_train)
model_step_b <- step(cpus_train,direction='backward')  #backward 方法
model_step_f <- step(model_2, scope=list(lower=model_2, upper=model_1),
                     direction='forward')# forward 方法
model_step_s <- step(model_2, scope=list(lower=model_2, upper=model_1),
                     direction='both') # stepwise regression 方法
summary(model_step_f)
summary(model_step_s)

#6.2



