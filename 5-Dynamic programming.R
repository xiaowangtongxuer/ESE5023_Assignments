#5.1
#穷举法 在123456789内插入“ ” “+” “-” 三种操作符，因此有 3^8 种情况
#直接将3^8转换成3进制的数储存起来，放入一个6561*8的矩阵里面
#矩阵的每一行就代表了一种运算方案
#然后进行6561次循环，用矩阵每一行的8个数对123456789逐一处理，0.1.2分别代表不加符号/-号/+号
#用str来记录对处理后的总表达式
#然后用eval(parse(text=str))函数计算得到sum值
#对每个sum值和目标值对比，如果相等，则输出对应的等式
Find_expression <- function(k)
{
  m_operator <- matrix(data=0,nrow = 6561,ncol = 8)#定义用来储存操作符的矩阵
  for (i in 1:6561){#将1：6561化为3进制的数储存到矩阵中，从右往左储存。
    #不满8位的其余位置用0赋值
    zh=i
    for(j in 8:1){
      m_operator[i,j]=zh%%3 #十进制转化为3进制即可
      zh =floor(zh/3)
      if(zh == 0 ){
        break
      }
    }
  }
  t=0
  for(i in 1:6561){#将见矩阵记录好的运算符插入到123456789中
    #0，1和2分别代表不插符号，减号和加号
    str <- '1'
    for(j in 1:8){#用字符串str来记录123456789之间的操作符号
      if (m_operator[i,j]==2){
        str=paste0(str,'+',j+1)#用paste0 函数来从1到9遍历，并形成所需要的字符串str
      }
      if(m_operator[i,j] == 1){
        str=paste0(str,'-',j+1)
      }
      if(m_operator[i,j]==0){
        str=paste0(str,j+1)
      }
    }
    sum <- eval(parse(text=str)) #用该函数来计算str的值，赋值给sum
    #print(nchar(str))
    #print(str)
    #print(sum)
    if(sum==k){#每生成一个sum都和目标值比较，若相等，则输出表达式
      t=t+1
      print(paste('第',t,'个：',str,'=',k))
    }
  }
  #计算str代表的算术式的值
}

#test
Find_expression(50)
Find_expression(100)

#5.2
Total_solutions <- vector(length=100)
Find_solution <- function(n){#重新定义了个函数，返回值变为满足条件数的个数
  #主体部分和5.1中Find_expression（）相同，只是在return处做了修改
  m_operator <- matrix(data=0,nrow = 6561,ncol = 8)
  for (i in 1:6561){
    zh=i
    for(j in 8:1){
      m_operator[i,j]=zh%%3
      zh =floor(zh/3)
      if(zh == 0 ){
        break
      }
    }
  }
  m_operator
  t=0
  for(i in 1:6561){
    str <- '1'
    for(j in 1:8){
      if (m_operator[i,j]==2){
        str=paste0(str,'+',j+1)
      }
      if(m_operator[i,j] == 1){
        str=paste0(str,'-',j+1)
      }
      if(m_operator[i,j]==0){
        str=paste0(str,j+1)
      }
    }
    sum <- eval(parse(text=str))
    #print(nchar(str))
    #print(str)
    #print(sum)
    if(sum==n){#记录sum与目标值相等的次数，用t来记录
      t=t+1
    }
  }
  if (i==6561){#执行最后一次循环时，返回t值
    return(t)
  }
}

#test 测试1-100内各个数的满足条件的个数
for (i in 1:100){
  Total_solutions[i] = Find_solution(i)
  #print(paste0(i,'->',Total_solutions[i]))
}


max=1
min=100
for(i in 1:100){#遍历Find_solution(1~100），并记录其中满足条件数的最大值和最小值
  if(Total_solutions[i]>max){
    max=Total_solutions[i]
  }
  if(Total_solutions[i]<min){
    min=Total_solutions[i]
  }
}
ma=mi=0
ma_num <- vector()
mi_num <- vector()
for(i in 1:100){#记录Total_solutions[1~100]中等于max和min值的数，然后输出
  if(Total_solutions[i]==max){
    ma=ma+1
    ma_num[ma]=i
  }
  if(Total_solutions[i]==min){
    mi=mi+1
    print(mi)
    mi_num[mi]=i
  }
}

#plot
plot(seq(1,100,1),Total_solutions,type="o",
     xlab="number",ylab="solutions")

{print(paste0('max = ',max,' and the number is: ',ma_num[1],', ',ma_num[2]))
print(paste0('min = ',min,' and the number is: ',mi_num[1]))}

