#2.1 生成矩阵M1和M2
#用sample函数生成两组0-50时之间的随机数，每组各五十个数
c1 <- sample(0:50,50)
c2 <- sample(0:50,50)
M1 <- matrix(data=c1,nrow=5,ncol=10)#M1,M2即为满足要求的矩阵
M2 <- matrix(data=c2,nrow=10,ncol=5)
M1
M2
#2.2 定义Matrix_multip函数，计算M1*M2
c3 <- vector(length=25)#用于记录对应行列相乘后的值
Matrix_multip <- function(ma,mb){
  ti=1
  for(k in 1:5 ) {#此处也可将5/5/10修改为输入矩阵对应的行列数
    for (i in 1:5)
      {
      for (j in 1:10)
      {
        sum=sum+M1[i,j]*M2[j,k]#sum用来记录每组M1的行元素和M2的列元素的乘积之和
        #每次for循环后，sum会记录十个乘积之和
      }
      c3[ti]=sum#c3 用来记录每个sum值，并用ti来记录次序
      sum=0#对sum重新初始化
      # here is a suggestion to initialize parameters at the beginning before using it,sometimes you don`t know what the compiler will throw to you
      ti=ti+1 #次序递增
    }
  }
  M3 <- matrix(data=c3[1:25],nrow=5,ncol=5)#因为M3为5x5矩阵，因此选取c3的前25个值赋给M3即可
  return(M3)#M3即为计算的结果
}
M4 <- M1 %*% M2
M1
M2
Matrix_multip(M1,M2)
M4
#经比较，M3和M4相等，即实现矩阵相乘。
