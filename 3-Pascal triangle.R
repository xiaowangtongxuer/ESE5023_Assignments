Pascal_triangle <- function(k){
  if (k==1)
  {
    return(1)
  }
  if (k==2){
    vec <- c(1,1)
    return(vec)
  }#第一二层直接用元素和向量来表示
  if(k>=3){#当k大于2层，考虑生成k x k的矩阵，用来记录3~k层的数
    m1 <- matrix(data=0,nrow = k,ncol = k)#矩阵的第i行会有i个有用的数
    m1[1,1]=1
    m1[2,1]=m1[2,2]=1#将第1/2行的前1/2个数赋值为1
    for(i in 3:k){
      m1[i,1]=m1[i,i]=1#第i行的第1个和第i个数总是为1
      for(j in 2:(i-1)){
        m1[i,j]=m1[(i-1),(j-1)]+m1[(i-1),j]#用双重for循环来实现对第i行2~（i-1）个元素的赋值
      }
    }
    #for(ii in 1:k){#此处测试是否满足杨辉三角
      #print(m1[ii,])
    #}
    return(m1[k,])#函数返回矩阵的第k行即可
  }
}

#test
Pascal_triangle(12)
