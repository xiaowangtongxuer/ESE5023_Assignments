Pascal_triangle <- function(k){
  if (k==1)
  {
    return(1)
  }
  if (k==2){
    vec <- c(1,1)
    return(vec)
  }#��һ����ֱ����Ԫ�غ���������ʾ
  if(k>=3){#��k����2�㣬��������k x k�ľ���������¼3~k�����
    m1 <- matrix(data=0,nrow = k,ncol = k)#����ĵ�i�л���i�����õ���
    m1[1,1]=1
    m1[2,1]=m1[2,2]=1#����1/2�е�ǰ1/2������ֵΪ1
    for(i in 3:k){
      m1[i,1]=m1[i,i]=1#��i�еĵ�1���͵�i��������Ϊ1
      for(j in 2:(i-1)){
        m1[i,j]=m1[(i-1),(j-1)]+m1[(i-1),j]#��˫��forѭ����ʵ�ֶԵ�i��2~��i-1����Ԫ�صĸ�ֵ
      }
    }
    #for(ii in 1:k){#�˴������Ƿ������������
      #print(m1[ii,])
    #}
    return(m1[k,])#�������ؾ���ĵ�k�м���
  }
}

#test
Pascal_triangle(12)