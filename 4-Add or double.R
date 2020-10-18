Least_moves <- function(k)#尝试使用递归求解
{#因为每次移动都只会倍增或者加1
#因此在1~100之间，第n（n>2）个奇数（2n-1）的步数一定是第n-1个偶数的步数加一。
#即有steps（2i+1）=steps（2i）+1
#而且steps（2i） = steps(i)+1
  #即有当i大于3时，如果为偶数，则计算i/2的步数，再加一即可得到i的步数
  #如果为奇数，则计算i-1的步数，再加一即可得到i的步数
  #一直递归到i=1，2，3时的情形即可
  if (k <= 3)
  {
      if (k==1) {
      return(0)
    }
    else if (k==2) 
      {
      return(1)
    }
    else if (k==3) {
      return(2)
    }#这三个if语句定义了2，3的步数
  }
  else {#k大于3时，可以使用递归
    if(k %% 2== 0)
    {
      return(Least_moves(k/2)+1)
    }
    else {
      return(Least_moves(k-1)+1)
    }
  }
}

#test
for (i in 1:10){#把1-10之间的所有步数都打印出来
  print(paste0(i,"->",Least_moves(i)))
}
# good work
