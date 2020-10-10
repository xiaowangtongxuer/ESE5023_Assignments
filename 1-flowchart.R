Print_values <- function(a,b,c) {
  if(a>b){#先比较a和b的大小
    if(b>c){#a>b时，再比较b和c的大小
      labc <- list(a=a,b=b,c=c)
      print(labc)
    }
    else{
      if(a>c){
        labc <- list(a=a,c=c,b=b)
        print(labc)
      }
      else {
        labc <- list(c=c,a=a,b=b)
        print(labc)
      }
    }
  }
  else {
    if (b>c){
      #流程图在a<b时，当b>c时红框只打印（c，a，b）
       labc <- list(c=c,a=a,b=b)
       print(labc)
      'if(c>a){
        labc <- list(b=b,c=c,a=a)
        print(labc)
      }
      else {
        labc <- list(b=b,a=a,c=c)
        print(labc)
      }
    }'#考虑了b>c>a 和 b>a>c 的情况
    else {
      labc <- list(c=c,b=b,a=a)
      print(labc)
    }
  }
}

#test 可以实现流程图的要求
Print_values(3,2,1)
Print_values(4,2,3)
Print_values(4,2,5)
Print_values(1,2,3)
