sw.ftest<- function(x,...) UseMethod("sw.ftest")


sw.ftest.lm<-function(mod,num=FALSE) {
  if (num[1]==FALSE) {  
    ss<-summary(mod)
    ff<-ss$fstatistic
    numdf<-round(ff[2],digit=0)
    dendf<-round(ff[3],digit=0)
    f<-round(ff[1],digit=3)
    p<-pf(ff[1], ff[2], ff[3], lower.tail=F)
    return(paste('F(',numdf,',',dendf,')=',f,', p.',pvalue(p),sep=''))
  }

}

