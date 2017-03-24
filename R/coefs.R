sw.estimates<- function(x,...) UseMethod("sw.estimates")

sw.estimates.lm<-function(mod,num=2,coef="B") {
  ss<-summary(mod)
  cc<-ss$coefficients
  paste0(coef,"=",mround(cc[num,1]),", t(",ss$df[2],")=",mround(cc[num,3]),", p.",pvalue(cc[num,4]))     
}