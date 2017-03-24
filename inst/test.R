ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
x<-rnorm(20,0,1)
weight <- c(ctl, trt)
m <- lm(weight ~ group+x)

anova(m)
summary(m)

frAnova(m)
bovinate(m)

fvalue(m,1)
tvalue(m,1)

dat<-as.data.frame(cbind(y=rbinom(100,1,.5),x=rnorm(100,0,1),z=rnorm(100,0,1),id=rep(1:20)))
mod0<-glmer(y~(1|id),dat,family=binomial())
mod1<-glmer(y~(1|id)+x,dat,family=binomial())
mod2<-glm(y~x+z,dat,family = "gaussian")
mod2<-lm(y~x+z,dat)

a<-anova(mod2,test="Chisq")
a
res<-anova(mod1,mod0)
class(a)
chisq(mod2,3) 

chisq(res) 
res
