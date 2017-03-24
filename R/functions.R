is.all.numeric<-function(a) {
  sum(which(is.na(as.numeric(a))))==0
}

validN<-function(amod) {
  if ('glm' %in% class(amod)) {    
    amod$df.null+1
  } else
  if ('lm' %in% class(amod)) {    
    amod$df.null+1
    a<-anova(amod)
    sum(a$Df)+1
  }
  
  
}

pvalue<-function(num,equal=T) {
  eq<-ifelse(equal,'=','')
  pr<-gsub('0.','.',round(num, digits = 3),fixed=T)
  ifelse(pr<.001,'<.001',paste(eq,pr,sep=''))
}


mround <-
  function(x, digits=2)
  {
    x<-as.numeric(x)
    # this is taken from Broman function at https://github.com/kbroman/broman/blob/master/R/myround.R
    if(digits < 0) digits<-2
    while(abs(x)*10^digits<1) { 
      digits<-digits+1
      if (digits>12) break
    }
    if (digits>12) return(format(x, scientific=T))
    tmp <- sprintf(paste("%.", digits, "f", sep=""), x)
    zero <- paste0("0.", paste(rep("0", digits), collapse=""))
    tmp[tmp == paste0("-", zero)] <- zero
    tmp
  }



percent<-function(num) {
  if (num<1) num<-num*100
  round(num, digits = 0)
}

tvalue<-function(mod,num=1) {
  if (is.character(num)==T) {
    aa<-attr(mod$terms,"term.labels")
    num<-which(aa==num)
  } 
  found=F
  if (class(mod)=="merModLmerTest" ) {
    library(lmerTest)
    a<-lmerTest::summary(mod)
    cof<-a$coefficients
    t<-mround(cof[num,4])
    df<-mround(cof[num,3])
    p<-pvalue(cof[num,5])
    found=T
  } 
  if (class(mod)=="htest" ) {
    t<-mround(mod$statistic)
    df<-round(mod$parameter,digits=0)
    p<-pvalue(mod$p.value)
    found=T
  }  
 if (!found) {
 s<-summary(mod)
 t=coef(s)[num,3]  
 df=s$df[2]
 p<-pvalue(coef(s)[num,4])
 }
  
 if (length(df)>0) {
   paste('t(',df,')=',mround(t),', p.',p,sep='')
 } else {
   paste('z=',mround(t),', p.',p,sep='')
 }
}

fvalue<-function(mod,num=NA) {
 
  if (is.character(num)==T) {
    aa<-attr(mod$terms,"term.labels")
    i<-which(aa==num)
  } else i<-num
  if (class(mod)[1]=="merModLmerTest" | class(mod)[1]=="lmerTest") {
    library(lmerTest)
    s<-do.call("anova", list(mod,method.grad="Richardson"))
    ndf<-round(s$NumDF[i],digits=0)
    edf<-mround(s$DenDF[i])
    f<-mround(s$F.value[i])
    p<-pvalue(s$"Pr(>F)"[i])
    return(  paste('F(',ndf,',',edf,')=',mround(f),', p.',p,sep=''))
  }
  if ("anova" %in% class(mod) ) {
    if ("NumDF" %in% names(mod)) {
      ndf<-round(mod$NumDF[i],digits=0)
      edf<-mround(mod$DenDF[i])
      f<-mround(mod$F.value[i])
      p<-pvalue(mod$"Pr(>F)"[i])
      return(  paste('F(',ndf,',',edf,')=',mround(f),', p.',p,sep=''))
    } else {
      print("this anova is yet to be implemented")
            }
  } 
  if ("lm" %in% class(mod) ) {
  if (is.na(num)) {  
  ss<-summary(mod)
  ff<-ss$fstatistic
  numdf<-round(ff[2],digit=0)
  dendf<-round(ff[3],digit=0)
  f<-round(ff[1],digit=3)
  p<-pf(ff[1], ff[2], ff[3], lower.tail=F)
  return(paste('F(',numdf,',',dendf,')=',f,', p.',pvalue(p),sep=''))
  }
  }
  
  s<-anova(mod)
  rrow<-dim(s)[1]
  if (rrow==2) i=1
  edf<-mround(s[rrow,1],digits = 0)
  ndf<-mround(s[i,1],digits = 0)
  f<-s[i,4]
  p<-pvalue(s[i,5])
  paste('F(',ndf,',',edf,')=',mround(f),', p.',p,sep='')
}

LRT<-function(amod) {
  done=F
  if ('anova' %in% class(amod) ) {    
    res<-paste("LRT(",amod$Df[2],")=",  round(amod$Chisq[2],3),sep='')
    if ("Pr(>Chi)" %in% colnames(amod))  res<-paste(res,", p.",  pvalue(amod$"Pr(>Chi)"[2]),sep="")
    if ("Pr(>Chisq)" %in% colnames(amod))  res<-paste(res,", p.",  pvalue(amod$"Pr(>Chisq)"[2]),sep="")
    done=T
  }
  if (!done) {
    res<-"unknown"
  }
  res
}
chisq<-function(amod,pos=2) {

    .formodels<-function(mmod,pos) {
      if (pos==0) pos<-dim(mmod)[1]
      names(mmod)<-tolower(names(mmod))
      if ("deviance" %in% names(mmod) ) {
      if ("pr(>chi)" %in% colnames(mmod)) pchi<-mmod$"pr(>chi)"[pos]
      if ("pr(>chisq)" %in% colnames(mmod)) pchi<-mmod$"pr(>chisq)"[pos]    
      if ("chisq" %in% colnames(mmod)) chi<-mmod$"chisq"[pos]    
      else chi<-mmod$deviance[pos]
      if ("chi df" %in% colnames(mmod)) df<-mmod$"chi df"[pos]    
      else df<-mmod$df[pos]
       paste("X(",df,")=",  round(chi,3),", p.",  pvalue(pchi),sep='')
    } else {
      res<-"not possible to extract"
    }
    }
    
    .forterms<-function(mmod,pos) {
      if (pos==0) pos<-dim(mmod)[1]
      names(mmod)<-tolower(names(mmod))
      if ("deviance" %in% names(mmod) ) {
        if ("pr(>chi)" %in% colnames(mmod)) chi<-mmod$"pr(>chi)"[pos]
        if ("pr(>chisq)" %in% colnames(mmod)) chi<-mmod$"pr(>chisq)"[pos]    
        paste("X(",mmod$"chi df"[pos],")=",  round(mmod$chisq[pos],3),", p.",  pvalue(chi),sep='')
      } else {
        res<-"not possible to extract"
      }
      
    }
    
  done=F
  if ('glm' %in% class(amod) | 'lm' %in% class(amod) ) {    
    mmod<-anova(amod,test="Chisq")
    res<-paste(.formodels(mmod,pos),", N=",validN(amod),sep='')
    done=T
  } 
  
  if ('anova' %in% class(amod) ) {    
    done=T
    res<-.formodels(amod,pos)
  } 
  if ('htest' %in% class(amod) ) {
    done=T
    ddf<-ifelse(is.na(amod$parameter),"=",paste("(",amod$parameter,")="))
    res<-paste("X",ddf,round(amod$statistic,3),", p.",  pvalue(amod$p.value),sep='')
  } 
  if (!done) {
   warning("unknown model from chisq extraction")
  }
  res
  
}


rsquared<-function(mod) {
  paste(mround(summary(mod)$r.squared))
}




trim.leading <- function (x)  sub("^\\s+", "", x)
trim.trailing <- function (x) sub("\\s+$", "", x)


cor.prob <- function (X, N) {
  ddim<-dim(X)
  r2 <- X^2
  dfr=N-2
  Fstat <- r2 * dfr/(1 - r2)
  R <- 1 - pf(Fstat, 1, dfr)
  R
  
} 


firstToupper <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}