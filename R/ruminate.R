
ruminate<- function(x,...) UseMethod("ruminate")


ruminate.default<-function(a) {
  a
}

ruminate.table<-function(a,plural=F,sep='',percent=F) {
  s<-''
  if (plural) s<-"s" 
  lper<-rep("",length(a))
  if (percent) {
      per=round(100*a/sum(a),digits=1)
      lper<-unlist(lapply(1:length(a), function(b) paste("(",per[b],"\\\\%)",sep='') ))   
  }
l<-unlist(lapply(1:length(a), function(b) paste(a[b],lper[b],sep,paste(names(a)[b],s,sep='') )))
paste(l,collapse=", ")
}

ruminate.numeric<-function(a,names=c("M","SD"),range=F) {
  if (range) {
    names<-c("max","min")  
    paste(names[1],"=",mround(max(a,na.rm=T)),", ",names[2],"=",mround(min(a,na.rm=T)),sep='') 
      
  }
    else paste(names[1],"=",mround(mean(a,na.rm=T)),", ",names[2],"=",mround(sd(a,na.rm=T)),sep='')
}
   
ruminate.factor<-function(a,plural=F,percent=F, useNa="no") {
  gsub("  "," ",gsub("NA","not available",ruminate(table(a,useNA = useNa),plural=plural,percent=percent)),fixed=T)
}


ruminate.htest<-function(test) {
  
  paste("r(",test$parameter,")=",mround(test$estimate),", p.",pvalue(test$p.value),sep="")
  
}


ruminate.lm<-function(mod,num=NA,varlabs=NA) {

  puke<-function(ss,i,lab) {
  insert<-"not"
  eff<-" "
  if (length(grep(":",rownames(ss$coefficients)[i],fixed=T))>0) eff<-" interaction "
  if (ss$coefficients[i,4]<.05) insert<-""
  dfs<-ss$df[2]
  paste0("There was a ",insert," significant ",eff,"effect of ",lab,", B=",mround(ss$coefficients[i,1]),", ",cnames[3],"(",dfs,")","=",mround(ss$coefficients[i,3]),", ","p.",pvalue(ss$coefficients[i,4]),".",sep='')
  }
  ss<-summary(mod)
  if (is.na(varlabs[1])) varlabs<-rownames(ss$coefficients)
  cnames<-gsub("t value","t",colnames(ss$coefficients),fixed=T)
  nt<-length(coef((mod)))  
  res<-NULL
  if (is.na(num)) {
    for (i in (2:nt)) res<-c(res,puke(ss,i,varlabs[(i)]))
  } else res<-puke(ss,2,varlabs[2])
res      
}


ruminate.glmerMod<-function(mod,what=NA,varlabs=NA) {
  puke<-function(ss,i,lab) {
    insert<-" not"
    eff<-" "
    if (length(grep(":",rownames(ss$coefficients)[i],fixed=T))>0) eff<-" interaction "
    if (ss$coefficients[i,4]<.05) insert<-""
    dfs<-ss$df[2]
    paste0("There was a",insert," significant",eff,"effect of ",lab,", expB=",mround(exp(ss$coefficients[i,1])),", ",cnames[3],"=",mround(ss$coefficients[i,3]),", ","p.",pvalue(ss$coefficients[i,4]),".",sep='')
  }
  ss<-summary(mod)
  cnames<-gsub("t value","t",colnames(ss$coefficients),fixed=T)
  cnames<-gsub("z value","z",colnames(ss$coefficients),fixed=T)
  .terms<-attr(terms(mod),"term.labels")
  if (is.na(what[1])) what<-1:(length(.terms))
  if (!is.all.numeric(what)) what<-which(what==.terms)
  .terms<-.terms[what]
  .labs<-.terms
  what<-what+1
  if (!is.na(varlabs[1])) for(i in 1:length(varlabs)) .labs[i]<-varlabs[i]
  res<-NULL
  for (i  in 1:length(what)) res<-c(res,puke(ss,what[i],.labs[i]))
    
  paste(res,sep=".")      
}


