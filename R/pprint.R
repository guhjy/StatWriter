pprint.cor<-function(R,N,asterisk=F,subdiag=F) {
  P<-cor.prob(R,N)
  if (asterisk) {
    R<-ifelse(P<.05 & R<1,paste(round(R,3),"*",sep=''),round(R,3))
  } else {
#    R<-ifelse(R<1,paste(gsub('0','',round(R,3),fixed=T),' (',pvalue(P,equal=F),')',sep=''),round(R,3))
    R<-paste(gsub('0.','.',round(R,3),fixed=T),' (',pvalue(P,equal=F),')',sep='')
    R<-matrix(R,nrow=dim(P)[1],ncol=dim(P)[2])
  }
  for (i in 1:ncol(R)) R[i,i]=1
  if (subdiag==T) R[!lower.tri(tab,diag = T)]<-""

  as.matrix(R)    

}


pprint.tab<-function(R,N,asterisk=F) {
  P<-prop.table(R)
    R<-paste(R,' (',round(P,digits=2),')',sep='')
    R<-matrix(R,nrow=dim(P)[1],ncol=dim(P)[2])
  
    as.matrix(R)   
  
}
