merge.table<-function(t1,t2,horizontal=T,pretty=T,digit=3,apa=T) {
  pr1=pr2=" "
  digit
  if (pretty) {
    pr1<-" ("
    pr2<-") "
  }
  mm<-matrix(paste(round(t1,digits=digit),pr1,round(t2,digit=digit),pr2,sep=''),nrow=dim(t1)[1])
  mm<-as.table(mm)  
  colnames(mm)<-colnames(t1)
  rownames(mm)<-rownames(t1)
  mm
}


tablize<-function(listmod) {
  

}