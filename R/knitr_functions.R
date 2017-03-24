
knitr_refs<-function() {
  library(knitr)
  knit_hooks$set(chunk = function(x, options) {
    if (length(grep("tab",options['label'],fixed=T)>0)  ) {
      z = capture.output(x)
      l = options['label']    
      x<-paste(trim.trailing(x),"\\label{",l,"}",sep='',collapse = "")
      x
    } else   if (!is.null(options$fig.cap)  ) {
      z = capture.output(x)
      l = options['label'] 
      x<-gsub("![",paste("![\\label{",l,"}",sep=""),x,fixed=T)
      x
    } else x
    
    
  })
}

.makerulers<-function(span) {
  j<-2
  qr<-""
  for (i in 1:length(span))
  {
    jj<-j+span[i]-1
    q<-paste0(j,"-",jj)
    j<-jj+1    
    qr<-c(qr,q)
  }
  qr[-1]
}


dhtable<-function(tt,head, span=F,cap=F) {
  library(xtable)
  options(xtable.comment = FALSE)
  nc<-length(head)
  if (span[[1]]==F) {
    span=dim(tt)[2]/nc
    if(span%%1!=0) stop("Table columns are not a multiple of header labels")
    span=rep(span,nc)
  } else {
    span<-if(length(span)==1) rep(span,nc) else span
    
  }
  
  rn<-colnames(tt)
##  tt<-round(tt,digits=3)
  tt<-rbind(rn,tt)
  tt<-xtable(tt,caption=cap)
  rownames(tt)[1]<-""
  addtorow <- list()
  addtorow$pos <- list(0)
  j=0
  sec<-.makerulers(span)
  
  #sec<-lapply(seq(2,dim(tt)[2],by=span),function(i) print(i))
  
  pp<-paste0('& \\multicolumn{',span,'}{c}')
  pp<-paste0(pp,"{",head,"} ",collapse = '')
  pp1<-paste0(' \\cmidrule(r){',sec,'}',collapse = '')
  pp<-paste0(pp,'\\\\',pp1)
  #  pp<-paste0(pp,'\\\\')
  addtorow$command <-pp
  print(tt, add.to.row=addtorow, include.colnames=F,hline.after = c(-1,1,nrow(tt)),caption.placement="top")
}

#hdtable(fixtime,c("ciao1","ciao2"))

#hdtable(fixtime,c("ciao1","ciao2"))

