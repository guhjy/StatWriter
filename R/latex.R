lxtable<-function(tab,caption,label,note=F,rownames=T,head=F) {
  nr<-dim(tab)[1]  
  nc<-dim(tab)[2]
  div<-1/nc
  headline=""
#  al<-paste0(rep("cp",nc),"{",div,"\\linewidth}",collapse="")
  al<-paste0(rep("c",nc),collapse="")
  
  if (!is.null(rownames(tab)) & rownames==T)
  {
    tab$Variabili<-rownames(tab)
    nc=nc+1
    tab<-tab[,c(nc,1:(nc-1))]
    colnames(tab)[1]<-" "
    al<-paste0("l",al,collapse = "")
  }
  he<-paste(colnames(tab),collapse = " & ")
  xnote=""
  if (!is.logical(note)) {
    xnote=paste("
  \\begin{tablenotes}[flushleft]
  \\footnotesize
  \\item \\textit{note}:",note,"
  \\end{tablenotes}
  ")
  }
  if (head!=F) {
    headline=paste("\\toprule
                   \\multicolumn{",nc,"}{c}{",head,"}\\\\")
  }
  
  p1<-paste0("
 \\begin{table}[h!]
 \\caption{",caption,"}
 \\label{",label,"}
 \\centering
 \\begin{threeparttable}
 \\begin{tabular}{",al,"}
 ",headline,"
 \\toprule
 ",he," \\\\
 \\midrule")
  p<-lapply(1:nr,function(i) {paste0(tab[i,],collapse = " & ")    })
  p2<-paste(p,"\\\\",collapse = " ")
  p3<-paste0("
 \\bottomrule
 \\addlinespace
 \\end{tabular}",xnote,"
 \\end{threeparttable}
 \\end{table}")
  cat(p1,p2,p3)
}

