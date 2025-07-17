#-------WordCloud-----


wordcloud <- function(abundancet, nk, model, wordnumber=5,colores = colorines,cex=0.8,pvalue=0.05,abmean=0.01, xmin = 0, xmax = 0, ymin=0, ymax=0, xspace=0, yspace=0){

  set.seed(123)


  modelclust<-cluster::agnes(abundancet,method="ward")
  cutmodel <-stats::cutree(modelclust,k=nk)
  temp5<-labdsv::indval(abundancet,cutmodel,numitr=1000)

  achsen<-model$cproj[,c(1:2)]
  cluster_dca<-cbind(round(temp5$indval,d=2),temp5$pval,names(temp5$pval),achsen)
  cluster_dcap<-cluster_dca[cluster_dca[nk+1]<pvalue,] #p-value less than ...
  final_cluster<-cluster_dcap[which(apply(cluster_dcap[,c(1:nk)],1,mean)>abmean),] #abmean means abudance indicators mean

  par(xpd=T)
  par(mar=c(2,2,0.1,0.1))
  plot(final_cluster$DCA1,final_cluster$DCA2,xlim=c(min(final_cluster$DCA1)+xmin,max(final_cluster$DCA1)+xmax),ylim=c(min(final_cluster$DCA2)+ymin,max(final_cluster$DCA2)+ymax),xlab="DCA1",ylab="DCA2",type="n")
  points(0, 0, pch = 16, col = "black", cex = 1.5)
  #wordnumber<-wordnumber

  subsets <- list()

  colorines <- c("#FF0000","#008000","#800000","#0000FF","#9400D3","#FF00FF","#FFD700",
               "#7FFF00","#FFA500","#000080","#A52A2A","#00CED1","#8B4513","#DC143C",
               "#DA70D6","#D2691E","#00CED1","#9400D3","#ADFF2F","#FF69B4","#CD5C5C"
  )

  for (i in 1:nk){
    subsets[[i]] <- final_cluster[order(final_cluster[,i],decreasing=T)[c(1: wordnumber)],]
    #text(subsets[[i]]$DCA1[c(1:wordnumber)],subsets[[i]]$DCA2[c(1:wordnumber)],labels=rownames(subsets[[i]])[c(1:wordnumber)],col=colores[i] ,cex=0.8)
    subsets[[i]]$cluster <- i
  }


  for (i in 1:nk){
    xadj <- 0
    yadj <- 0
    for (j in 1:wordnumber){
      graphics::text(subsets[[i]]$DCA1[c(j)]-xadj,subsets[[i]]$DCA2[c(j)], labels=rownames(subsets[[i]])[c(j)],col=colores[i],cex=cex, font=2)
      xadj <- xadj + xspace
      yadj <- yadj + yspace
    }
  }
  return(list(final_cluster=final_cluster, subsets=subsets))
}
