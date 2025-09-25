#-------WordCloud-----


#' Producing word cloud.
#'
#' This function provides a plot with the DCA coordinates of the most representative words in each cluster.
#'
#' @param x The abundance table. This table is not scaled nor normalized yet, but it has the optimal words incidence.
#' Recommended to use the output from function optimal_incidence()
#' @param k The number of clusters
#' @param model The model produced by the Detrented Correspondance Analysis
#' @param wordnumber The number of words per cluster to be displayed in the wordcloud
#' @param colores The vector of colors. The length of this vector should be at least the same as the k.
#' @param cex The font size of the words
#' @param pvalue The probabilty that the abundance indicators are high
#' @param abmean The mean of the abundance indicators per cluster
#' @param xmin Numeric. To adjust the lower x-axis
#' @param xmax Numeric. To adjust the higher x-axis
#' @param ymin Numeric. To adjust the lower y-axis
#' @param ymax Numeric. To adjust the higher y-axis
#' @param xspace Numeric. To adjust the coordinate of words along the x axis
#' @param yspace Numeric. To adjust the coordinate of words along the y axis
#' @importFrom cluster agnes
#' @importFrom stats cutree
#' @importFrom labdsv indval
#' @importFrom graphics text
#' @importFrom graphics par
#'
#' @returns A wordcloud, a dataframe with the clusters and abundance statistics, and a list with the specific cluster word subsets
#' @export
wordcloud <- function(x, k, model, wordnumber=5,colores = colores,cex=0.8,pvalue=0.05,abmean=0.01, xmin = 0, xmax = 0, ymin=0, ymax=0, xspace=0, yspace=0){

  ## Correcting the frequencies of words by the total words in each article
  pa <- (x > 0) * 1
  sites_length <- unname(colSums(pa, na.rm = TRUE))
  abundance <- sweep(x, 2, sites_length, "/")
  abundance <- abundance*1000

  #transpossing and scaling the abundance table
  abundancet <- log(t(abundance)+1)
  #abundancet <- t(x)

  set.seed(123)
  #running the clustering
  modelclust<-agnes(abundancet,method="ward")
  cutmodel <-cutree(modelclust,k=k)
  temp5<-indval(abundancet,cutmodel,numitr=1000)

  nk <- k

  achsen<-model$cproj[,c(1:2)]
  cluster_dca<-cbind(round(temp5$indval,digits=2),temp5$pval,names(temp5$pval),achsen)
  cluster_dcap<-cluster_dca[cluster_dca[nk+1]<pvalue,] #p-value less than ...
  final_cluster<-cluster_dcap[which(apply(cluster_dcap[,c(1:nk)],1,mean)>abmean),] #abmean means abudance indicators mean

  par(xpd=T)
  par(mar=c(2,2,0.1,0.1))
  plot(final_cluster$DCA1,final_cluster$DCA2,xlim=c(min(final_cluster$DCA1)+xmin,max(final_cluster$DCA1)-xmax),ylim=c(min(final_cluster$DCA2)+ymin,max(final_cluster$DCA2)-ymax),xlab="DCA1",ylab="DCA2",type="n")
  points(0, 0, pch = 16, col = "black", cex = 1.5)

  subsets <- list()

  colores <- c("#FF0000","#008000","#800000","#0000FF","#9400D3","#FF00FF","#FFD700",
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
      text(subsets[[i]]$DCA1[c(j)]-xadj,subsets[[i]]$DCA2[c(j)], labels=rownames(subsets[[i]])[c(j)],col=colores[i],cex=cex, font=2)
      xadj <- xadj + xspace
      yadj <- yadj + yspace
    }
  }
  return(list(final_cluster=final_cluster, subsets=subsets))
}

#' @examples to be added

