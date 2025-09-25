#---Clusters dataframe------

#' Creating a dataframe of the different clusters, articles and words
#'
#' @param x The abundance table. This table is not scaled nor normalized yet, but it has the optimal words incidence.
#' Recommended to use the output from function optimal_incidence()
#' @param k The number of clusters
#' @param model The model produced by the Detrented Correspondance Analysis
#' @param wordnumber The number of words per cluster to be displayed in the wordcloud
#' @param colores The vector of colors. The length of this vector should be at least the same as the k.
#' @param pvalue The probabilty that the abundance indicators are high
#' @param abmean The mean of the abundance indicators per cluster
#' @importFrom cluster agnes
#' @importFrom stats cutree
#' @importFrom labdsv indval
#'
#' @returns A dataframe with three columns:
#'
#'  $article: the title of the article
#'  $cluster_number: the number of the cluster
#'  $words: the respective words in the cluster
#' @export
groups_df <- function(x,k, model, wordnumber=5,colores = colores,pvalue=0.05,abmean=0.01){

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
  temp5<-labdsv::indval(abundancet,cutmodel,numitr=1000)

  nk <- k

  achsen<-model$cproj[,c(1:2)]
  cluster_dca<-cbind(round(temp5$indval,d=2),temp5$pval,names(temp5$pval),achsen)
  cluster_dcap<-cluster_dca[cluster_dca[nk+1]<pvalue,] #p-value less than ...
  final_cluster<-cluster_dcap[which(apply(cluster_dcap[,c(1:nk)],1,mean)>abmean),] #abmean means abudance indicators mean

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


  grupos <- data.frame(article= row.names(abundancet), cluster_number = cutmodel)
  grupos$words <- NA
  for (i in 1:nrow(grupos)) {
    cluster_id <- grupos$cluster_number[i]
    # Get the elements (names) for this specific cluster
    palabras_for_cluster <- row.names(subsets[[cluster_id]])
    # Concatenate the elements into a single string, or store as a list
    grupos$words[i] <- paste(palabras_for_cluster, collapse = ", ") # or `I(list(palabras_for_cluster))` if you want a list
    #grupos$colores[i] <- colores[cluster_id]
  }
  return(grupos)
}

#' @examples to be added

