#----Dendogram function----

#' Generating clusters dendogram with colors
#'
#' This dendogram allows you inspect the clusters and how they were formed. It is useful to identify anomalies in the clustering.
#'
#' @param x The abundance table. This table is not scaled nor normalized yet, but it has the optimal words incidence.
#' Recommended to use the output from function optimal_incidence()
#' @param k The number of clusters
#' @param colores The vector of colors. The length of this vector should be at least the same as the k.
#' @importFrom stats cutree
#' @importFrom labdsv indval
#' @importFrom stats as.dendrogram
#' @importFrom dendextend get_subdendrograms
#' @importFrom dendextend color_branches
#'
#' @returns A dendogram and list with different objects:
#'
#' $dend The dendogram as a whole object
#' $subdend The different subdendograms or general branches
#' $clusters The different clusters
#' $site_names A list of vectors containing the names of the articles in each subdend or cluster
#' $clusters_length The length of each subdendogram or general branches
#'
#' @export
wordendo <- function(x,k, colores=c()){
  ## Correcting the frequencies of words by the total words in each article
  pa <- (x > 0) * 1
  sites_length <- unname(colSums(pa, na.rm = TRUE))
  abundance <- sweep(x, 2, sites_length, "/")
  abundance <- abundance*1000

  #transpossing and scaling the abundance table
  abundancet <- log(t(abundance)+1)
  #abundancet <- t(x)

  set.seed(123)

  set.seed(123)
  #running the clustering
  modelclust<-agnes(abundancet,method="ward")
  cutmodel <-cutree(modelclust,k=k)
  temp5<-labdsv::indval(abundancet,cutmodel,numitr=1000)

  nk <- k

  modelclust<-agnes(abundancet,method="ward")

  ## creating a vector that matches the wordcloud and dendogram
  cutmodel <-cutree(modelclust,k=nk)


  #colores <- c("#FF0000","#008000","#800000","#0000FF","#9400D3","#FF00FF","#FFD700",
  #             "#7FFF00","#FFA500","#000080","#A52A2A","#00CED1","#8B4513","#DC143C",
  #             "#DA70D6","#D2691E","#00CED1","#9400D3","#ADFF2F","#FF69B4","#CD5C5C"
  #)

  grupos <- data.frame(article= row.names(abundancet), Cluster = cutmodel)
  grupos$palabras <- NA
  for (i in 1:nrow(grupos)) {
    cluster_id <- grupos$Cluster[i]
    grupos$colores[i] <- colores[cluster_id]
  }

  dend <- as.dendrogram(modelclust)

  sub_dend <- get_subdendrograms(dend, k = nk) #getting the subdendrograms in a list
  #plot(sub_dend[[2]], main = paste0("Dendrogram ", nk)) #plotting individual dendograms

  clusters <- list() #the clusters
  site_names <- list() #the names of the papers or labels
  member_count <- list(list()) #the number of papers in each branch
  clusters_length <- c()
  for(i in 1:length(sub_dend)){
    clusters[[i]] <- sub_dend[[i]]
    site_names[[i]] <- labels(clusters[[i]])
    clusters_length <- c(clusters_length,length(site_names[[i]]))
    member_count[[i]] <- length(site_names[[i]])
    for(j in 1:2){
      member_count[[i]][[j]]<-length(labels(clusters[[i]][[j]]))
    }
  }

  labeles = c()
  ### matching the colors
  colores_dend <- c()
  for (i in 1:length(clusters)) {
    A <- labels(clusters[[i]])
    for (j in 1:length(unique(grupos$Cluster))){
      B <- grupos$article[grupos$Cluster==j]
      if (all(A%in%B)==TRUE){
        color <- unique(grupos$colores[grupos$Cluster==j])
        colores_dend <- append(colores_dend, color)
        labeles = append(labeles,j)
      }
    }
  }

  dend <- color_branches(dend, k = nk, col=colores_dend, groupLabels=labeles)
  # Plot the dendrogram with colored clusters
  plot(dend, main = paste0("Dendrogram with ", nk , " Clusters"))

  return(list(dend=dend,
              sub_dend=sub_dend,
              clusters=clusters,
              site_names=site_names,
              clusters_length=clusters_length))
}

#' @examples

