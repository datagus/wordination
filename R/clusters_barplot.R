#----cluster barplot ------

clusters_barplot <- function(abundancet, number_of_clusters, printclusters = TRUE, colores = colores){
  modelclust<-cluster::agnes(abundancet,method="ward")
  nk<-2 # initial number of clusters
  cutmodel_values <- list()
  for (i in c(2:20)){
    cutmodel <-cutree(modelclust,k=nk)
    elements_incluster <- table(cutmodel)

    cluster_below_number <- FALSE
    gus <- c()
    for (j in c(1:nk)){
      num_elements <- as.numeric(elements_incluster[j])
      gus <- c(gus, num_elements)
      if (num_elements < 1){
        cluster_below_number <- TRUE
        break
      }
    }

    if (cluster_below_number){
      break
    }
    cutmodel_values[[i]] <- gus
    if (printclusters == TRUE){
      print(elements_incluster)
    }

    nk <- nk + 1
  }
  nk <- nk -1
  paste0("The maximum number of clusters is ", nk)
  #---ploting the cutmodel
  num_rows <- nk
  num_columns <- nk
  cutmodel_m<- matrix(NA, nrow = num_rows, ncol = num_columns)
  for (i in 2:num_rows) {
    # Fill the matrix row by row
    cutmodel_m[i, 1:length(cutmodel_values[[i]])] <- cutmodel_values[[i]]
  }
  cutmodel_m <- cutmodel_m[-1,]

  colores <- c("#FF0000","#008000","#800000","#0000FF","#9400D3","#FF00FF","#FFD700",
               "#7FFF00","#FFA500","#000080","#A52A2A","#00CED1","#8B4513","#DC143C",
               "#DA70D6","#D2691E","#00CED1","#9400D3","#ADFF2F","#FF69B4","#CD5C5C"
  )

  number_of_clusters <- number_of_clusters
  nk <- number_of_clusters - 1

  return(barplot(t(cutmodel_m[1:nk, 1:(nk+1)]),
                 beside = TRUE,
                 horiz = FALSE,
                 col = colores[1:(nk+1)],
                 legend.text = paste("Cluster", 1:(nk+1)),
                 args.legend = list(x = "topright", bty = "n", title = "Clusters"),
                 ylab = "Number of papers",
                 xlab = "Number of clusters",
                 names.arg = paste((1:nk)+1),
                 main = "Distribution of clusters",
                 space = c(0, 0)))
}
