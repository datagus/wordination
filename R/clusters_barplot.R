#----cluster barplot ------

#' Producing cluster barplots
#'
#' This function provides a barplot with different arrangement of clusters. Starting with minimum of  2 clusters arrangement,
#' the users can opt for several arrangements.
#'
#' @param x The abundance table. This table is not scaled nor normalized yet, but it has the optimal words incidence.
#' Recommended to use the output from function optimal_incidence()
#' @param maxclusters Number of maximum clusters to create. The user can indicate, for example,
#' maxclusters=10. In this case, independent clustering arrangements of two clusters up to ten clusters will be created.
#' By default this is set to 21, if the maxclusters is > 21, then the user has to provide a color vector that matches this number of clusters.
#' @param printclusters logical. FALSE (default) and TRUE if the table showing the partitions and clusters is to be displayed
#' @param colores A vector containing the colors to be used for the barplot. The lenght of this vector
#' should be at least equal to the number of maxclusters.
#' @param showCounts logical. FALSE (default) and TRUE if the users want the counts of cluster printed directly in the barplot
#' @importFrom cluster agnes
#' @importFrom stats cutree
#' @importFrom graphics barplot
#'
#' @returns A plot containing all the barplots for each clustering arrangement
#' @export
clusters_barplot <- function(x, maxclusters=21, printclusters = FALSE, colores = NULL, showCounts=FALSE){

  # default colors if none provided
  if (is.null(colores)) {

    colores <- c("#FF0000","#008000","#800000","#0000FF","#9400D3","#FF00FF","#FFD700",
                 "#7FFF00","#FFA500","#000080","#A52A2A","#00CED1","#8B4513","#DC143C",
                 "#DA70D6","#D2691E","#00CED1","#9400D3","#ADFF2F","#FF69B4","#CD5C5C"
    )
  }

  if (maxclusters > length(colores)) {
    stop("Number of clusters (", maxclusters,
         ") exceeds length of color vector (", length(colores), "). ",
         "Please provide at least as many colors as clusters.")
  }

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

  #cutmodel_values will store the the different clustering arrangements. For examples, if maxclusters=5,
  #cutmodel_values[1] will be NULL
  #cutmodel_values[2] will contain the arrangements of two clusters,
  #cutmodel_values[3] will contain the arrangements of three clusters,
  #cutmodel_values[10] will contain the arrangements of ten clusters.
  cutmodel_values <- list()

  #in this loop, the initial cluster tree is going to be cut according to the specified number of clusters
  for (k in c(2:maxclusters)){

    #k is the clustering arrangement
    #starting the cut tree with two clusters, then in the second iteration, it will be three
    cutmodel <- cutree(modelclust, k = k)

    #storing the clusters and the count of articles in it.
    #table(cutmodel) produces a table with the first row with the clusters (1, 2, ...)...
    #...and the second row with the respective number of elements in the cluster
    elements_incluster <- table(cutmodel)

    # check if any cluster has fewer than 1 element
    if (any(elements_incluster < 1)) {
      message("Maximum number of clusters reached")
      break
    }

    #Suppossing that number_of_clusters is 3.
    #For the first iteration(k=2), the gus list will have the number of elements of cluster 1 and 2
    #These numbers will be saved in cutmodel_values[[i]]
    #For the second iteration(nk=3), the cutmodel_values will have the number of elements of clusters 1, 2 and 3
    cutmodel_values[[k]] <- as.numeric(elements_incluster)

    #if the user wants to get the number of elements in each cluster for each iteration
    if (printclusters){
      print(elements_incluster)
    }

    # final k is the last successful cluster count
    k <- length(cutmodel_values)
    paste0("The maximum number of clusters reached is ", k)
    #k will eventually take the value of maxclusters
  }


  #---creating a matrix (tree_m)
  #with ows as the clustering arrangments and columns as the clusters.
  #When k=5, the matrix will have 5 rows (one empty that will be deleted after)...
  #... and 5 columns (cluster 1, cluster 2, ..., cluster 5)
  #The matrix is not populated yet.
  num_rows <- k
  num_columns <- k
  tree_m<- matrix(NA, nrow = num_rows, ncol = num_columns)

  #The matrix will be populated with the following loop. Starting at the i=2, which is the minimum of the clustering arrangements:
  # the cells located in the second row and the first two columns of the matrix will be populated,
  # because 1:length(partition_values[[i]]), which in this case would be 1:2.
  # For the next iteration, where i = 3, the cells located in the third row and first three columns...
  #...be populated because 1:length(partition_values[[i]]), which in this case would be 1:3. And so on...

  for (i in 2:num_rows) {
    # Fill the matrix row by row
    tree_m[i, 1:length(cutmodel_values[[i]])] <- cutmodel_values[[i]]
  }
  #first row is deleted
  tree_m <- tree_m[-1,]



  #Since we deleted the first row in tree_m, nk is the number of rows, which tecnically would be...
  #...the number of partitions or clustering arrangements.
  #The matrix tree_m is transposed in order to build the barplots


  bp <- barplot(t(tree_m),
                beside = TRUE,
                horiz = FALSE,
                col = colores[1:k],
                legend.text = paste("Cluster", 1:k),
                args.legend = list(
                  x = "topright",
                  bty = "n",
                  title = "Clusters",
                  cex = 0.7,        # Makes text smaller (default is 1)
                  pt.cex = 0.8,     # Makes legend symbols smaller
                  y.intersp = 0.8,  # Reduces vertical spacing between items
                  x.intersp = 0.5   # Reduces horizontal spacing
                ),
                ylab = "Number of papers",
                xlab = "Number of clusters",
                names.arg = paste(2:k),
                main = "Distribution of clusters",
                space = c(0, 0))

  if(showCounts){
    for (i in seq_len(nrow(bp))) {     # clusters
      for (j in seq_len(ncol(bp))) {   # number of clusters
        val <- tree_m[j, i]        # careful: cutmodel_m[1,] = 2 clusters now
        if (!is.na(val)) {
          text(x = bp[i, j], y = val, labels = val, pos = 3, cex = 0.7)
        }
      }
    }
  }

  return(bp)
}
#'
#' @examples to be added

