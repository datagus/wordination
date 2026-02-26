#' Visualize cluster size distribution
#'
#' This function produces a grouped barplot showing the number of articles in each cluster across
#' different cluster arrangements. Starting from 2 clusters up to \code{maxclusters}, each arrangement
#' is displayed side by side, making it easier to evaluate which number of clusters best partitions the data.
#' Clustering is performed internally using Ward's method via \code{\link[cluster]{agnes}}.
#'
#' @param x Data frame. The abundance table with words as rows and articles as columns.
#'  All columns must be numeric. The table should not be scaled or normalized beforehand.
#'  It is recommended to apply an incidence threshold using \code{\link{optimal_incidence}}
#'  before passing the table to this function.
#' @param maxclusters Integer. Number of maximum clusters to evaluate. Arrangements from 2 up to this
#'  value are created. Default is 21. If greater than 21, a custom color vector must be provided.
#' @param printclusters Logical. If \code{TRUE} (default), the cluster size for each arrangement is printed
#'  to the console. Default is \code{FALSE}.
#' @param colores Character vector. Colors for the barplot.The length must be at least equal to
#' \code{maxclusters}. If \code{NULL} (default), a built-in palette of 21 colors is used.
#' @param showCounts Logical. If \code{TRUE}, cluster counts are displayed directly above each bar.
#'  Default is \code{FALSE}.
#' @importFrom cluster agnes
#' @importFrom stats cutree
#' @importFrom graphics barplot
#'
#' @returns A barplot showing the cluster size distributions for each arrangement.
#'
#' @export
#'
#' @examples
#' \donttest{
#' x <- data.frame(
#'   art1 = c(3, 0, 1, 2, 0, 1),
#'   art2 = c(0, 2, 1, 0, 3, 1),
#'   art3 = c(1, 1, 0, 2, 1, 0),
#'   art4 = c(2, 0, 1, 1, 0, 2),
#'   art5 = c(0, 1, 2, 0, 1, 1),
#'   art6 = c(1, 0, 0, 1, 2, 1)
#' )
#' rownames(x) <- paste0("word", 1:6)
#' clusters_barplot(x, maxclusters = 4)
#' clusters_barplot(x, maxclusters = 4, showCounts = TRUE)
#' }
#'
clusters_barplot <- function(x, maxclusters = 21, printclusters = FALSE, colores = NULL, showCounts = FALSE){

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

  # Convert to relative frequencies per article (per 1000 words)
  pa <- (x > 0) * 1
  sites_length <- unname(colSums(pa, na.rm = TRUE))
  abundance <- sweep(x, 2, sites_length, "/")
  abundance <- abundance * 1000

  # Log-transform and transpose (articles as rows)
  abundancet <- log(t(abundance)+1)

  set.seed(123)
  modelclust<-agnes(abundancet, method = "ward")

  # Cut the tree at each level from 2 to maxclusters.
  # cutmodel_values[[k]] stores the number of articles in each cluster
  # for the arrangement with k clusters. Index 1 is always NULL since
  # the minimum arrangement is 2 clusters.
  cutmodel_values <- list()

    for (k in c(2:maxclusters)){
    cutmodel <- cutree(modelclust, k = k)
    elements_incluster <- table(cutmodel)

    if (any(elements_incluster < 1)) {
      message("Maximum number of clusters reached")
      break
    }

    cutmodel_values[[k]] <- as.numeric(elements_incluster)

    if (printclusters){
      print(elements_incluster)
    }

    k <- length(cutmodel_values)
  }

  # Build matrix: rows = cluster arrangements (2, 3, ..., k),
  # columns = cluster IDs (1, 2, ..., k).
  # Each row stores the article counts per cluster for that arrangement.
  # First row is empty (no 1-cluster arrangement) and removed after.
  num_rows <- k
  num_columns <- k
  tree_m <- matrix(NA, nrow = num_rows, ncol = num_columns)

  for (i in 2:num_rows) {
    tree_m[i, 1:length(cutmodel_values[[i]])] <- cutmodel_values[[i]]
  }
  tree_m <- tree_m[-1,] # removing empty first row

  # Transpose tree_m so barplot groups bars by arrangement
  bp <- barplot(t(tree_m),
          beside = TRUE, # not stapled
          horiz = FALSE, # displayed in vertical
          col = colores[1:k], #the colors ofr each bar
          legend.text = paste("Cluster", 1:k), #legend text
          args.legend = list(x = "topright", bty = "n", title = "Clusters"),
          ylab = "Number of papers",
          xlab = "Number of clusters",
          names.arg = paste(2:k),
          main = "Distribution of clusters",
          space = c(0, 0))

  # Overlay article counts above each bar
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
