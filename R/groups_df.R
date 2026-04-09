#' Create a summary data frame of clusters, articles, and top words.
#'
#' This function creates a dataframe mapping each article to its cluster and the top
#' indicator words for that cluster. It is useful for inspecting which articles belong
#' to which cluster and what words characterize each cluster. Clustering is performed
#' internally using Ward's method via \code{\link[cluster]{agnes}}, and words are selected
#' based on their indicator values from \code{\link[labdsv]{indval}}.
#'
#' @param x Data frame. The abundance table with words as rows and articles as columns.
#'  All columns must be numeric. The table should not be scaled or normalized beforehand.
#'  It is recommended to apply an incidence threshold using \code{\link{optimal_incidence}}
#'  before passing the table to this function.
#' @param k Integer. The number of clusters.
#' @param model A \code{decorana} object produced by \code{link{run_dca}}.
#' @param wordnumber Integer. Number of top indicator words to list per cluster. Default is 5.
#' @param colores Character vector. Colors for each cluster. Must have at least
#'  \code{k} elements. If \code{NULL} (default), a built-in palette is used.
#' @param pvalue Numeric. Maximum p-value threshold for filtering words from the indicator
#'  analysis. Default is 0.05.
#' @param abmean Numeric. Minimum mean abundance indictor for a word to be included.
#'  Default is 0.01.
#' @importFrom cluster agnes
#' @importFrom stats cutree
#' @importFrom labdsv indval
#'
#' @returns A data frame with three columns:
#' \describe{
#'   \item{article}{Character. The article filename.}
#'   \item{cluster_number}{Integer. The cluster assignment.}
#'   \item{words}{Character. The top indicator words for the cluster,
#'     comma-separated.}
#' }
#'
#' @seealso \code{\link{wordcloud}}, \code{\link{cluster_indicators}}
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(example_abundance)
#' model <- run_dca(example_abundance)
#' groups_df(example_abundance, k = 3, model = model,
#'           pvalue = 1, abmean = 0)
#' }
#'
groups_df <- function(x, k, model, wordnumber = 5,colores = NULL,
                      pvalue = 0.05, abmean = 0.01) {

  # Default palette
  if (is.null(colores)) {
    colores <- c("#FF0000","#008000","#800000","#0000FF","#9400D3","#FF00FF","#FFD700",
                 "#7FFF00","#FFA500","#000080","#A52A2A","#00CED1","#8B4513","#DC143C",
                 "#DA70D6","#D2691E","#00CED1","#9400D3","#ADFF2F","#FF69B4","#CD5C5C")
  }

  # Convert to relative frequencies per article (per 1000 words)
  pa <- (x > 0) * 1
  sites_length <- unname(colSums(pa, na.rm = TRUE))
  abundance <- sweep(x, 2, sites_length, "/")
  abundance <- abundance * 1000

  # Log-transform and transpose (articles as rows)
  abundancet <- log(t(abundance)+1)

  set.seed(123)
  modelclust <- agnes(abundancet, method = "ward")
  cutmodel <- cutree(modelclust, k = k)
  temp5 <- labdsv::indval(abundancet, cutmodel, numitr = 1000)

  # Combine indicator values with DCA coordinates, then filter
  achsen <- model$cproj[, 1:2]
  cluster_dca <- cbind(round(temp5$indval, digits = 2),temp5$pval,
                       names(temp5$pval), achsen)
  cluster_dcap <- cluster_dca[cluster_dca[k + 1] < pvalue, ]
  final_cluster <- cluster_dcap[which(apply(cluster_dcap[, 1:k], 1, mean) > abmean), ]

  # Select top words per cluster, sorted by indicator value
  subsets <- list()
  for (i in 1:k){
    subsets[[i]] <- final_cluster[order(final_cluster[, i], decreasing = TRUE)[1:wordnumber], ]
    subsets[[i]]$cluster <- i
  }

  # Build output: map each article to its cluster and top words
  grupos <- data.frame(article= row.names(abundancet), cluster_number = cutmodel)
  grupos$words <- NA
  for (i in 1:nrow(grupos)) {
    cluster_id <- grupos$cluster_number[i]
    palabras_for_cluster <- row.names(subsets[[cluster_id]])
    grupos$words[i] <- paste(palabras_for_cluster, collapse = ", ")
  }
  return(grupos)
}
