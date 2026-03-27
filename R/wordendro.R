#' Generate a colored cluster dendrogram
#'
#' This function produces a dendrogram of the hierarchical clustering, with branches colored by
#' cluster membership. It is useful for inspecting how clusters were formed and identifying potential
#' anomalies in the clustering. Clustering is performed internally using Ward's method via
#' \code{\link[cluster]{agnes}}.
#'
#' @param x Data frame. The abundance table with words as rows and articles as columns.
#'  All columns must be numeric. The table should not be scaled or normalized beforehand.
#'  It is recommended to apply an incidence threshold using \code{\link{optimal_incidence}}
#'  before passing the table to this function.
#' @param k Integer. The number of clusters.
#' @param colores Character vector. Colors for each cluster. Must have at least \code{k} elements.
#' @importFrom cluster agnes
#' @importFrom stats cutree
#' @importFrom labdsv indval
#' @importFrom stats as.dendrogram
#' @importFrom dendextend get_subdendrograms
#' @importFrom dendextend color_branches
#'
#' @returns A list with five elements:
#' \describe{
#'  \item{dend}{The full colored dendrogram object.}
#'  \item{sub_dend}{List of subdendrogram object.}
#'  \item{clusters}{List of dendrogram branches, one per cluster}
#'  \item{site_names}{List of character vectors with the article names in each cluster.}
#'  \item{clusters_length}{Integer vector with the number of articles in each cluster.}
#'  }
#'
#' @seealso \code{\link{wordcloud}}, \code{\link{ellipsis_plot}}, \code{\link{clusters_barplot}}
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(example_abundance)
#' wordendro(example_abundance, k = 3)
#' }
#'

wordendro <- function(x, k, colores = NULL) {

  # Default palette
  if (is.null(colores)){
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
  abundancet <- log(t(abundance) + 1)

  set.seed(123)
  modelclust <- agnes(abundancet, method = "ward")
  cutmodel <-cutree(modelclust, k = k)

  # Map each article to its cluster and color
  grupos <- data.frame(article = row.names(abundancet), Cluster = cutmodel)
  for (i in 1:nrow(grupos)) {
    cluster_id <- grupos$Cluster[i]
    grupos$colores[i] <- colores[cluster_id]
  }

  dend <- as.dendrogram(modelclust)
  sub_dend <- get_subdendrograms(dend, k = k)

  # Extract cluster members and counts
  clusters <- list()
  site_names <- list()
  member_count <- list(list())
  clusters_length <- c()
  for(i in 1:length(sub_dend)) {
    clusters[[i]] <- sub_dend[[i]]
    site_names[[i]] <- labels(clusters[[i]])
    clusters_length <- c(clusters_length, length(site_names[[i]]))
    member_count[[i]] <- length(site_names[[i]])
    # Store top-level branch sizes when the subdendrogram is not a leaf
    if (is.list(clusters[[i]]) && length(clusters[[i]]) >= 2) {
      for(j in 1:2){
        member_count[[i]][[j]] <- length(labels(clusters[[i]][[j]]))
      }
    }
  }

  # Match dendrogram branches to cluster colors
  labeles = c()
  colores_dend <- c()
  for (i in 1:length(clusters)) {
    A <- labels(clusters[[i]])
    for (j in 1:length(unique(grupos$Cluster))){
      B <- grupos$article[grupos$Cluster==j]
      if (all(A%in%B) == TRUE) {
        color <- unique(grupos$colores[grupos$Cluster == j])
        colores_dend <- append(colores_dend, color)
        labeles = append(labeles, j)
      }
    }
  }

  dend <- color_branches(dend, k = k, col = colores_dend, groupLabels = labeles)
  plot(dend, main = paste0("Dendrogram with ", k , " Clusters"))

  return(list(dend = dend, sub_dend = sub_dend, clusters = clusters,
              site_names = site_names, clusters_length = clusters_length))
}
