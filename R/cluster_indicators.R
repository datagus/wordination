#' Calculate word abundance indicators per cluster
#'
#' This file function computes indicator values for words across clusters using an
#' adaptation of the Dufrene-Legendre Indicator Species Analysis (\code{\link[labdsv]{indval}})
#' applied to textual abundance data. Clustering is performed internally using Ward's method via
#' \code{link[cluster]{agnes}}.
#'
#' @details
#' A higher indicator value means that the cluster is highly unique while a lower value indicates the cluster is sparse
#' or has more variance. If a cluster does not appear in the output, its indicator value is zero. If any cluster has an
#' indicator of zero, the respective cluster arrangement should not be considered.
#'
#' @param x Data frame. The abundance table with words as rows and articles as columns.
#'  All columns must be numeric. The table should not be scaled or normalized beforehand.
#'  It is recommended to apply an ancidence threshold using \code{\link{optimal_incidence}}
#'  before passing the table to this function.
#' @param k Integer. The number of clusters.
#' @importFrom cluster agnes
#' @importFrom stats cutree
#' @importFrom labdsv indval
#'
#' @returns A summary of an \code{indval} object from the \pkg{labdsv} package.
#'  See \code{\link{optimal_incidence}}, \code{\link{clusters_barplot}}
#'
#' @seealso \code{\link{optimal_incidence}}, \code{\link{clusters_barplot}}
#'
#' @export
#'
#' @examples
#' data(transformative_abundance)
#' #Selecting 6 clusters
#' cluster_indicators(transformative_abundance, k = 5)
#'
cluster_indicators <- function(x,k){

  # Convert to relative frequencies per article (per 1000 words)
  pa <- (x > 0) * 1
  sites_length <- unname(colSums(pa, na.rm = TRUE))
  abundance <- sweep(x, 2, sites_length, "/")
  abundance <- abundance * 1000

  #Log-transform and transpose (articles as rows)
  abundancet <- log(t(abundance) + 1)

  set.seed(123)
  #running the clustering
  modelclust<-agnes(abundancet, method = "ward")
  cutmodel <-cutree(modelclust, k = k)
  temp5<-labdsv::indval(abundancet, cutmodel, numitr = 1000)
  return(summary(temp5))
}
