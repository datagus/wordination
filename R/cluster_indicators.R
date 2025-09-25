#-----cluster indicators-----

#' Calculating the abundance indicators of words in the clusters
#'
#' It is basically an adaptation of the Dufrene-Legendre Indicator Species Analysis to textual abundance analysis.
#'
#' @param x The abundance table. This table is not scaled nor normalized yet, but it has the optimal words incidence.
#' Recommended to use the output from function optimal_incidence()
#' @param k The number of clusters
#' @importFrom cluster agnes
#' @importFrom stats cutree
#' @importFrom labdsv indval
#'
#' @returns Returns the same object as in the indval function. Interpretation: A higher indicator means that the cluster is very unique while a lower
#' a lower indicator means that cluster is sparse or with high variance. If one cluster doesn't appear, it meas that it has indicator of value zero.
#' If one of the cluster has zero, do not consider the respective cluster arrangement.
#' @export

cluster_indicators <- function(x,k){

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
  return(summary(temp5))
}
#' @examples to be added

