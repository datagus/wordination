#-----showing numbers in cluster and indicators-----
cluster_indicators <- function(abundancet,nk){
  set.seed(123)
  modelclust<-cluster::agnes(abundancet,method="ward")
  cutmodel <-stats::cutree(modelclust,k=nk)
  print(table(cutmodel))
  temp5<-labdsv::indval(abundancet,cutmodel,numitr=1000)
  summary(temp5)
}
