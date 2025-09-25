#----running DCA----


#' Running the Detrended Correspondence Analysis of the words abundance table
#'
#' @param x The abundance table. This table is not scaled nor normalized yet. It is recommended to have the optimal incidence already.
#' @param drop_words Vector containing the words to be dropped.
#' @param incidence The incidence threshold. Default set to 0.
#' @param logscaling logical. TRUE(default) for logarithmic scale of the abundance table
#' @importFrom vegan decorana
#'
#' @returns The resulting model from the Detrented Correspondance Analysis (DCA)
#'
#' @export
running_dca <- function(x, drop_words=c(), incidence=0, logscaling=TRUE){

  #ensure object is a dataframe
  stopifnot(is.data.frame(x))

  # ensure all columns are numeric
  if (!all(vapply(x, is.numeric, logical(1)))) {
    stop("All columns of `x` must be numeric (counts).")
  }
  if (nrow(x) == 0L || ncol(x) == 0L) stop("`x` has zero rows or columns.")

  #dropping_words
  x <- x[!rownames(x) %in% drop_words, 1:ncol(x), drop = FALSE]

  #subselecting the abundance table based on the incidence threshold indicated
  abundance <- x[rowSums(x > 0, na.rm = TRUE)>incidence,]

  #log scaling
  if(isTRUE(logscaling)){
    abundance<- log(abundance+1)
  }

  #transpossing the abundance table
  abundancet <- t(abundance)

  model<-vegan::decorana(abundancet,iweigh=0, ira=0)
  return(model)
}

#' @examples

