#---Refinig Abundance Table---

#' Customizing abundance table
#'
#' This function allows the user to customize the abundance table by:
#'  -removing words
#'  -subselecting the abundance table base on the incidence threshold
#'  -converting the abundances to relative abundances
#'  -scaling the abundance table by applying a logarithm
#'
#' @param x the abundance table in its raw form
#' @param incidence The incidence threshold
#' @param drop_words Vector containing the words to be dropped.
#' @param site_correction logical. FALSE (default) and TRUE if you want that the relative abundances are calculated by dividing each abundance the total number of words present in each article.
#' @param multiplier integer. The scale in which the correct abundances are to be shown. By default, the abundance is shown by 1000 species in each site.
#' @param logscaling logical. TRUE(default) for logarithmic scale of the abundance table
#'
#' @returns An abundance table. For running the Detrented Correspondance Analysis, the abundance table
#' needs to have site_correction=FALSE and logscaling=TRUE. For running the clustering, species indicators the abundance table needs have
#' site_correction=FALSE and logscaling=FALSE
#'
#'
#' @export
custom_abundance <- function(x, drop_words=c(),incidence=0, site_correction = FALSE, multiplier=1, logscaling=FALSE){

  #this a control to stop the function if the criteria are not met
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


  ## Correcting the frequencies of words by the total words in each article (word richness)
  if (isTRUE(site_correction)) {
    pa <- (abundance > 0) * 1
    sites_length <- unname(colSums(pa, na.rm = TRUE))
    abundance <- sweep(abundance, 2, sites_length, "/")
    abundance <- abundance*multiplier
    #changing the multiplier doesn't have an effect on the dca
  }

  #log scaling
  if(isTRUE(logscaling)){
    abundance<- log(abundance+1)
  }

  # Moritz table
  y <- data.frame(
    words = rownames(abundance),
    incidence = rowSums(abundance > 0, na.rm = TRUE),
    total_abundance = rowSums(abundance, na.rm = TRUE),
    stringsAsFactors = FALSE,
    row.names = NULL,
    check.names = FALSE
  )

  new_moritz <- y[order(y$incidence,decreasing = TRUE), ]
  rownames(new_moritz) <- NULL

  return(list(new_abundance = abundance, new_moritz = new_moritz))
}
#' @examples to be added
