#----running DCA----


#' Running the Detrended Correspondence Analysis of the words abundance table
#'
#' @param word_df a matrix representing the abundance table
#' @param drop_words a vector of words that the user may want to remove from the abundance table
#' @param freq the minimum word's frequency across all articles
#' @param sumrows integer value indicating the overall frequency value of the word in order to be removed from the matrix
#' @param logscaling logical. TRUE(default) for logarithmic scale of the abundance table
#' @importFrom vegan decorana
#' @returns model, moritz and abundancet
#' @export
running_dca <- function(word_df, drop_words = c(), freq,sumrows=0, logscaling=TRUE){

  word_df <- word_df[!rownames(word_df) %in% drop_words, 1:ncol(word_df), drop = FALSE]

  ## Correcting the frequencies of words by the total words in each article
  word_df <- sweep(word_df, 2, articles_length, "/")
  word_df <- word_df*1000

  moritz<- word_df[apply(ifelse(word_df>0,1,0),1,sum)>freq,]
  abundance <- moritz
  abundancet <- t(abundance)
  abundancet <- abundancet[rowSums(abundancet)>0,]
  if(logscaling==TRUE){
    abundancet<- log(abundancet+1)
  }
  #eliminating rows or articles with empty values
  abundancet <- abundancet[rowSums(abundancet) > sumrows, ]
  model<-vegan::decorana(abundancet,iweigh=0, ira=0)
  return(list(model = model,
              moritz = moritz,
              abundancet = abundancet))
}

#' @examples

