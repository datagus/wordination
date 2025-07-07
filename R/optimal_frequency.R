#-----Getting the optimal frequency----

optimal_frequency <- function(word_df, top_words){
  top_words <- top_words/100
  rows_number <- nrow(word_df)
  top <- round(nrow(word_df)*top_words,0) #change here to get the percentage of top words with highest abundance
  freq <- 1

  pb <- txtProgressBar(min = 0, max = 100, style = 3)  # create progress bar

  # While loop
  while (rows_number>=top) {
    moritz<- word_df[apply(ifelse(word_df>0,1,0),1,sum)>freq,]
    rows_number <- nrow(moritz)
    # Increment the counter to avoid infinite loop
    freq <- freq + 1
    # Update progress bar
    progress <- min(100, 100 - ((rows_number - top) / (nrow(word_df) - top)) * 100)
    setTxtProgressBar(pb, progress)
  }

  close(pb)  # close progress bar

  freq <- freq -1
  paste0("The higher frequency to get the", top_words*100, "is ", freq)
  return(list(freq = freq,
              words_number = rows_number
  ))
}
