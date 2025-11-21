#------Creating abundance table----

#' Creating the word abundance table
#'
#' This function retrieves all the text files from an specified path and creates a list of
#' dataframes.
#'
#' @param articles List of dataframes where each dataframe contains two columns (words and Freq)
#' @param papers_titles Name of the original text files (path excluded). They will be used to build the header of the abundance table columns
#' @param npapers Total number of dataframes
#' @param articles_length Vector contained the length (number of words/rows) of each dataframe
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#'
#' @returns A list with two objects:
#'
#' $abtab: The abundance table
#'
#' $moritz: A dataframe with all the words sorted by incidence (descending). It also contains the total abundance of all words across all articles
#'
#' @export
abundance_table <-function(articles, papers_titles, npapers, articles_length){

  # 1.-Check if the input is a list
  if (!is.list(articles)) {
    stop("Input must be a list. Provided input is not a list.", call. = FALSE)
  }

  # 2.-Check if every element of the list is a data frame
  if (!all(sapply(articles, is.data.frame))) {
    stop("All elements of the list must be data frames.", call. = FALSE)
  }

  # 3.- This function takes a list of dataframes and puts the first column (in this case, words) as index then the column is removed
  # Then it renames the remaining column with the dataframe name
  # This function is to be used when iterating a list of dataframes

  abundant_table <- function(list_dataframes, i){
    df <- as.data.frame(list_dataframes[[i]]$Freq)
    colnames(df) <- names(list_dataframes[i])
    rownames(df) <- list_dataframes[[i]]$words
    return(df)
  }

  # 4.- Removing dataframes with zero words and updating list of dataframes----
  # A for loop will iterate over the list of dataframes contained in the variable/parameter articles, and remove the empty ones
  # Initializing the for loop:
  end <- npapers
  words_frequencies = list() #list containing the new dataframes (words as index and the column as the name of the original file)
  j = 0 #counter to keep things in order when interating if a dataframe is removed
  deleted_papers <- 0 #to keep a count of deleted articles or dataframes: dataframes with zero words/rows
  new_articles <- articles #copy of the list of dataframes, needed if there are dataframes to be deleted

  print("Checking for empty dataframes")
  pb <- txtProgressBar(min = 0, max = 100, char = "=", style = 3) #Progressbar
  for (i in 1:end){
    if(length(row.names(articles[[i]])!=0)){
      j = j + 1
      words_frequencies[[j]] <- abundant_table(articles,i)}
    else{
      print (paste0("The ", as.character(i), "th list-entry is empty - following PDF seems not having been read rightly:  ", as.character(papers_titles[i]) ) )
      deleted_papers <- deleted_papers + 1
      new_articles <- new_articles[-i]
    }
    progress <- i/end *100
    setTxtProgressBar(pb, progress)
  }
  close(pb)
  npapers <- npapers - deleted_papers #updated the count of dataframes
  end <- npapers
  articles <- new_articles #updating the list of dataframes in case one was removed.

  # 5.- Creating abundance_table-----
  # This section merges each dataframe, one by one, until getting a raw abundance table
  # The trick is dealing with the first two dataframes
  # The first iterations takes the first two dataframes, df1 and df2, and merges them...
  # ...by row names. The new merged tables is rewritten to df1, so for the next iteration...
  # ... that is, when i!=1, the next dataframe, df2, will be merged to the previosly merged one.
  # After every merge, the resulting dataframe is rewritten to df1.
  # The first column, that is, the words, are then passed to index and then the column is removed.
  # One completed the final merged dataframe is rewritten to abundance_raw

  print("Creating abundance_table")
  pb <- txtProgressBar(min = 0, max = 100, char = "=", style = 3)

  # Check how many valid papers remain
  n_remaining <-length(words_frequencies)

  if (n_remaining == 0){
    stop("no valid dataframes remaining after filtering empty files.")
  } else if (n_remaining == 1) {
    df1 <- words_frequencies[[1]] # If only one paper, no merging needed. Just formatting.
    setTxtProgressBar(pb, 100)
  } else {
    # Run loop merge loop only if we have 2 or more papers
    for ( i in 1:(length(words_frequencies)-1) ){
      if (i == 1){
        df1 = words_frequencies[[i]]
        df2 = words_frequencies[[i+1]]}else{df2 = words_frequencies[[i+1]]}
      df1 <- merge(df1, df2, all=T,by="row.names",sort=F)
      row.names(df1) <- df1[,1] #putting the words as the index (again)
      df1 <- df1[,-1] #removing the first column "row.names"
      progress <- (i+1)/end *100
      setTxtProgressBar(pb, progress)
    }
  }
  close(pb)
  abundance_raw <- df1
  abundance_raw <- abundance_raw[order(row.names(abundance_raw)), , drop = FALSE] #Added drop = FALSE to prevent 1-column dataframe from becoming a vector

  # 6.- replacing NAs with 0 and renaming columns
  print("Refining abundance table")
  pb <- txtProgressBar(min = 0, max = 100, char = "=", style = 3)
  for (i in 1:npapers) {
    abundance_raw[is.na(abundance_raw[, i]), i] <- 0
    progress <- i/end *100
    setTxtProgressBar(pb, progress)
  }
  close(pb)

  y <- data.frame(
    words = rownames(abundance_raw),
    incidence = rowSums(abundance_raw > 0, na.rm = TRUE),
    total_abundance = rowSums(abundance_raw, na.rm = TRUE),
    stringsAsFactors = FALSE,
    row.names = NULL,
    check.names = FALSE
  )

  moritz <- y[order(y$incidence, y$total_abundance, y$words, decreasing = TRUE), ]
  rownames(moritz) <- NULL

  return(list(abtab=abundance_raw,
              moritz = moritz))

}
#' @examples
