#' Create word abundance table
#'
#' @description
#' `abundance_table()` creates (1) an abundance table recording
#' the count of each word per article and (2) a summary table recording
#' the incidence and total abundance of each word across all articles.
#'
#' @details
#' The function expects the output of `import_words()`, i.e. a list of dataframes
#' with the words and word frequencies for each article and additional information
#' about those dataframes.
#'
#' It creates an abundance table with one column per article and one row per
#' word recording the count of each word per article.
#'
#' Additionally, it creates a table recording the incidence and total abundance of each word.
#' The incidence of a word is defined as the number of articles where the word
#' is present at least once (abundance > 0).  The total abundance of a word is defined as
#' the total count of a word summed across all articles.
#'
#' @param articles List of dataframes each containing two columns (words and Freq).
#' @param papers_titles Character vector. Names of the original text files (path excluded).
#' @param npapers Integer. Total number of dataframes.
#' @param articles_length Integer vector. Number of words/rows in each article dataframe.
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#'
#' @returns A list with two elements:
#'    \item{abtab}{The abundance table with the count of each word per article.}
#'    \item{moritz}{A dataframe with all words sorted by decreasing incidence and their total abundance across all articles.}
#'
#' @export
#'
#' @examples
#' # Create two small example article data frames
#' article1 <- data.frame(
#'   words = c("climate", "change", "policy"),
#'   Freq  = c(3, 2, 1),
#'   stringsAsFactors = FALSE
#' )
#'
#' article2 <- data.frame(
#'   words = c("climate", "energy", "policy"),
#'   Freq  = c(1, 4, 2),
#'   stringsAsFactors = FALSE
#' )
#'
#' articles <- list(article1 = article1, article2 = article2)
#'
#' # Create abundance table
#' result <- abundance_table(
#'   articles = articles,
#'   papers_titles = c("article1.txt", "article2.txt"),
#'   npapers = length(articles),
#'   articles_length = sapply(articles, nrow)
#' )
#'
#' # Abundance table
#' result$abtab
#'
#' # Summary table
#' result$moritz
#'
#'
#' \dontrun{
#' # Use your own frequency tables
#' # Assume you already imported your articles using import_words()
#' # and the results are stored in the variable "output"
#'
#' # Extract dataframes and additional information
#' articles_dfs <- output$articles_dfs
#' papers_titles <- output$papers_titles
#' npapers <- output$npapers
#' articles_length <- output$article_length
#'
#' # Create abundance table
#' words <- abundance_table(articles_dfs, papers_titles, npapers, articles_length)
#'
#' # Extract abundance and summary table
#' words_df <- words$abtab
#' moritz <- words$moritz
#' }
abundance_table <-function(articles, papers_titles, npapers, articles_length){

  # 1.-Check input is a list
  if (!is.list(articles)) {
    stop("Input must be a list. Provided input is not a list.", call. = FALSE)
  }

  # 2.-Check all elements are dataframes
  if (!all(sapply(articles, is.data.frame))) {
    stop("All elements of the list must be data frames.", call. = FALSE)
  }

  # 3.-Convert single article to word-indexed frequency table
  abundant_table <- function(list_dataframes, i){
    df <- as.data.frame(list_dataframes[[i]]$Freq)
    colnames(df) <- names(list_dataframes[i])
    rownames(df) <- list_dataframes[[i]]$words
    return(df)
  }

  # 4.-Remove empty dataframes and update list
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

  # 5.-Merge the frequency tables of all articles iteratively into one abundance table
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

  # 6.-Replace NAs with 0 and compute summary table
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
