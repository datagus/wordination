#------Creating abundance table----

abundance_table <-function(articles, papers_titles, npapers, articles_length){
  # Check if the input is a list
  if (!is.list(articles)) {
    stop("Input must be a list. Provided input is not a list.", call. = FALSE)
  }

  # Check if every element of the list is a data frame
  if (!all(sapply(articles, is.data.frame))) {
    stop("All elements of the list must be data frames.", call. = FALSE)
  }

  words_as_index <- function(data_frame, i){
    df           <- as.data.frame(data_frame[[i]]$Freq)
    colnames(df) <- names(data_frame[i])
    rownames(df) <- data_frame[[i]]$words
    return(df)
  }

  i <- 1
  start <- 1
  end <- npapers

  #Applying the function
  words_frequencies = list()
  j = 0
  deleted_papers <- 0
  new_articles <- articles
  for (i in start:end){
    print(i)
    if(length(row.names(articles[[i]])!=0)){
      j = j + 1
      words_frequencies[[j]] <- words_as_index(articles,i)}
    else{
      print (paste0("The ", as.character(i), "th list-entry is empty - following PDF seems not having been read rightly:  ", as.character(papers_titles[i]) ) )
      deleted_papers <- deleted_papers + 1
      new_articles <- new_articles[-i]
    }
  }
  npapers <- npapers - deleted_papers
  end <- npapers
  articles <- new_articles

  ##--CREATING SINGLE DATAFRAME WITH ALL THE ARTICLES' WORDS----

  for ( i in 1:(length(words_frequencies)-1) ){
    if (i == 1){
      df1 = words_frequencies[[i]]
      df2 = words_frequencies[[i+1]]}else{df2 = words_frequencies[[i+1]]}
    df1 <- merge(df1, df2, all=T,by="row.names",sort=F)
    # The merge function drops the row.names, and in turn adds a column named "row.names": Correct this!
    row.names(df1) <- df1[,1] #putting the words as the index (again)
    print(i)
    df1 <- df1[,-1]#removing the first column "row.names"
  }

  allwords <- df1
  rm(df1, df2)

  ##--CREATING ABUNDANCE TABLE----

  ###vector of unique words
  word_vector<-as.factor(sort(row.names(allwords))) #words sorted
  word_vector<- word_vector[which(nchar(as.character(word_vector))<20)] #all words less that 20 length
  word_vector<-as.factor(word_vector) #words as factor
  word_levels<-levels(word_vector) #words as levels

  ## abundance table
  word_df <- data.frame(words = word_levels)
  colnames(word_df)<-"words"

  #populating the table with all the words present in each article
  for (j in c(1:end)){
    article <-articles[[j]]
    article_filtered <-article[article$words %in% word_levels,]
    word_df <- merge(word_df, article_filtered, by="words",all.x=T)
    print(j)
  }

  word_df<-word_df[,2:(npapers+1)] #removing first column
  rownames(word_df) <- word_levels #putting the words as index

  #replacing NAs with 0 and renaming columns
  for (i in 1:npapers) word_df[is.na(word_df[,i]),i] <- 0
  colnames(word_df)<-names(articles)[c(1:npapers)]

  return(word_df)

}
