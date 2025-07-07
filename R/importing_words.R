#------Importing files and words----
#' Import the text files
#'
#' @param folder_path The folder path or directory where your text files are
#' @param remove_papers A vector containg the names of files you don't want to include
#'
#' @return A list of our elements: article_length, articles_dfs, papers_titles, npapers
#' @export

importing_words <- function(folder_path,remove_papers=c(), no_outliers = FALSE){
  all_titles <- list.files(folder_path, pattern = "\\.txt$", all.files=FALSE, full.names = TRUE)#files_names are storied into this list
  papers_titles <- list() #empty list for the filtered titles
  articles <- list() # empty list where articls are going to be stored
  i=1
  start=1
  for (i in c(i:length(all_titles))){
    method <- all_titles[i]
    papers_titles <- append(papers_titles, all_titles[i])
  }
  papers_titles <- as.character(papers_titles)
  remove_papers <- file.path(folder_path, remove_papers)

  condicional <- TRUE

  while (condicional == TRUE){

    papers_titles <- papers_titles[!papers_titles %in% remove_papers]
    npapers <- length(papers_titles)
    end <- npapers
    article_length <- c()

    for (i in c(start:end)){
      try({
        doc <- scan(papers_titles[i], what = "character", sep = "\n", quiet=TRUE) #importing the .txt file
        doc <- tolower(doc)
        doc <- paste(doc, collapse=" ") #separating each character by " "
        doc <- gsub("- ", "", doc)  # undo hyphenation
        doc <- gsub('\f', " ", doc) #form feed character
        doc <- gsub("[^A-Za-z[:space:]]", " ", doc) #removing all non-words and non-spaces
        doc <- gsub(" +", " ", doc) #removing all more than 1 spaces
        doc <- gsub('\n', " ", doc) #removing paragraph jump
        doc <- trimws(doc) #removing all the spaces at the beginning and end of the string
        #doc <- tolower(doc) #converting to lowercase
        words <- strsplit(doc," ") #splitting the whole string into single characters

        articles[[i]] <- data.frame(table(words))
        wn <- nrow(articles[[i]]) # wn stands for words number in the article
        article_length <- c(article_length, wn) #vector with the length of each paper
        names(articles)[i] <- basename(papers_titles[i])
        print(i)
      })
    }

    if (length(boxplot.stats(article_length)$out) > 0){
      to_remove <- boxplot.stats(article_length)$out
      remove_papers <- append(remove_papers,papers_titles[article_length %in% to_remove])
      condicional <- no_outliers
    } else{
      condicional <- FALSE
    }
  }

  papers_titles2 <- basename(papers_titles)

  return(list(
    article_length = article_length,
    articles_dfs = articles,
    papers_titles = papers_titles2,
    npapers = npapers
  ))
}

#' @examples
#'
#' folder_path = "/Users/jorgegustavorodriguezaboytes/example"
#' output <- importing_words(folder_path)
