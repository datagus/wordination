#------Importing files and words----
#' Import the text files
#'
#' This function retrieves all the text files from an specified path and creates a list of
#' dataframes.
#'
#'
#' @param folder_path The folder path or directory where your text files are. It is not the same as the working directory.
#' @param remove_papers A vector containg the names of files you want to exclude from importing.
#' @param remove_outliers Boolean. TRUE if you want that the function iterates until excluding all the
#' files that have extreme values regarding the number words. FALSE if you want to keep the outliers
#' and remove them yourself.
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom grDevices boxplot.stats
#'
#' @return A list of our elements: article_length, articles_dfs, papers_titles, npapers.
#'
#' To access the element you have to assign a variable to this function, for example, output <- importing_words(folder_path, no_outliers=TRUE).
#'
#' You can acess then the different elements as output$article_length. The description of these elements is as follows:
#'
#' $article_length: A vector with the word count in each text file. You can plot its distribution.
#'
#' $articles_dfs: A list containing all the datafames. Each dataframe has two columns:
#' "words" which contains all the words and "Freq" which the frequency of each word.
#'
#' $npapers: The total number of articles imported
#'
#' $articles_length: A vector containing the number of total number of words (not unique) in each article
#'
#'
#' @export

importing_words <- function(folder_path,remove_papers=c(), remove_outliers = FALSE){

  # 1.- Importing all text files using the list.files() function given any folder path.----
  # The text files are going to be stored in the vector all_titles
  # The argument pattern is the a regex pattern="\\.txt$" to identify only text files.
  # The argument all.files=FALSE to avoid importing hidden files
  # The argument full.names=TRUE to add the folder_path to the file name name, otherwise, the import won't function
  # This latter argument is important to avoid setting the folder containing the files as working directory.

  all_titles <- list.files(folder_path, pattern = "\\.txt$", all.files=FALSE, full.names=TRUE)

  # 2.- An empty list is created to store the articles names with its full path --> papers_titles ----
  # A for loop is written to iterate over each text file contained in the vector all_titles...
  # ...to store into the list papers_titles, which is then is converted to caracter vector
  # The parameter vector remove_papers is read and added the folder path. It is overwritten.

  papers_titles <- list()
  i=1
  for (i in c(i:length(all_titles))){
    papers_titles <- append(papers_titles, all_titles[i])
  }
  papers_titles <- as.character(papers_titles)
  remove_papers <- file.path(folder_path, remove_papers)

  # 3.- A boolean variable is set: condicional <- TRUE. While this variable remains TRUE...----
  # ...the for loop described in #4.- will continue running
  # A text progress bar is initialized with the function txtProgressBar() and saved into the pb variable.
  # Above it is the printing message for the user.
  # Moreover, when the while loop runs, the aforementioned papers_titles vector will be updated...
  # ...excluding those files indicated in the vector remove_papers
  # The length of the vector papers_titles is saved into npapers
  # The variables start and end are going to control the range of iterations of the loop described in #4.-
  # An empty list, articles, is created to store the data frames...
  # ...of the imported files (words and frequency as the respective columns)
  # An empty vector, article_length, is initialized. This will contained the number of words in each file that is read.

  condicional <- TRUE
  print("Importing files")
  pb <- txtProgressBar(min = 0, max = 100, char = "=", style = 3)
  while (condicional == TRUE){
    papers_titles <- papers_titles[!papers_titles %in% remove_papers]
    npapers <- length(papers_titles)
    end <- npapers
    articles <- list()
    article_length <- c()


  # 4.- For loop to read and import each file and build a respective dataframe with columns $words and $Freq----
  # The function scan() is used to read the data or words of each file, with the following arguments:
  # papers_titles[i] correspond to each file contained in the vector papers_titles
  # what = "characters" is used to indicate that data to be read is of character type
  # sep = " " is used to indicate that the data or words are separated by a single blank space
  # quiet = TRUE to avoid an overload of prints in the console everytime a file is read

  # All the words read from each file are saved into the variable doc.
  # The subsequent steps are explained in inline comments.

    for (i in c(1:end)){
      try({
        doc <- scan(papers_titles[i], what = "character", sep = " ", quiet=TRUE)
        doc <- tolower(doc) # converting to lower case
        doc <- paste(doc, collapse=" ") #separating each character by " "
        doc <- gsub("- ", "", doc)  # undo hyphenation
        doc <- gsub('\f', " ", doc) #form feed character
        doc <- gsub("[^A-Za-z[:space:]]", " ", doc) #removing all non-words and non-spaces
        doc <- gsub(" +", " ", doc) #removing all more than 1 spaces
        doc <- gsub('\n', " ", doc) #removing paragraph jump
        doc <- trimws(doc) #removing all the spaces at the beginning and end of the string
        words <- strsplit(doc," ") #splitting the whole string into single characters to store them...
        #...into a single column (one word per colummn)

        articles[[i]] <- data.frame(table(words)) #creating the specific dataframe...
        #...table(splitted) provides a frequency table of the words contained in the variable "splitted"

        number_of_words <- nrow(articles[[i]]) # the number of unique words in the respective dataframe
        article_length <- c(article_length, number_of_words) #this vector contains the number of words of each dataframe of all files
        names(articles)[i] <- basename(papers_titles[i]) #adding the name to the respective dataframe

        progress <- i/end *100 #setting the progress indicator
        setTxtProgressBar(pb, progress) #setting the progress bar to be shown
      })
    }
    close(pb) #closing progress bar

    # 5.- Removing outliers automatically (we are still inside the while)----
    # This section is a if-else block to detect if there are outliers regarding the number of words in each dataframe
    # If the parameter specified in the function, remove_outliers, is TRUE then:...
    # ...another if-else block is used to detect if there are actually some outliers...
    # ...if there is any outlier, the length of boxplot.stats(article_length)$out is more than zero...
    # ...then, the outliers values are stored in the variable to_remove...
    # ...the parameter, remove_papers, is updated by appending the respective elements...
    # ...the variable condicional remains TRUE to keep the loops running.
    # When there are no more outliers, the condicional is set to FALSE and the loops will stop running
    # if the remove_outliers parameter is FALSE from the beginning, then the condicional variable will be FALSE

    if(remove_outliers == TRUE){
      if (length(boxplot.stats(article_length)$out) > 0){
        print("Removing outliers")
        to_remove <- boxplot.stats(article_length)$out
        remove_papers <- append(remove_papers,papers_titles[article_length %in% to_remove])
        condicional <- remove_outliers
      } else{
        condicional <- FALSE
        print(paste0("A total of ", length(remove_papers), " outliers were removed"))
        remove_papers2 <- basename(remove_papers)
      }
    } else{
      condicional <- FALSE
      remove_papers2 <- c()
    }
  }


  return(list(
    article_length = article_length,
    articles_dfs = articles,
    papers_titles = basename(papers_titles), #basename() function is to retrieve the name of the file and its full path
    npapers = npapers,
    papers_removed = remove_papers2
  ))
}

#' @examples
#'
#' folder_path = "/Users/jorgegustavorodriguezaboytes/example"
#' output <- importing_words(folder_path)
