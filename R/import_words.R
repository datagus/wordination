#' Import text files as word-frequency dataframes
#'
#' This function reads all text files from a specified directory, performs basic
#' text cleaning (lowercase, remove punctuation and numbers), and creates a
#' word-frequency dataframe for each file. Optionally, files with extreme word counts can be
#' detected and automatically removed using boxplot statistics.
#'
#' @param folder_path Character string. The directory path containing the text files to import.
#' @param remove_papers Character vector. Filenames (without path) to exclude from importing.
#'  Default is an empty vector.
#' @param words_to_ignore Character string. The directory path containing a text files with words that you don't want to import.n
#' @param remove_outliers Logical. If \code{TRUE}, files with extreme word counts
#'  (detected via \code{link[grDevices]{boxplot.stats}}) are iteratively removed.
#'  If \code{FALSE} (default), all files are kept.
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom grDevices boxplot.stats
#'
#' @return A list with five elements:
#' \describe{
#'  \item{article_length}{Integer vector. The number of unique words in each imported file.}
#'  \item{articles_dfs}{List of dataframes, each with columns \code{words} and \code{Freq}.}
#'  \item{papers_titles}{Character vector. Filenames (basename only) of the imported files.}
#'  \item{npapers}{Integer. Total number of imported files.}
#'  \item{papers_removed}{Character vector. Filenames of files removed as outliers
#'     (empty if \code{remove_outliers = FALSE}).}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Import all text files from a directory
#' result <- import_words("path/to/text_files")
#' result$npapers
#'
#' # Exclude specific files
#' result <- import_words("path/to/text_files", remove_papers = c("bad_file_1.txt", "bad_file_2.txt"))
#' result$papers_titles    # files that were imported
#'
#' # Automatically remove outliers
#' result <- import_words("path/to/text_files", remove_outliers = TRUE)
#' result$papers_removed
#' }
import_words <- function(folder_path, remove_papers = c(), words_to_ignore=nullfile(), remove_outliers = FALSE) {

  # Get all .txt files with full paths
  all_titles <- list.files(folder_path, pattern = "\\.txt$",
                           all.files = FALSE, full.names = TRUE)

  generic_words <- scan(words_to_ignore, what = "character", sep = " ", quiet=TRUE)

  papers_titles <- as.character(all_titles)
  remove_papers <- file.path(folder_path, remove_papers)

  condicional <- TRUE
  print("Importing files")
  pb <- txtProgressBar(min = 0, max = 100, char = "=", style = 3)

  outliers_length <- 0
  while (condicional == TRUE) {
    papers_titles <- papers_titles[!papers_titles %in% remove_papers]
    npapers <- length(papers_titles)
    end <- npapers
    articles <- list()
    article_length <- c()

    # Read each file, clean text, build word-frequency table
    for (i in c(1:end)) {
      try({
        doc <- scan(papers_titles[i], what = "character", sep = " ", quiet = TRUE)
        doc <- tolower(doc)
        doc <- paste(doc, collapse = " ")
        doc <- gsub("- ", "", doc)                    # undo hyphenation
        doc <- gsub('\f', " ", doc)                    # remove form feed
        doc <- gsub("[^A-Za-z[:space:]]", " ", doc)    # keep only letters and spaces
        doc <- gsub(" +", " ", doc)                    # collapse multiple spaces
        doc <- gsub('\n', " ", doc)                    # remove newlines
        doc <- trimws(doc)
        words <- strsplit(doc, " ")
        words <- lapply(words, function(x) x[!x %in% generic_words]) #ignoring the words

        articles[[i]] <- data.frame(table(words))
        article_length <- c(article_length, nrow(articles[[i]]))
        names(articles)[i] <- basename(papers_titles[i])

        setTxtProgressBar(pb, i / end * 100)
      })
    }
    close(pb)

    # Iteratively remove files with extreme word counts (boxplot outliers)
    if (remove_outliers == TRUE) {
      if (length(boxplot.stats(article_length)$out) > 0) {
        print("Removing outliers")
        to_remove <- boxplot.stats(article_length)$out
        outliers_length <- outliers_length + length(to_remove)
        remove_papers <- append(remove_papers, papers_titles[article_length %in% to_remove])
        condicional <- remove_outliers
      } else {
        condicional <- FALSE
        print(paste0("A total of ", outliers_length, " outliers were removed"))
        remove_papers2 <- basename(remove_papers)
      }
    } else {
      condicional <- FALSE
      remove_papers2 <- c()
    }
  }

  return(list(
    article_length = article_length,
    articles_dfs = articles,
    papers_titles = basename(papers_titles),
    npapers = npapers,
    papers_removed = remove_papers2
  ))
}
