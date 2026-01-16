#-----Getting the incidence threshold----

#' Subselecting the abundance table based on the percentage of words' incidence
#'
#' This function computes the incidence (frequency) of each word across all articles in an abundance table.
#' The incidence of a word is defined as the number of articles where the word is present at least once (abundance > 0).
#' The maximum possible incidence for any word is equal to the total number of articles.
#'
#' Knowing the words incidence helps to determine a threshold and...
#' ...if the words with high incidence contain meaning in the context of the research topic.
#' Therefore, one can subselect the abundance table with only those words with the highest incidence.
#' Keeping words with low incidence will disturb the ordination and clustering process.
#'
#' In this sense, this function takes as arguments, the abundance table the and...
#' ...the desired percentage of most common words (words with higher incidence)
#' For example, if you want the 20% most common words.
#'
#' @param x two dimensional matrix representing the abundance table
#' @param most_common the porcentage given in integer in a range from 1 to 100
#'
#' @returns The incidence threshold and the resulting total number of words if you apply the incidence threshold

#' @export

optimal_incidence <- function(x, most_common=50){

  #this a control to stop the function if the criteria are not met
  stopifnot(is.data.frame(x),
            is.numeric(most_common), most_common > 0, most_common <= 100)

  # ensure all columns are numeric
  if (!all(vapply(x, is.numeric, logical(1)))) {
    stop("All columns of `x` must be numeric (counts).")
  }
  if (nrow(x) == 0L || ncol(x) == 0L) stop("`x` has zero rows or columns.")

  # how many words represent the top_prop
  top_prop <- most_common / 100
  top_words   <- ceiling(nrow(x) * top_prop)

  # Incidence: It produced a count of words appearances where it occurs at least once
  inc <- rowSums(x > 0, na.rm = TRUE)

  # creating the cutoff: sorting by incided using the number of words corresponding to most_common
  cutoff_incidence <- sort(inc, decreasing = TRUE)[top_words]

  incidence_threshold = unname(cutoff_incidence)

  # Getting the words indexes that greater than the cutoff.
  keep_idx <- which(inc > incidence_threshold)

  # Filter only (preserve original row order)
  abtable <- x[keep_idx, , drop = FALSE]

  w_number <- nrow(abtable)

  message(
    "The threshold incidence for the ",most_common," % articles is ", incidence_threshold, " resulting also in ", w_number, " total words")

  return(list(incidence_threshold, w_number))
}

#' @examples to be added

