#' Determine the incidence threshold for word filtering
#'
#' This function computes the incidence (frequency) of each word across all articles in an abundance table.
#' The incidence of a word is defined as the number of articles where the word is present at least once (abundance > 0),
#' regardless of how many times it appears within a single article.
#' The maximum possible incidence for any word is equal to the total number of articles.
#'
#' Words with low incidence tend to disturb the ordination and clustering process.
#' This function helps identify an incidence threshold based on a desired percentage of the most common words.
#' For example, setting \code{most_common = 20} returns the threshold corresponding to the top 20% most common words.
#'
#' @param x Data frame. The abundance table with words as rows and articles as columns.
#'  All columns must be numeric.
#' @param most_common Numeric. The percentage of most common words to consider,
#'  in a range from 1 to 100. Default is 50.
#'
#' @returns A list with two elements:
#' \describe{
#'   \item{incidence_threshold}{The incidence threshold value.}
#'   \item{words_remaining}{The number of words remaining after applying the threshold.}
#' }
#'
#' @export
#'
#' @examples
#' data(transformative_abundance)
#' optimal_incidence(transformative_abundance, most_common = 20)
#'
optimal_incidence <- function(x, most_common=50){

  stopifnot(is.data.frame(x),
            is.numeric(most_common), most_common > 0, most_common <= 100)

  # ensure all columns are numeric
  if (!all(vapply(x, is.numeric, logical(1)))) {
    stop("All columns of `x` must be numeric (counts).")
  }
  if (nrow(x) == 0L || ncol(x) == 0L) stop("`x` has zero rows or columns.")

  top_prop <- most_common / 100
  top_words   <- ceiling(nrow(x) * top_prop)

  # Count articles where each word occurs at least once
  inc <- rowSums(x > 0, na.rm = TRUE)

  # Threshold: incidence of the word at the top_prop cutoff
  cutoff_incidence <- sort(inc, decreasing = TRUE)[top_words]
  incidence_threshold = unname(cutoff_incidence)

  # Keep only words with incidence strictly above the threshold
  keep_idx <- which(inc > incidence_threshold)
  abtable <- x[keep_idx, , drop = FALSE]
  w_number <- nrow(abtable)

  message(
    "The threshold incidence for the ", most_common,
    "% most common words is ", incidence_threshold,
    ", resulting in ", w_number, " words remaining.")

  return(list(incidence_threshold = incidence_threshold, words_remaining = w_number))
}
