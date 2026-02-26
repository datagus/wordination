#' Customize word abundance table
#'
#' @description
#' Allows to customize an abundance table by (1) removing words, (2) applying an
#' incidence threshold, (3) converting abundances to relative abundances,
#' (4) re-scaling relative abundances, (5) log-scaling abundances.
#'
#' @details
#' The function provides several options for customizing an abundance table
#' (created by the function `abundance_table()`):
#'
#' \enumerate{
#'   \item Remove user-defined words, e.g. irrelevant words that have not yet
#'   been filtered out by cleaning steps before.
#'   \item Sub-select the abundance table keeping only words with a sufficiently high incidence.
#'   \item Convert absolute word abundances to relative word abundances compared
#'   to the total length of the article (site correction). Relative abundances are calculated by
#'   dividing each abundance by the total number of words present in each article.
#'   \item Re-scale relative abundances using a multiplier (e.g., per 1,000 words).
#'   \item Log-scaling the abundances (\code{log(x + 1)}).
#' }
#'
#' Filtering by incidence (= number of articles in which a word occurs at least once)
#' is highly recommended because words with low incidence tend to
#' disturb the ordination and clustering process. An appropriate threshold can be
#' determined using \code{optimal_incidence()}.
#'
#' For running Detrended Correspondence Analysis, the abundance table
#' needs to have \code{site_correction = FALSE} and \code{logscaling = TRUE}.
#' For running the clustering and species indicator analysis the abundance table
#' needs to have \code{site_correction = FALSE} and \code{logscaling = FALSE}.
#'
#' @param x Data frame. The raw abundance table (rows = words, columns = articles).
#' @param drop_words Character vector. Words to remove from the table.
#' @param incidence Integer. The incidence threshold, i.e. minimum number of articles in which a word must
#'   occur (abundance > 0) to be retained.
#' @param site_correction Logical. If \code{TRUE}, convert counts to relative
#'   abundances per article (default: \code{FALSE}).
#' @param multiplier Numeric. Scaling factor applied to relative abundances (default: 1).
#' @param logscaling Logical. If \code{TRUE}, apply log-transformation
#'   (\code{log(x + 1)}) to the abundance table (default: \code{FALSE}).
#'
#' @returns A named list with:
#'   \item{new_abundance}{The customized abundance table.}
#'   \item{new_moritz}{A summary table containing words, incidence,
#'   and total abundance, sorted by decreasing incidence.}
#'
#' @export
#'
#' @examples
#' # Example 1: simple input
#' abtab <- data.frame(
#'   article1 = c(3, 2, 1, 5),
#'   article2 = c(1, 4, 2, 0),
#'   row.names = c("climate", "change", "policy", "irrelevant"),
#'   stringsAsFactors = FALSE
#' )
#' result <- custom_abundance(
#'   x = abtab,
#'   drop_words = c("irrelevant"),
#'   incidence = 0,
#'   site_correction = FALSE,
#'   multiplier = 1,
#'   logscaling = TRUE
#' )
#' result$new_abundance
#' result$new_moritz
#'
#' \dontrun{
#' # Example 2: user input
#' abtab <- your_abundance_table
#' result <- custom_abundance(
#'   x = abtab,
#'   drop_words = c(),
#'   incidence = 2,
#'   site_correction = FALSE,
#'   multiplier = 1,
#'   logscaling = TRUE
#' )
#' words_df2 <- result$new_abundance
#' moritz2 <- result$new_moritz
#' }
custom_abundance <- function(x, drop_words=c(),incidence=0, site_correction = FALSE, multiplier=1, logscaling=FALSE){

  #this a control to stop the function if the criteria are not met
  stopifnot(is.data.frame(x))

  # ensure all columns are numeric
  if (!all(vapply(x, is.numeric, logical(1)))) {
    stop("All columns of `x` must be numeric (counts).")
  }
  if (nrow(x) == 0L || ncol(x) == 0L) stop("`x` has zero rows or columns.")

  #dropping_words
  x <- x[!rownames(x) %in% drop_words, 1:ncol(x), drop = FALSE]

  #subselecting the abundance table based on the incidence threshold indicated
  abundance <- x[rowSums(x > 0, na.rm = TRUE)>incidence,]


  ## Correcting the frequencies of words by the total words in each article (word richness)
  if (isTRUE(site_correction)) {
    pa <- (abundance > 0) * 1
    sites_length <- unname(colSums(pa, na.rm = TRUE))
    abundance <- sweep(abundance, 2, sites_length, "/")
    abundance <- abundance*multiplier
    #changing the multiplier doesn't have an effect on the dca
  }

  #log scaling
  if(isTRUE(logscaling)){
    abundance<- log(abundance+1)
  }

  # Moritz table
  y <- data.frame(
    words = rownames(abundance),
    incidence = rowSums(abundance > 0, na.rm = TRUE),
    total_abundance = rowSums(abundance, na.rm = TRUE),
    stringsAsFactors = FALSE,
    row.names = NULL,
    check.names = FALSE
  )

  new_moritz <- y[order(y$incidence,decreasing = TRUE), ]
  rownames(new_moritz) <- NULL

  return(list(new_abundance = abundance, new_moritz = new_moritz))
}
