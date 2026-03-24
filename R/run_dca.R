#' Run a Detrended Correspondence Analysis
#'
#' This function runs a Detrended Correspondence Analysis (DCA) on the abundance table
#' using \code{\link[vegan]{decorana}}. Before running the DCA, the abundance table can
#' be filtered by dropping specific words and applying an incidence threshold.
#' It is recommended to determine the incidence threshold beforehand using \code{\link{optimal_indidence}}
#'
#' @param x Data frame. The abundance table with words as rows and articles as columns.
#'  All columns must be numeric. The table should not be scaled or normalized beforehand.
#' @param drop_words Character vector. Words to remove before analysis. Default is empty.
#' @param incidence Numeric. Incidence threshold; only words that appear in more than this number of articles are kept.
#'  Default is 0 (no filtering).
#' @param logscaling Logical. If \code{TRUE} (default), a log(x + 1) transformation is applied before analysis.
#' @importFrom vegan decorana
#'
#' @returns A \code{decorana} object from the \pkg{vegan} package.
#'  See \code{\link[vegan]{decorana}} for details.
#'
#'@seealso \code{\link{optimal_incidence}} for determining the incidence threshold.
#'
#' @export
#'
#' @examples
#' data(example_abundance)
#' model <- run_dca(example_abundance)
#' model
#'
run_dca <- function(x, drop_words=c(), incidence=0, logscaling=TRUE){

  stopifnot(is.data.frame(x))

  if (!all(vapply(x, is.numeric, logical(1)))) {
    stop("All columns of `x` must be numeric (counts).")
  }
  if (nrow(x) == 0L || ncol(x) == 0L) stop("`x` has zero rows or columns.")

  # Remove specified words
  x <- x[!rownames(x) %in% drop_words, 1:ncol(x), drop = FALSE]

  # Keep only words exceeding the incidence threshold
  abundance <- x[rowSums(x > 0, na.rm = TRUE)>incidence,]

  # Log scaling
  if(isTRUE(logscaling)){
    abundance<- log(abundance+1)
  }

  # Transpose: DCA expects articles as rows
  abundancet <- t(abundance)

  model<-vegan::decorana(abundancet,iweigh=0, ira=0)
  return(model)
}
