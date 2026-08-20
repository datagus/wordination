#' Transformative learning review abundance table
#'
#' Word abundance counts for the transformative learning in sustainability research corpus.
#' Note: This package wasn't used for the analysis of the source article.
#'
#' This data frame was created using two functions from this package: \code{import_words()} and
#' \code{abundance_table()}. Of the 149 source documents, only 145 were imported, since
#' \code{remove_outliers = TRUE} excluded 4 documents as outliers. It the object $abtab
#'
#' Each column corresponds to one of the 145 source documents; each row corresponds
#' to a noun from the corpus vocabulary. Cell values give the raw count of that word
#' in that document.
#'
#' @format A data frame with 7049 observations (words/nouns) and 145 variables
#'   (documents). Row names give the word/noun for each observation and are
#'   required for the package's functions to work correctly. Important: Do not discard
#'   or reset the row names! Each of the 145 columns is one source document, with cell
#'   values giving the raw count of that word in that document.
#' @source Derived from the 149-document corpus underlying the systematic
#'   review by Rodríguez Aboytes JG, Barth M (2020). Transformative learning
#'   in the field of sustainability: a systematic literature review
#'   (1999-2019). \emph{Int J Sustain High Educ} 21(5):993-1013.
#'   \url{https://doi.org/10.1108/ijshe-05-2019-0168}. The raw text files
#'   are bundled with this package and accessible via
#'   \code{system.file("extdata", package = "wordination")}.
"transformative_abundance"

#' Transformative learning review incidence table
#'
#' A summary table reporting, for each word in the transformative learning
#' in sustainability research corpus, its incidence and total abundance
#' across documents. The incidence of a word is defined as the number of
#' documents in which the word is present at least once (abundance > 0).
#' The total abundance of a word is the sum of its counts across all
#' documents.
#'
#' Note: this package was not used for the analysis in the source article.
#'
#' This data frame was created using two functions from this package:
#' \code{import_words()} and \code{incidence_table()}. Of the 149 source
#' documents, only 145 were imported, since \code{remove_outliers = TRUE}
#' excluded 4 documents as outliers.
#'
#' @format A data frame with 7090 rows (one per word) and 3 variables:
#' \describe{
#'   \item{\code{words}}{The word/noun.}
#'   \item{\code{incidence}}{Number of documents (out of 145) in which the word appears at least once.}
#'   \item{\code{total_abundance}}{Total count of the word summed across all 145 documents.}
#' }
#' @source Derived from the 149-document corpus underlying the systematic
#'   review by Rodríguez Aboytes JG, Barth M (2020). Transformative learning
#'   in the field of sustainability: a systematic literature review
#'   (1999-2019). \emph{Int J Sustain High Educ} 21(5):993-1013.
#'   \url{https://doi.org/10.1108/ijshe-05-2019-0168}. The raw text files
#'   are bundled with this package and accessible via
#'   \code{system.file("extdata", package = "wordination")}.
"transformative_incidence"
