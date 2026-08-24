#' Check if a file is a valid PDF
#'
#' @description
#' `pdf_is_valid()` checks if a file is a valid PDF.
#'
#' @details
#' To check if a file is a valid pdf, this function checks if PDF metadata can
#' be extracted, if the PDF has pages, has content and is not encrypted.
#'
#' @param f Character string. Path to a PDF file to check.
#'
#' @returns Logical. TRUE if valid PDF, FALSE otherwise.
#' @export
#'
#'
#' @examples
#' # Disclaimer: Some examples rely on example data provided with this package.
#' # All example PDFs and text files were authored by the creators of this package.
#'
#' # Example 1: Detect a valid PDF from the package example data
#' valid_pdf <- system.file(
#'   "extdata", "pdf", "Tolksdorf_2023_Stranded_seaweed.pdf",
#'   package = "wordination"
#' )
#' pdf_is_valid(valid_pdf)  # Should return TRUE
#'
#' # Example 2: Detect an invalid PDF from the package example data
#' invalid_pdf <- system.file(
#'   "extdata", "pdf", "Tolksdorf_2023_Stranded_seaweed_INVALID.pdf",
#'   package = "wordination"
#' )
#' pdf_is_valid(invalid_pdf)  # Should return FALSE
#'
#' \dontrun{
#' # Example 3: Check user input
#' pdf_is_valid("path/to/your/file.pdf")
#' }
pdf_is_valid <- function(f) {
  # Try to read PDF info
  info <- tryCatch({
    suppressMessages(suppressWarnings(
      pdftools::pdf_info(f)
    ))
  }, error = function(e) return(NULL))

  # Validate PDF has content & is not encrypted
  if (is.null(info)) return(FALSE)
  if (!is.null(info$encrypted) && info$encrypted) return(FALSE)
  if (is.null(info$pages) || is.na(info$pages) || info$pages == 0) return(FALSE)

  return(TRUE)
}
