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
#' # Example 1: Detect a valid PDF created with base R
#' temp_valid_pdf <- tempfile(fileext = ".pdf")
#' pdf(temp_valid_pdf)
#' plot(1:10, main = "Test Plot")
#' dev.off()
#' pdf_is_valid(temp_valid_pdf)  # Should return TRUE
#' unlink(temp_valid_pdf)
#'
#' # Example 2: Detect an invalid "fake" PDF
#' temp_fake_pdf <- tempfile(fileext = ".pdf")
#' writeLines("Not a real PDF", temp_fake_pdf)
#' pdf_is_valid(temp_fake_pdf)  # Should return FALSE
#' unlink(temp_fake_pdf)
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
