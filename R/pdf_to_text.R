#' Convert PDF files to text files
#'
#' @description
#' `pdf_to_text()` extracts text from PDF files in a folder and saves it in text files.
#' It attempts Optical Character Recognition (OCR) for scanned PDFs with minimal text content.
#'
#' @details
#' The following workflow is suggested for this function.
#'
#' First, try the function with default values or adjustments to your needs.
#'
#' It is strongly recommended to keep `check_valid = TRUE` in the first step because
#' skipping the filtering out of invalid PDFs can lead to increased runtime as
#' the function is trying to extract text from PDFs that cannot be extracted.
#' Furthermore, if `check_valid = FALSE`, PDFs that cannot be extracted are not
#' explicitly shown as invalid, but are skipped and result in an empty text file.
#'
#' If `use_ocr = TRUE`, Optical Character Recognition (OCR) is attempted for
#' scanned PDFs. Here, an unexpectedly low word count (lower than `ocr_threshold`)
#' of the output of the normal PDF extraction is used as an indicator that the
#' PDF is scanned and needs to be extracted with OCR. Hence, `ocr_threshold`
#' needs to be adjusted to the expected text length. Since OCR is the
#' most computationally expensive and time-consuming step in this function, a
#' higher threshold, i.e. attempting OCR more often, might increase the runtime.
#'
#' If `clean_text = TRUE` the following cleaning steps are performed:
#' Remove punctuation, numbers, whitespace, English stopwords (e.g. "a", "and", "the")
#' and change all words to lowercase.
#'
#' Second, check if files classified as invalid are actually invalid,
#' i.e. they cannot be opened in a PDF editor. If so, replace
#' them with valid versions if possible and apply the function again.
#'
#' Third, try extracting files incorrectly classified as invalid by applying the
#' function to those files again with `check_valid = FALSE`. Note that if
#' `check_valid = FALSE`, PDFs that cannot be extracted are not shown as invalid,
#' but are skipped and result in an empty text file. Single PDFs can be used as
#' input and `quiet = FALSE` can be used for more transparent error tracking.
#'
#' @param dir_pdf Character string. Path to PDF file or folder containing PDF files.
#' @param dir_text Character string. Path to folder for saving text files.
#' @param check_valid Logical. Whether to check if PDFs are valid before text extraction (default TRUE).
#' @param use_ocr Logical. Whether to attempt OCR for low-text PDFs (default TRUE).
#' @param ocr_threshold Integer. Word count below which to attempt OCR (default 100).
#' @param clean_text Logical. Whether to clean text (default TRUE).
#' @param quiet Logical. Whether to suppress error, warning and update messages (default TRUE).
#'
#' @returns A list containing:
#'   \item{text_folder}{Path to the output text folder}
#'   \item{num_converted}{Number of successfully converted files}
#'   \item{invalid_files}{Character vector of files that were not converted because they were classified as invalid.}
#'   \item{invalid_folder}{Path to the folder with files that were not converted}
#'
#' @export
#'
#' @examples
#' # Create a temporary folder for PDFs
#' temp_pdf_dir <- tempfile("pdfs")
#' dir.create(temp_pdf_dir)
#'
#' # Create two simple PDFs
#' for (i in 1:2) {
#'   pdf_file <- file.path(temp_pdf_dir, paste0("file", i, ".pdf"))
#'   pdf(pdf_file)
#'   plot.new()
#'   text(0.5, 0.5, paste("PDF", i), cex = 2)
#'   dev.off()
#' }
#'
#' # Create a temporary output folder for text files
#' temp_output <- tempfile("txt")
#' dir.create(temp_output)
#'
#' # Convert all PDFs in the folder to text
#' result <- pdf_to_text(temp_pdf_dir, temp_output, use_ocr = FALSE)
#' print(result$num_converted)  # Should be 2
#'
#' # List created text files
#' list.files(temp_output, pattern = "\\.txt$")
#'
#' # Clean up
#' unlink(temp_pdf_dir, recursive = TRUE)
#' unlink(temp_output, recursive = TRUE)
#'
#'
#' \dontrun{
#' # Use your own directories
#' dir_pdf <- "path/to/your/pdfs"
#' dir_text <- "path/to/output"
#' pdf_to_text(dir_pdf, dir_text)
#' }
pdf_to_text <- function(dir_pdf, dir_text,
                        check_valid = TRUE,
                        use_ocr = TRUE,
                        ocr_threshold = 100,
                        clean_text = TRUE,
                        quiet = TRUE) {

  # Validate input directories
  if (!file.exists(dir_pdf)) {
    stop("PDF directory or file does not exist: ", dir_pdf)
  }
  # Validate output directories
  if (!dir.exists(dir_text)) {
    stop("Text directory does not exist: ", dir_text)
  }

  # Step 1: Get list of PDF files or single file
  if (file.info(dir_pdf)$isdir) {
    files <- list.files(path = dir_pdf, pattern = "\\.pdf$", full.names = TRUE)
  } else if (grepl("\\.pdf$", dir_pdf, ignore.case = TRUE)) {
    files <- dir_pdf
  } else {
    stop("Input path must be a directory or a .pdf file: ", dir_pdf)
  }

  if (length(files) == 0) {
    warning("No PDF files found in: ", dir_pdf)
    return(list(
      text_folder = normalizePath(dir_text),
      num_converted = 0,
      invalid_files = character(0)
    ))
  }

  # Step 2: Check which PDFs are valid
  if (check_valid) {
    valid_flags <- sapply(files, pdf_is_valid)
    valid_files <- files[valid_flags]
    invalid_files <- files[!valid_flags]

    invalid_dir <- file.path(dir_text, "invalid_pdfs")
    if (length(invalid_files) > 0) {
      if (!dir.exists(invalid_dir)) dir.create(invalid_dir, recursive = TRUE)
      file.copy(invalid_files, invalid_dir, overwrite = TRUE)
    } else {
      invalid_dir <- NULL
    }
  } else {
    valid_files <- files
    invalid_files <- character(0)
    invalid_dir <- NULL
  }

  # Step 3: Read valid PDFs into a list
  docs <- lapply(valid_files, function(f) {
    txt <- tryCatch({
      if (quiet) {
        suppressMessages(suppressWarnings(pdftools::pdf_text(f)))
      } else {
        pdftools::pdf_text(f)
      }
    }, error = function(e) {
      if (!quiet) message("Error reading PDF '", f, "': ", e$message)
      ""
    })

    list(
      name = basename(f),
      path = f,
      text = paste(txt, collapse = " ")
    )
  })

  # Step 4: Use OCR for scanned pdfs (wordcount < 100)
  if (use_ocr) {
    ocr_engine <- if (quiet) {
      suppressMessages(suppressWarnings(tesseract::tesseract("eng")))
    } else {
      tesseract::tesseract("eng")
    }
    ocr_pb <- utils::txtProgressBar(min = 0, max = length(docs), style = 3)

    for (i in seq_along(docs)) {
      original_text <- docs[[i]]$text
      original_word_count <- length(unlist(strsplit(original_text, "\\W+")))

      if (original_word_count < ocr_threshold){
        pdf_path <- docs[[i]]$path

        # Extract text from pdf with OCR
        full_ocr_text <- tryCatch({
          if (quiet) {
            suppressMessages(suppressWarnings({
              # Temporarily redirect stdout to null
              temp_file <- tempfile()
              sink(temp_file)
              txt <- pdftools::pdf_ocr_text(pdf_path, language = "eng")
              sink() # reset output
              unlink(temp_file)
              txt
            }))
          } else {
            pdftools::pdf_ocr_text(pdf_path, language = "eng")
          }
        }, error = function(e) {
          if (!quiet) message("Error reading PDF with OCR '", pdf_path, "': ", e$message)
          ""
        })

        ocr_word_count <- length(unlist(strsplit(full_ocr_text, "\\W+")))

        # Replace only if OCR gives more words
        if (ocr_word_count > original_word_count) {
          docs[[i]]$text <- paste(full_ocr_text, collapse = " ")
        }
      }

      utils::setTxtProgressBar(ocr_pb, i)
    }

    close(ocr_pb)
  }

  # Step 5: Remove non-narrative elements
  corp <- tm::VCorpus(tm::VectorSource(lapply(docs, function(x) x$text)))
  if (clean_text) {
    corp <- tm::tm_map(corp, tm::removePunctuation)
    corp <- tm::tm_map(corp, tm::removeNumbers)
    corp <- tm::tm_map(corp, tm::removeWords, tm::stopwords("english"))
    corp <- tm::tm_map(corp, tm::content_transformer(tolower))
    corp <- tm::tm_map(corp, tm::stripWhitespace)
  }

  # Write cleaned text back into docs
  for (i in seq_along(docs)) {
    docs[[i]]$text <- corp[[i]]$content
  }

  # Step 6: Store the text of each pdf in a txt file
  for (i in seq_along(docs)) {
    doc_text <- docs[[i]]$text
    doc_name <- sub("\\.pdf$", "", docs[[i]]$name, ignore.case = TRUE)
    txt_path <- file.path(dir_text, paste0(doc_name, ".txt"))
    writeLines(doc_text, txt_path)
  }

  # Step 7: Return path, number of files and corrupted files to user
  return(list(
    text_folder = normalizePath(dir_text),
    num_converted = length(docs),
    invalid_files = basename(invalid_files),
    invalid_folder = if (!is.null(invalid_dir)) normalizePath(invalid_dir) else NULL
  ))
}
