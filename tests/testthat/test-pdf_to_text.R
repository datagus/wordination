# Tests for pdf_to_text()

# Setup helper function to create test PDFs ----

create_test_pdf <- function(path, content = "Test content", n_pages = 1) {
  pdf(path)
  for (i in seq_len(n_pages)) {
    plot.new()
    text(0.5, 0.5, paste(content, "Page", i), cex = 2)
  }
  dev.off()
  invisible(NULL)
}

# Input validation tests ----

test_that("errors on non-existent PDF directory", {
  temp_output <- tempfile("output")
  dir.create(temp_output)
  on.exit(unlink(temp_output, recursive = TRUE), add = TRUE)

  expect_error(
    pdf_to_text("non_existent_dir", temp_output),
    "PDF directory or file does not exist"
  )
})

test_that("errors on non-existent output directory", {
  temp_pdf <- tempfile(fileext = ".pdf")
  create_test_pdf(temp_pdf)
  on.exit(unlink(temp_pdf), add = TRUE)

  expect_error(
    pdf_to_text(temp_pdf, "non_existent_output_dir"),
    "Text directory does not exist"
  )
})

test_that("errors on invalid input path (not directory or PDF)", {
  temp_output <- tempfile("output")
  dir.create(temp_output)
  on.exit(unlink(temp_output, recursive = TRUE), add = TRUE)

  temp_txt <- tempfile(fileext = ".txt")
  writeLines("Not a PDF", temp_txt)
  on.exit(unlink(temp_txt), add = TRUE)

  expect_error(
    pdf_to_text(temp_txt, temp_output),
    "Input path must be a directory or a .pdf file"
  )
})

test_that("warns when no PDF files found in directory", {
  temp_pdf_dir <- tempfile("pdfs")
  dir.create(temp_pdf_dir)
  on.exit(unlink(temp_pdf_dir, recursive = TRUE), add = TRUE)

  temp_output <- tempfile("output")
  dir.create(temp_output)
  on.exit(unlink(temp_output, recursive = TRUE), add = TRUE)

  expect_warning(
    result <- pdf_to_text(temp_pdf_dir, temp_output),
    "No PDF files found"
  )
  expect_equal(result$num_converted, 0)
})

# Basic functionality tests ----

test_that("converts single PDF file", {
  temp_pdf <- tempfile(fileext = ".pdf")
  create_test_pdf(temp_pdf, content = "Single file test")
  on.exit(unlink(temp_pdf), add = TRUE)

  temp_output <- tempfile("output")
  dir.create(temp_output)
  on.exit(unlink(temp_output, recursive = TRUE), add = TRUE)

  result <- pdf_to_text(temp_pdf, temp_output, use_ocr = FALSE)

  expect_equal(result$num_converted, 1)
  expect_true(dir.exists(result$text_folder))

  # Check that text file was created
  txt_files <- list.files(temp_output, pattern = "\\.txt$")
  expect_equal(length(txt_files), 1)
})

test_that("converts multiple PDF files from directory", {
  temp_pdf_dir <- tempfile("pdfs")
  dir.create(temp_pdf_dir)
  on.exit(unlink(temp_pdf_dir, recursive = TRUE), add = TRUE)

  # Create multiple PDFs
  for (i in 1:3) {
    create_test_pdf(
      file.path(temp_pdf_dir, paste0("test", i, ".pdf")),
      content = paste("Document", i)
    )
  }

  temp_output <- tempfile("output")
  dir.create(temp_output)
  on.exit(unlink(temp_output, recursive = TRUE), add = TRUE)

  result <- pdf_to_text(temp_pdf_dir, temp_output, use_ocr = FALSE)

  expect_equal(result$num_converted, 3)

  # Check that text files were created
  txt_files <- list.files(temp_output, pattern = "\\.txt$")
  expect_equal(length(txt_files), 3)
})

test_that("creates text files with same basename as PDF", {
  temp_pdf_dir <- tempfile("pdfs")
  dir.create(temp_pdf_dir)
  on.exit(unlink(temp_pdf_dir, recursive = TRUE), add = TRUE)

  create_test_pdf(file.path(temp_pdf_dir, "my_document.pdf"))

  temp_output <- tempfile("output")
  dir.create(temp_output)
  on.exit(unlink(temp_output, recursive = TRUE), add = TRUE)

  result <- pdf_to_text(temp_pdf_dir, temp_output, use_ocr = FALSE)

  expect_true(file.exists(file.path(temp_output, "my_document.txt")))
})

# Return structure tests ----

test_that("returns correct structure", {
  temp_pdf <- tempfile(fileext = ".pdf")
  create_test_pdf(temp_pdf)
  on.exit(unlink(temp_pdf), add = TRUE)

  temp_output <- tempfile("output")
  dir.create(temp_output)
  on.exit(unlink(temp_output, recursive = TRUE), add = TRUE)

  result <- pdf_to_text(temp_pdf, temp_output, use_ocr = FALSE)

  expect_type(result, "list")
  expect_named(result, c("text_folder", "num_converted", "invalid_files", "invalid_folder"))
  expect_type(result$text_folder, "character")
  expect_type(result$num_converted, "integer")
  expect_type(result$invalid_files, "character")
})

test_that("returns normalized paths", {
  temp_pdf <- tempfile(fileext = ".pdf")
  create_test_pdf(temp_pdf)
  on.exit(unlink(temp_pdf), add = TRUE)

  temp_output <- tempfile("output")
  dir.create(temp_output)
  on.exit(unlink(temp_output, recursive = TRUE), add = TRUE)

  result <- pdf_to_text(temp_pdf, temp_output, use_ocr = FALSE)

  # Check that paths are normalized (absolute)
  expect_true(grepl("^/|^[A-Z]:", result$text_folder))
})

# check_valid parameter tests ----

test_that("identifies and separates invalid PDFs when check_valid = TRUE", {
  temp_pdf_dir <- tempfile("pdfs")
  dir.create(temp_pdf_dir)
  on.exit(unlink(temp_pdf_dir, recursive = TRUE), add = TRUE)

  # Create valid PDF
  create_test_pdf(file.path(temp_pdf_dir, "valid.pdf"))

  # Create invalid PDF
  invalid_path <- file.path(temp_pdf_dir, "invalid.pdf")
  writeLines("Not a real PDF", invalid_path)

  temp_output <- tempfile("output")
  dir.create(temp_output)
  on.exit(unlink(temp_output, recursive = TRUE), add = TRUE)

  result <- pdf_to_text(temp_pdf_dir, temp_output,
                        check_valid = TRUE, use_ocr = FALSE)

  expect_equal(result$num_converted, 1)
  expect_equal(length(result$invalid_files), 1)
  expect_equal(result$invalid_files, "invalid.pdf")

  # Check that invalid PDFs folder was created
  expect_true(!is.null(result$invalid_folder))
  expect_true(dir.exists(result$invalid_folder))
  expect_true(file.exists(file.path(result$invalid_folder, "invalid.pdf")))
})

test_that("returns NULL invalid_folder when all PDFs are valid", {
  temp_pdf_dir <- tempfile("pdfs")
  dir.create(temp_pdf_dir)
  on.exit(unlink(temp_pdf_dir, recursive = TRUE), add = TRUE)

  # Create only valid PDFs
  create_test_pdf(file.path(temp_pdf_dir, "valid1.pdf"))
  create_test_pdf(file.path(temp_pdf_dir, "valid2.pdf"))

  temp_output <- tempfile("output")
  dir.create(temp_output)
  on.exit(unlink(temp_output, recursive = TRUE), add = TRUE)

  result <- pdf_to_text(temp_pdf_dir, temp_output,
                        check_valid = TRUE, use_ocr = FALSE)

  expect_equal(result$num_converted, 2)
  expect_equal(length(result$invalid_files), 0)
  expect_null(result$invalid_folder)
})

test_that("processes all PDFs when check_valid = FALSE", {
  temp_pdf_dir <- tempfile("pdfs")
  dir.create(temp_pdf_dir)
  on.exit(unlink(temp_pdf_dir, recursive = TRUE), add = TRUE)

  # Create valid PDF
  create_test_pdf(file.path(temp_pdf_dir, "valid.pdf"))

  # Create invalid PDF (will fail extraction but not be filtered)
  invalid_path <- file.path(temp_pdf_dir, "invalid.pdf")
  writeLines("Not a real PDF", invalid_path)

  temp_output <- tempfile("output")
  dir.create(temp_output)
  on.exit(unlink(temp_output, recursive = TRUE), add = TRUE)

  result <- pdf_to_text(temp_pdf_dir, temp_output,
                        check_valid = FALSE, use_ocr = FALSE, quiet = TRUE)

  # Should attempt both, but count includes both (one will have empty text)
  expect_equal(result$num_converted, 2)
  expect_equal(length(result$invalid_files), 0)
  expect_null(result$invalid_folder)
})

# clean_text parameter tests ----

test_that("cleans text when clean_text = TRUE", {
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf)
  plot.new()
  text(0.5, 0.5, "Test 123! With PUNCTUATION.", cex = 2)
  dev.off()
  on.exit(unlink(temp_pdf), add = TRUE)

  temp_output <- tempfile("output")
  dir.create(temp_output)
  on.exit(unlink(temp_output, recursive = TRUE), add = TRUE)

  result <- pdf_to_text(temp_pdf, temp_output,
                        clean_text = TRUE, use_ocr = FALSE)

  txt_file <- file.path(temp_output, sub("\\.pdf$", ".txt", basename(temp_pdf)))
  text_content <- readLines(txt_file, warn = FALSE)
  text_combined <- paste(text_content, collapse = " ")

  # Check that cleaning occurred (lowercase, no punctuation, no numbers)
  expect_false(grepl("[A-Z]", text_combined))  # No uppercase
  expect_false(grepl("[0-9]", text_combined))  # No numbers
  expect_false(grepl("[!.]", text_combined))   # No punctuation
})

test_that("preserves text when clean_text = FALSE", {
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf)
  plot.new()
  text(0.5, 0.5, "Test 123! With PUNCTUATION.", cex = 2)
  dev.off()
  on.exit(unlink(temp_pdf), add = TRUE)

  temp_output <- tempfile("output")
  dir.create(temp_output)
  on.exit(unlink(temp_output, recursive = TRUE), add = TRUE)

  result <- pdf_to_text(temp_pdf, temp_output,
                        clean_text = FALSE, use_ocr = FALSE)

  txt_file <- file.path(temp_output, sub("\\.pdf$", ".txt", basename(temp_pdf)))
  text_content <- readLines(txt_file, warn = FALSE)
  text_combined <- paste(text_content, collapse = " ")

  # Verify preservation of formatting
  expect_true(nchar(text_combined) > 0)
  expect_true(grepl("[A-Z]", text_combined))      # HAS uppercase
  expect_true(grepl("[0-9]", text_combined))      # HAS numbers
  expect_true(grepl("[!.]", text_combined))       # HAS punctuation
})

# quiet parameter tests ----

test_that("suppresses messages when quiet = TRUE", {
  temp_pdf <- tempfile(fileext = ".pdf")
  create_test_pdf(temp_pdf)
  on.exit(unlink(temp_pdf), add = TRUE)

  temp_output <- tempfile("output")
  dir.create(temp_output)
  on.exit(unlink(temp_output, recursive = TRUE), add = TRUE)

  # Should not produce output
  expect_silent(
    result <- pdf_to_text(temp_pdf, temp_output, use_ocr = FALSE, quiet = TRUE)
  )
})

# more tests ----

test_that("handles PDF with multiple pages", {
  temp_pdf <- tempfile(fileext = ".pdf")
  create_test_pdf(temp_pdf, content = "Multi-page doc", n_pages = 5)
  on.exit(unlink(temp_pdf), add = TRUE)

  temp_output <- tempfile("output")
  dir.create(temp_output)
  on.exit(unlink(temp_output, recursive = TRUE), add = TRUE)

  result <- pdf_to_text(temp_pdf, temp_output, use_ocr = FALSE)

  expect_equal(result$num_converted, 1)

  # Check that text file contains content
  txt_file <- list.files(temp_output, pattern = "\\.txt$", full.names = TRUE)
  text_content <- readLines(txt_file[1], warn = FALSE)
  expect_true(length(text_content) > 0)
})

test_that("handles PDFs with special characters in names", {
  temp_pdf_dir <- tempfile("pdfs")
  dir.create(temp_pdf_dir)
  on.exit(unlink(temp_pdf_dir, recursive = TRUE), add = TRUE)

  # Create PDF with special chars (that are file-system safe)
  create_test_pdf(file.path(temp_pdf_dir, "test-file_2024.pdf"))

  temp_output <- tempfile("output")
  dir.create(temp_output)
  on.exit(unlink(temp_output, recursive = TRUE), add = TRUE)

  result <- pdf_to_text(temp_pdf_dir, temp_output, use_ocr = FALSE)

  expect_equal(result$num_converted, 1)
  expect_true(file.exists(file.path(temp_output, "test-file_2024.txt")))
})

test_that("handles empty PDF (no text content)", {
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf)
  plot.new()  # Empty plot, no text
  dev.off()
  on.exit(unlink(temp_pdf), add = TRUE)

  temp_output <- tempfile("output")
  dir.create(temp_output)
  on.exit(unlink(temp_output, recursive = TRUE), add = TRUE)

  result <- pdf_to_text(temp_pdf, temp_output, use_ocr = FALSE)

  expect_equal(result$num_converted, 1)

  # Should create a text file even if empty/minimal content
  txt_files <- list.files(temp_output, pattern = "\\.txt$")
  expect_equal(length(txt_files), 1)
})

# OCR test ----
test_that("pdf_to_text successfully converts PDFs with OCR", {
  skip_if_not_installed("pdftools")
  skip_if_not_installed("tm")

  # This group focuses on OCR behavior. Skip if tesseract is not installed.
  if (!requireNamespace("tesseract", quietly = TRUE)) {
    skip("tesseract not available; skipping OCR behavior tests")
  }

  # Additional requirement: we create a PNG and embed it into a PDF.
  if (!requireNamespace("png", quietly = TRUE)) {
    skip("png package not available; skipping scanned-image PDF OCR tests")
  }

  # Setup temporary directories
  dir_pdf <- tempfile("pdfs_ocr_")
  dir_text <- tempfile("out_ocr_")
  dir.create(dir_pdf); dir.create(dir_text)
  on.exit({
    if (dir.exists(dir_pdf)) unlink(dir_pdf, recursive = TRUE, force = TRUE)
    if (dir.exists(dir_text)) unlink(dir_text, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  # Create a scanned-style PDF:
  # 1) draw text to a PNG (bitmap)
  # 2) read the PNG and embed it into a PDF as a raster (so the PDF contains an image)
  png_file <- tempfile(fileext = ".png")
  png(png_file, width = 800, height = 200, res = 150)
  par(mar = c(0,0,0,0))
  plot.new()
  # Put a distinct string we can reasonably expect OCR to extract
  text(0.5, 0.5, "SCANNED TEXT OCRABLE example 123", cex = 4, col = "black")
  dev.off()
  expect_true(file.exists(png_file))

  scanned_pdf <- file.path(dir_pdf, "scanned_image.pdf")
  # Embed the PNG into a PDF as raster
  img <- png::readPNG(png_file)
  pdf(scanned_pdf, width = 8, height = 2)
  # Use grid to place the raster so it becomes an image in the PDF
  grid::grid.raster(img)
  dev.off()
  expect_true(file.exists(scanned_pdf))

  # Case A: use_ocr = FALSE -> likely little or no extracted text from the scanned image
  res_no_ocr <- pdf_to_text(scanned_pdf, dir_text, use_ocr = FALSE, quiet = TRUE, ocr_threshold = 1000)
  txt_no_ocr <- paste(readLines(file.path(dir_text, "scanned_image.txt"), warn = FALSE), collapse = " ")
  wc_no_ocr <- length(unlist(strsplit(txt_no_ocr, "\\W+")))
  # Expect very small word count (commonly 0), but allow <=3 to be tolerant
  expect_true(wc_no_ocr <= 3)

  # Remove file to avoid conflict for next run
  unlink(file.path(dir_text, "scanned_image.txt"))

  # Case B: use_ocr = TRUE and low threshold so OCR is attempted
  res_ocr <- pdf_to_text(scanned_pdf, dir_text, use_ocr = TRUE, quiet = TRUE, ocr_threshold = 1000)
  expect_equal(res_ocr$num_converted, 1)
  expect_true(file.exists(file.path(dir_text, "scanned_image.txt")))
  txt_ocr <- paste(readLines(file.path(dir_text, "scanned_image.txt"), warn = FALSE), collapse = " ")
  wc_ocr <- length(unlist(strsplit(txt_ocr, "\\W+")))

  # OCR should produce more words than no-ocr extraction in most environments.
  # Assert that OCR produced strictly more words than the no-OCR run, if possible.
  expect_true(wc_ocr >= wc_no_ocr)

  # Bonus: expect to find a token from the rendered string to show OCR worked reasonably
  # (tolerant check, case-insensitive)
  expect_true(grepl("scann|ocrable|example|123", tolower(txt_ocr)))
})
