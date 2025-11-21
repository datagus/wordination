# Tests for pdf_is_valid()

# Basic functionality tests ----

test_that("detects valid PDF created with base R", {
  temp_valid_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_valid_pdf)
  plot(1:10, main = "Test Plot")
  dev.off()

  expect_true(pdf_is_valid(temp_valid_pdf))
  unlink(temp_valid_pdf)
})

test_that("returns FALSE for non-existent file", {
  expect_false(pdf_is_valid("non_existent_file.pdf"))
})

test_that("returns FALSE for text file", {
  temp_txt <- tempfile(fileext = ".txt")
  writeLines("This is not a PDF", temp_txt)
  expect_false(pdf_is_valid(temp_txt))
  unlink(temp_txt)
})

test_that("returns FALSE for fake PDF with PDF extension", {
  temp_fake_pdf <- tempfile(fileext = ".pdf")
  writeLines("Not a real PDF", temp_fake_pdf)
  expect_false(pdf_is_valid(temp_fake_pdf))
  unlink(temp_fake_pdf)
})

test_that("returns FALSE for empty file", {
  temp_empty <- tempfile(fileext = ".pdf")
  file.create(temp_empty)
  expect_false(pdf_is_valid(temp_empty))
  unlink(temp_empty)
})

test_that("returns FALSE for corrupted PDF", {
  temp_corrupt <- tempfile(fileext = ".pdf")
  # Start with valid PDF header but corrupt content
  writeBin(charToRaw("%PDF-1.4\n%corrupted\nxref\n0 0\ntrailer\n<<>>\nstartxref\n0\n%%EOF"),
           temp_corrupt)
  expect_false(pdf_is_valid(temp_corrupt))
  unlink(temp_corrupt)
})

test_that("detects valid multi-page PDF", {
  temp_multipage <- tempfile(fileext = ".pdf")
  pdf(temp_multipage)
  for(i in 1:3) {
    plot(1:10, main = paste("Page", i))
  }
  dev.off()

  expect_true(pdf_is_valid(temp_multipage))
  unlink(temp_multipage)
})

# Edge case tests ----

test_that("returns FALSE for NULL input", {
  expect_false(pdf_is_valid(NULL))
})

test_that("returns FALSE for NA input", {
  expect_false(pdf_is_valid(NA))
})

test_that("returns FALSE for empty string", {
  expect_false(pdf_is_valid(""))
})

test_that("handles special characters in filename", {
  temp_special <- tempfile(pattern = "test#file-with_special chars_",
                           fileext = ".pdf")
  pdf(temp_special)
  plot(1:5)
  dev.off()

  expect_true(pdf_is_valid(temp_special))
  unlink(temp_special)
})

test_that("returns FALSE for directory input", {
  temp_dir <- tempdir()
  expect_false(pdf_is_valid(temp_dir))
})

test_that("handles malformed PDF with only header", {
  temp_malformed <- tempfile(fileext = ".pdf")

  # Write just a PDF header with minimal structure
  con <- file(temp_malformed, "wb")
  writeBin(charToRaw("%PDF-1.4\n"), con)
  close(con)

  expect_false(pdf_is_valid(temp_malformed))
  unlink(temp_malformed)
})

# Different PDF versions ----

test_that("detects PDFs with version 1.4", {
  temp_pdf_v1.4 <- tempfile(fileext = ".pdf")
  pdf(temp_pdf_v1.4, version = "1.4")
  plot(1:5)
  dev.off()

  expect_true(pdf_is_valid(temp_pdf_v1.4))
  unlink(temp_pdf_v1.4)
})

test_that("detects PDFs with version 1.7", {
  temp_pdf_v1.7 <- tempfile(fileext = ".pdf")

  # Some R versions may not support 1.7, so wrap in tryCatch
  pdf_created <- tryCatch({
    pdf(temp_pdf_v1.7, version = "1.7")
    plot(1:5)
    dev.off()
    TRUE
  }, error = function(e) {
    FALSE
  })

  if (pdf_created && file.exists(temp_pdf_v1.7)) {
    expect_true(pdf_is_valid(temp_pdf_v1.7))
    unlink(temp_pdf_v1.7)
  } else {
    skip("PDF version 1.7 not supported")
  }
})
