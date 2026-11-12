# Tests for filter_words_in_text()

# Basic tests which don't require UDPipe

test_that("validates directory input", {
  expect_error(filter_words_in_text("fake_directory"), "does not exist")
})

test_that("validates min_word_count parameter", {
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  expect_error(filter_words_in_text(test_dir, min_word_count = -100), "positive number")
  expect_error(filter_words_in_text(test_dir, min_word_count = 0), "positive number")
})

test_that("handles empty directory", {
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  result <- suppressMessages(filter_words_in_text(test_dir))
  expect_equal(result, list())
})


test_that("clean_text removes extra whitespace", {
  expect_equal(clean_text("  hello   world  "), "hello world")
  expect_equal(clean_text("single"), "single")
})

test_that("show_pos_tags returns valid data frame", {
  tags <- suppressMessages(show_pos_tags())
  expect_s3_class(tags, "data.frame")
  expect_true(all(c("Tag", "Type", "Description", "Example") %in% names(tags)))
  expect_true("NN" %in% tags$Tag)
})

# Integration test requiring UDPipe

test_that("full pipeline processes files correctly", {
  skip_on_cran()

  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create test file with enough words
  text <- paste(rep("The cat sat on the mat and the dog ran in the park", 200), collapse = " ")
  writeLines(text, file.path(test_dir, "test.txt"))

  # Create short file (should be skipped)
  writeLines("short text", file.path(test_dir, "short.txt"))

  result <- suppressMessages(filter_words_in_text(test_dir, min_word_count = 500))

  expect_equal(length(result), 2)

  # Find results by filename
  test_result <- result[[which(sapply(result, function(x) x$file == "test.txt"))]]
  short_result <- result[[which(sapply(result, function(x) x$file == "short.txt"))]]

  expect_equal(test_result$status, "processed")
  expect_equal(short_result$status, "skipped")
  expect_true(file.exists(file.path(test_dir, "filtered", "test-filtered.txt")))
})
