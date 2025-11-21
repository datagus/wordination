# Tests for filter_words_in_text()

# Helper to create test file
create_test_file <- function(dir, filename, word_count) {
  words <- c("cat", "dog", "bird", "tree", "run", "jump", "quickly", "beautiful")
  text <- paste(rep(paste(sample(words, 5, replace = TRUE), collapse = " "),
                    ceiling(word_count / 5)), collapse = " ")
  writeLines(text, file.path(dir, filename))
}

# Basic functionality
test_that("creates filtered directory and processes files", {
  test_dir <- file.path(tempdir(), "test_filter_1")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  create_test_file(test_dir, "test.txt", 2100)
  result <- suppressMessages(filter_words_in_text(test_dir))

  expect_true(dir.exists(file.path(test_dir, "filtered")))
  expect_equal(result[[1]]$status, "processed")
  expect_true(file.exists(file.path(test_dir, "filtered", "test-filtered.txt")))
})

# Word count threshold
test_that("skips files below min_word_count", {
  test_dir <- file.path(tempdir(), "test_filter_2")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  create_test_file(test_dir, "short.txt", 500)
  result <- suppressMessages(filter_words_in_text(test_dir))

  expect_equal(result[[1]]$status, "skipped")
  expect_match(result[[1]]$reason, "Less than 2000 words")
})

test_that("custom min_word_count parameter works", {
  test_dir <- file.path(tempdir(), "test_filter_3")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  create_test_file(test_dir, "medium.txt", 800)

  result1 <- suppressMessages(filter_words_in_text(test_dir))
  expect_equal(result1[[1]]$status, "skipped")

  result2 <- suppressMessages(filter_words_in_text(test_dir, min_word_count = 500))
  expect_equal(result2[[1]]$status, "processed")
})

# Rewrite parameter
test_that("rewrite=TRUE overwrites originals, rewrite=FALSE creates subdirectory", {
  test_dir <- file.path(tempdir(), "test_filter_4")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  create_test_file(test_dir, "file.txt", 2100)
  original <- readLines(file.path(test_dir, "file.txt"), warn = FALSE)

  suppressMessages(filter_words_in_text(test_dir, rewrite = TRUE))
  filtered <- readLines(file.path(test_dir, "file.txt"), warn = FALSE)

  expect_false(identical(original, filtered))
  expect_false(dir.exists(file.path(test_dir, "filtered")))
})

# Multiple files
test_that("processes multiple files correctly", {
  test_dir <- file.path(tempdir(), "test_filter_5")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  create_test_file(test_dir, "file1.txt", 2100)
  create_test_file(test_dir, "file2.txt", 2200)
  create_test_file(test_dir, "short.txt", 1000)

  result <- suppressMessages(filter_words_in_text(test_dir))

  expect_equal(length(result), 3)
  expect_equal(sum(sapply(result, function(x) x$status == "processed")), 2)
  expect_equal(sum(sapply(result, function(x) x$status == "skipped")), 1)
})

# POS tags
test_that("different POS tags filter differently", {
  test_dir <- file.path(tempdir(), "test_filter_6")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  create_test_file(test_dir, "test.txt", 2100)

  result_nouns <- suppressMessages(filter_words_in_text(test_dir, pos_tag = c("NN", "NNP")))
  unlink(file.path(test_dir, "filtered"), recursive = TRUE)

  result_verbs <- suppressMessages(filter_words_in_text(test_dir, pos_tag = c("VB", "VBG")))

  expect_false(identical(result_nouns[[1]]$filtered_word_count,
                         result_verbs[[1]]$filtered_word_count))
})

# Input validation
test_that("validates parameters correctly", {
  test_dir <- file.path(tempdir(), "test_filter_7")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  expect_error(filter_words_in_text("fake_directory"), "does not exist")
  expect_error(filter_words_in_text(test_dir, min_word_count = -100), "positive number")
  expect_error(filter_words_in_text(test_dir, min_word_count = 0), "positive number")
})

# Return values
test_that("returns correct structure", {
  test_dir <- file.path(tempdir(), "test_filter_8")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  create_test_file(test_dir, "test.txt", 2100)
  result <- suppressMessages(filter_words_in_text(test_dir))

  expect_true(all(c("file", "status", "word_count", "filtered_word_count",
                    "pos_proportion") %in% names(result[[1]])))
  expect_true(is.numeric(result[[1]]$pos_proportion))
  expect_true(result[[1]]$pos_proportion >= 0 && result[[1]]$pos_proportion <= 1)
})

# Helper function
test_that("show_pos_tags works", {
  tags <- suppressMessages(show_pos_tags())
  expect_s3_class(tags, "data.frame")
  expect_true(all(c("Tag", "Type", "Description") %in% names(tags)))
  expect_true("NN" %in% tags$Tag)
})
