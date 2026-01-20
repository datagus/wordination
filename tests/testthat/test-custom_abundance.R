test_that("custom_abundance refines an abundance table correctly", {

  # toy abundance table: words = rows, articles = columns
  x <- data.frame(
    art1 = c(2, 0, 1, 0),
    art2 = c(0, 1, 1, 0),
    art3 = c(1, 1, 0, 0)
  )
  rownames(x) <- c("word1", "word2", "word3", "word4")

  # incidence per word:
  # word1 = 2, word2 = 2, word3 = 2, word4 = 0

  res <- custom_abundance(
    x,
    drop_words = "word3",
    incidence  = 1,
    site_correction = FALSE,
    logscaling = FALSE
  )

  # Structure checks
  expect_type(res, "list")
  expect_named(res, c("new_abundance", "new_moritz"))

  expect_s3_class(res$new_abundance, "data.frame")
  expect_s3_class(res$new_moritz, "data.frame")


  # Abundance table checks
  # word3 dropped, incidence > 1 keeps word1, word2
  expect_equal(nrow(res$new_abundance), 2)
  expect_false("word4" %in% rownames(res$new_abundance))

  # Values preserved (no scaling applied)
  expect_equal(res$new_abundance["word1", "art1"], 2)
  expect_equal(res$new_abundance["word2", "art2"], 1)

  # Moritz table checks
  expect_equal(
    res$new_moritz$words,
    c("word1", "word2")
  )

  # incidence column exists and is correct
  expect_equal(res$new_moritz$incidence, c(2, 2))
})

test_that("custom_abundance applies site correction correctly", {

  x <- data.frame(
    art1 = c(2, 0),
    art2 = c(1, 1)
  )
  rownames(x) <- c("word1", "word2")

  # incidence:
  # word1 = 2, word2 = 1

  res <- custom_abundance(
    x,
    site_correction = TRUE,
    multiplier = 1,
    logscaling = FALSE
  )

  # Presence–absence per article:
  # art1: 1 word, art2: 2 words
  # site lengths = c(1, 2)

  expect_equal(
    res$new_abundance["word1", "art1"],
    2 / 1
  )
  expect_equal(
    res$new_abundance["word1", "art2"],
    1 / 2
  )
})

test_that("custom_abundance applies log scaling correctly", {

  x <- data.frame(
    art1 = c(1, 0),
    art2 = c(2, 1)
  )
  rownames(x) <- c("word1", "word2")

  res <- custom_abundance(
    x,
    site_correction = FALSE,
    logscaling = TRUE
  )

  expect_equal(
    res$new_abundance["word1", "art1"],
    log(1 + 1)
  )
  expect_equal(
    res$new_abundance["word2", "art2"],
    log(1 + 1)
  )
})

test_that("function errors on non-numeric columns", {

  x_bad <- data.frame(
    art1 = c(1, 0),
    art2 = c("a", "b")
  )

  expect_error(
    optimal_incidence(x_bad, most_common = 50),
    "must be numeric"
  )
})

test_that("function errors on empty data frame", {

  x_empty <- data.frame()

  expect_error(
    optimal_incidence(x_empty, most_common = 50),
    "zero rows or columns"
  )
})

test_that("function errors when x is not a data.frame", {

  x_mat <- matrix(c(1, 0, 1, 1), nrow = 2)

  expect_error(
    optimal_incidence(x_mat, most_common = 50),
  )
})

