test_that("running_dca returns a decorana model on valid input", {

  # toy abundance table: words = rows, articles = columns
  x <- data.frame(
    art1 = c(2, 0, 1),
    art2 = c(0, 1, 1),
    art3 = c(1, 1, 0)
  )
  rownames(x) <- c("word1", "word2", "word3")

  res <- running_dca(
    x,
    drop_words = "word3",
    incidence  = 0,
    logscaling = TRUE
  )

  # result should be of class decorana
  expect_s3_class(res, "decorana")

  # DCA should have site scores (articles)
  expect_true(!is.null(res$rproj))
  expect_equal(nrow(res$rproj), ncol(x) - 0)  # number of articles
})

test_that("running_dca works with logscaling = FALSE", {

  skip_if_not_installed("vegan")

  x <- data.frame(
    art1 = c(2, 0, 1),
    art2 = c(0, 1, 1)
  )
  rownames(x) <- c("word1", "word2", "word3")

  res <- running_dca(
    x,
    logscaling = FALSE
  )

  expect_s3_class(res, "decorana")
  expect_true(!is.null(res$rproj))
})

test_that("running_dca respects drop_words and incidence filtering", {

  skip_if_not_installed("vegan")

  x <- data.frame(
    art1 = c(1, 0, 1, 0, 2),
    art2 = c(1, 1, 0, 0, 1),
    art3 = c(0, 1, 1, 0, 0),
    art4 = c(0, 0, 1, 0, 1)
  )
  rownames(x) <- c("word1", "word2", "word3", "word4", "word5")

  # incidence:
  # word1 = 2
  # word2 = 2
  # word3 = 3
  # word4 = 0
  # word5 = 3

  res <- running_dca(
    x,
    drop_words = c("word2"),
    incidence  = 1,
    logscaling = FALSE
  )

  # Remaining words after filtering: word1, word3, word5
  expect_s3_class(res, "decorana")

  # Species scores should correspond to 3 remaining words
  expect_equal(nrow(res$cproj), 3)
})

test_that("running_dca errors on non-numeric columns", {

  x_bad <- data.frame(
    art1 = c(1, 0),
    art2 = c("a", "b")
  )
  rownames(x_bad) <- c("word1", "word2")

  expect_error(
    running_dca(x_bad),
    "must be numeric"
  )
})

test_that("running_dca errors on empty data frame", {

  x_empty <- data.frame()

  expect_error(
    running_dca(x_empty),
    "zero rows or columns"
  )
})

test_that("running_dca errors when x is not a data.frame", {

  x_mat <- matrix(c(1, 0, 1, 1), nrow = 2)

  expect_error(
    running_dca(x_mat),
  )
})


