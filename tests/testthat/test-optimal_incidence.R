test_that("optimal_incidence works on a simple abundance table", {

  # toy abundance table: words = rows, articles = columns
  x <- data.frame(
    art1 = c(1, 0, 3, 0),
    art2 = c(0, 2, 1, 0),
    art3 = c(1, 1, 0, 0)
  )
  rownames(x) <- c("word1", "word2", "word3", "word4")

  # Incidence per word:
  # word1 = 2, word2 = 2, word3 = 2, word4 = 0

  res  <- optimal_incidence(x, most_common = 50)
  res2 <- optimal_incidence(x, most_common = 90)
  res3 <- optimal_incidence(x, most_common = 100)  # edge case 1
  res4 <- optimal_incidence(x, most_common = 1)    # edge case 2

  # Basic checks
  expect_type(res, "list")
  expect_length(res, 2)
  expect_type(res2, "list")
  expect_length(res2, 2)

  # Top 50% of 4 words = 2 words
  # Sorted incidences: 2, 2, 2, 0
  # Cutoff = 2 → keep inc > 2 → none
  expect_equal(res[[1]], 2)
  expect_equal(res[[2]], 0)

  # Top 90% of 4 words = ceiling(3.6) = 4
  # Cutoff = 0 → keep inc > 0 → 3 words
  expect_equal(res2[[1]], 0)
  expect_equal(res2[[2]], 3)

  # Top 100% of words
  # Cutoff = 0 → keep inc > 0 → 3 words
  expect_equal(res3[[1]], 0)
  expect_equal(res3[[2]], 3)

  # Top 1% of 4 words = ceiling(0.04) = 1
  # Cutoff = 2 → keep inc > 2 → none
  expect_equal(res4[[1]], 2)
  expect_equal(res4[[2]], 0)
})

test_that("optimal_incidence errors on non-numeric columns", {

  x_bad <- data.frame(
    art1 = c(1, 0),
    art2 = c("a", "b")
  )

  expect_error(
    optimal_incidence(x_bad, most_common = 50),
    "must be numeric"
  )
})

test_that("function errors on invalid most_common values", {

  x <- data.frame(a = c(1, 2), b = c(0, 1))

  expect_error(optimal_incidence(x, most_common = 0))
  expect_error(optimal_incidence(x, most_common = 101))
  expect_error(optimal_incidence(x, most_common = "50"))
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
