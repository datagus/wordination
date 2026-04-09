# Tests for groups_df()

# Helper function to generate standard test data
make_test_data <- function() {
  x <- data.frame(
    art1 = c(1, 0, 3, 2, 0, 1, 4, 0, 2, 1),
    art2 = c(0, 2, 1, 0, 3, 1, 0, 2, 1, 0),
    art3 = c(1, 1, 0, 2, 1, 0, 1, 3, 0, 2),
    art4 = c(2, 0, 1, 1, 0, 2, 0, 1, 3, 0)
  )
  rownames(x) <- paste0("word", 1:10)
  return(x)
}

test_that("groups_df returns a dataframe with correct structure", {
  x <- make_test_data()
  model <- run_dca(x)

  result <- groups_df(x, k = 2, model = model)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("article", "cluster_number", "words"))
})

test_that("groups_df returns one row per article", {
  x <- make_test_data()
  model <- run_dca(x)

  result <- groups_df(x, k = 2, model = model)

  expect_equal(nrow(result), ncol(x))
})

test_that("groups_df assigns valid cluster numbers", {
  x <- make_test_data()
  model <- run_dca(x)
  k <- 3

  result <- groups_df(x, k = k, model = model)

  expect_true(all(result$cluster_number >= 1))
  expect_true(all(result$cluster_number <= k))
})

test_that("groups_df works with different k values", {
  x <- make_test_data()
  model <- run_dca(x)

  expect_no_error(groups_df(x, k = 2, model = model))
  expect_no_error(groups_df(x, k = 3, model = model))
})
