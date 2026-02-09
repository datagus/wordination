# Tests for cluster_indicators()

# Helper to generate standard test data
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

test_that("cluster_indicators runs without error", {
  x <- make_test_data()

  expect_no_error(cluster_indicators(x, k = 2))
})

test_that("cluster_indicators works with different k values", {
  x <- make_test_data()

  expect_no_error(cluster_indicators(x, k = 2))
  expect_no_error(cluster_indicators(x, k = 3))
})
