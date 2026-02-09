# Tests for clusters_barplot()

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

test_that("clusters_barplot returns a matrix", {
  x <- make_test_data()

  pdf(NULL)  # suppress graphical output
  result <- clusters_barplot(x, maxclusters = 3)
  dev.off()

  expect_true(is.matrix(result))
})

test_that("clusters_barplot runs with default parameters", {
  x <- make_test_data()

  pdf(NULL)
  expect_no_error(clusters_barplot(x, maxclusters = 3))
  dev.off()
})

test_that("clusters_barplot works with showCounts = TRUE", {
  x <- make_test_data()

  pdf(NULL)
  expect_no_error(clusters_barplot(x, maxclusters = 3, showCounts = TRUE))
  dev.off()
})

test_that("clusters_barplot works with printclusters = TRUE", {
  x <- make_test_data()

  pdf(NULL)
  expect_no_error(clusters_barplot(x, maxclusters = 3, printclusters = TRUE))
  dev.off()
})

test_that("clusters_barplot errors when maxclusters exceeds color vector length", {
  x <- make_test_data()
  custom_colors <- c("#FF0000", "#00FF00")  # only 2 colors

  expect_error(
    clusters_barplot(x, maxclusters = 5, colores = custom_colors),
    "exceeds length of color vector"
  )
})

test_that("clusters_barplot works with custom colors", {
  x <- make_test_data()
  custom_colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00")

  pdf(NULL)
  expect_no_error(clusters_barplot(x, maxclusters = 3, colores = custom_colors))
  dev.off()
})
