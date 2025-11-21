# Tests for abundance_table().

# Standard Input
test_that("abundance_table handles standard input correctly", {
  # Setup data
  df1 <- data.frame(words = c("apple", "banana"), Freq = c(5, 3))
  df2 <- data.frame(words = c("banana", "date"), Freq = c(4, 3))

  articles <- list(df1, df2)
  names(articles) <- c("paper1", "paper2")

  # Suppress output (progress bars/prints) during testing
  result <- suppressMessages(suppressWarnings(
    abundance_table(articles, names(articles), 2, c(2, 2))
  ))

  # 1. Structure Checks
  expect_type(result, "list")
  expect_named(result, c("abtab", "moritz"))

  # 2. Value Checks (The missing piece in your original test)
  # Check specific values to ensure merge worked
  expect_equal(result$abtab["apple", "paper1"], 5)
  expect_equal(result$abtab["apple", "paper2"], 0) # Check NA -> 0 conversion
  expect_equal(result$abtab["banana", "paper1"], 3)
  expect_equal(result$abtab["banana", "paper2"], 4)

  # 3. Moritz Checks
  expect_true(is.data.frame(result$moritz))
  # Check sorting (incidence descending)
  expect_equal(result$moritz$words[1], "banana") # Total 7 (3+4), highest incidence
})

# Empty Data frames
test_that("abundance_table handles empty dataframes gracefully", {
  df1 <- data.frame(words = c("apple"), Freq = c(1))
  df_empty <- data.frame(words = character(0), Freq = numeric(0))

  articles <- list(df1, df_empty)
  names(articles) <- c("p1", "p_empty")

  # Expect the function to print "The 2nd list-entry is empty"
  # Capture output to prevent it from spamming the test log
  expect_output(
    result <- abundance_table(articles, names(articles), 2, c(1, 0)),
    "list-entry is empty"
  )

  expect_equal(ncol(result$abtab), 1) # Should drop the empty column
  expect_equal(colnames(result$abtab), "p1")
})

# Wrong Input
test_that("abundance_table throws errors on bad input", {
  expect_error(abundance_table("not a list", "t", 1, 1), "Input must be a list")

  bad_list <- list(data.frame(a=1), "string")
  expect_error(abundance_table(bad_list, "t", 2, 2), "must be data frames")
})
