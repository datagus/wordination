test_that("importing_words() correctly imports text files and processes them", {
  # Setup temporary test files
  tmp <- tempdir()
  writeLines("apple banana apple mango pera mango", file.path(tmp, "file1.txt"))
  writeLines("banana pera mango banana apple mango", file.path(tmp, "file2.txt"))
  writeLines("sandia mango papaya mango apple mango", file.path(tmp, "file3.txt"))
  writeLines("remove me please ", file.path(tmp, "remove_this.txt"))

  # Run function excluding one file
  result <- importing_words(folder_path = tmp, remove_papers = "remove_this.txt")

  # Check that result is a list of expected elements
  expect_type(result, "list")
  expect_named(result, c("article_length", "articles_dfs", "papers_titles", "npapers", "papers_removed"))

  # Check lengths
  expect_type(result$article_length, "integer")
  expect_equal(length(result$article_length), result$npapers)

  # Check data frames
  expect_type(result$articles_dfs, "list")
  expect_equal(length(result$articles_dfs), result$npapers)
  expect_true(all(sapply(result$articles_dfs, is.data.frame)))

  # Check that data frames have expected columns
  for (df in result$articles_dfs) {
    expect_true(all(c("words", "Freq") %in% colnames(df)))
    expect_equal(length(df$words), length(df$Freq))
  }

  # Check filenames
  expect_type(result$papers_titles, "character")
  expect_equal(sort(result$papers_titles), sort(c("file1.txt", "file2.txt", "file3.txt")))

  # Check number of files
  expect_equal(result$npapers, 3)
})
