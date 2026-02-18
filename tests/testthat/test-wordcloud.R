test_that("wordcloud returns expected structure", {

  # small but valid abundance table
  x <- data.frame(
    art1 = c(3, 0, 1, 2, 1),
    art2 = c(0, 2, 1, 0, 1),
    art3 = c(1, 1, 0, 1, 0),
    art4 = c(2, 0, 1, 1, 1)
  )
  rownames(x) <- paste0("word", 1:5)

  # obtain DCA model from your function
  dca_model <- running_dca(x, logscaling = TRUE)

  # suppress plotting during tests
  pdf(NULL)

  # wordcloud with relaxed filtering to ensure words pass
  res <- wordcloud(
    x = x,
    k = 2,
    model = dca_model,
    wordnumber = 2,
    pvalue = 1,
    abmean = 0
  )

  dev.off()

  # structure checks
  expect_type(res, "list")
  expect_named(res, c("final_cluster", "subsets", "word"))

  expect_s3_class(res$final_cluster, "data.frame")
  expect_type(res$subsets, "list")

  # number of cluster subsets equals k
  expect_length(res$subsets, 2)

  # each subset has cluster column
  expect_true("cluster" %in% colnames(res$subsets[[1]]))
})

