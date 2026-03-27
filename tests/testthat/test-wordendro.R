test_that("wordendro returns expected dendrogram structure", {

  # slightly larger abundance table
  x <- data.frame(
    art1 = c(3, 0, 1, 2, 1, 0),
    art2 = c(0, 2, 1, 0, 1, 1),
    art3 = c(1, 1, 0, 1, 0, 2),
    art4 = c(2, 0, 1, 1, 1, 0),
    art5 = c(0, 1, 2, 0, 1, 1)
  )
  rownames(x) <- paste0("word", 1:6)

  k <- 2
  colores <- c("#FF0000", "#0000FF")

  # suppress plotting during tests
  pdf(NULL)

  res <- wordendro(
    x = x,
    k = k,
    colores = colores
  )

  dev.off()

  # Structure checks
  expect_type(res, "list")
  expect_named(res,
               c("dend", "sub_dend", "clusters",
                 "site_names", "clusters_length"))

  expect_s3_class(res$dend, "dendrogram")
  expect_type(res$sub_dend, "list")
  expect_type(res$clusters, "list")
  expect_type(res$site_names, "list")

  # number of clusters equals k
  expect_length(res$sub_dend, k)
  expect_length(res$clusters, k)
  expect_length(res$site_names, k)

  # total number of sites equals number of articles
  total_sites <- sum(res$clusters_length)
  expect_equal(total_sites, ncol(x))
})
