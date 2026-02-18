test_that("ellipsis_plot returns expected structure", {

  # ---- mock wordcloud() outputs ----
  # create small reproducible cluster output
  set.seed(1)

  df1 <- data.frame(
    DCA1 = rnorm(5),
    DCA2 = rnorm(5),
    cluster = rep(1, 5)
  )
  rownames(df1) <- paste0("w", 1:5)

  df2 <- data.frame(
    DCA1 = rnorm(5),
    DCA2 = rnorm(5),
    cluster = rep(2, 5)
  )
  rownames(df2) <- paste0("w", 6:10)

  subsets <- list(df1, df2)

  final_cluster <- rbind(df1, df2)

  colores <- c("#FF0000", "#0000FF")

  # suppress plotting device
  pdf(NULL)

  res <- ellipsis_plot(
    subsets = subsets,
    final_cluster = final_cluster,
    colores = colores,
    showWords = FALSE
  )

  dev.off()

  # ---- structure checks ----
  expect_type(res, "list")
  expect_named(res,
               c("cc", "cc2", "coordinates",
                 "clusteros", "cluster_points"))

  # total rows equal combined subsets
  expect_equal(nrow(res$cc), 10)

  # coordinates must have 2 columns
  expect_equal(ncol(res$coordinates), 2)

  # cluster vector length matches rows
  expect_length(res$clusteros, nrow(res$coordinates))

  # clusters detected correctly
  expect_setequal(unique(res$clusteros), c(1, 2))
})

test_that("ellipsis_plot returns expected structure on real wordcloud() output", {

  # ---- small but valid abundance table ----
  x <- data.frame(
    art1 = c(3, 0, 1, 2, 1),
    art2 = c(0, 2, 1, 0, 1),
    art3 = c(1, 1, 0, 1, 0),
    art4 = c(2, 0, 1, 1, 1)
  )
  rownames(x) <- paste0("word", 1:5)

  # ---- obtain DCA model ----
  dca_model <- running_dca(x, logscaling = TRUE)

  # ---- run wordcloud (relaxed filtering) ----
  pdf(NULL)  # suppress plotting
  wc_res <- wordcloud(
    x = x,
    k = 2,
    model = dca_model,
    wordnumber = 2,
    pvalue = 1,
    abmean = 0
  )
  dev.off()

  # ---- run ellipsis_plot using actual wordcloud output ----
  pdf(NULL)  # suppress plotting
  ell_res <- ellipsis_plot(
    subsets = wc_res$subsets,
    final_cluster = wc_res$final_cluster,
    colores = c("#FF0000", "#0000FF"),
    showWords = FALSE
  )
  dev.off()

  # ---- structural checks ----
  expect_type(ell_res, "list")
  expect_named(ell_res, c("cc", "cc2", "coordinates", "clusteros", "cluster_points"))

  # cc combines all subsets
  expect_equal(nrow(ell_res$cc), sum(sapply(wc_res$subsets, nrow)))

  # coordinates has DCA1/DCA2
  expect_equal(ncol(ell_res$coordinates), 2)
  expect_setequal(unique(ell_res$clusteros), c(1,2))
})

