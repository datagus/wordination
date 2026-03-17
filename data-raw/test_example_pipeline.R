devtools::load_all()

data(example_abundance)
x <- example_abundance

# optimal_incidence
optimal_incidence(x, most_common = 30)

# custom_abundance
ca <- custom_abundance(x, incidence = 4, logscaling = TRUE)

# run_dca
model <- run_dca(x, logscaling = TRUE)

# wordcloud (relaxed filters)
wc <- wordcloud(x, k = 3, model = model, wordnumber = 5,
                pvalue = 1, abmean = 0)

# ellipsis_plot
ellipsis_plot(
  subsets = wc$subsets,
  final_cluster = wc$final_cluster,
  opacity = 0.3,  coverage = 0.95,
  showWords = TRUE
)

# wordendo
wordendo(x, k = 3)

# clusters_barplot
clusters_barplot(x, maxclusters = 5)

# cluster_indicators
cluster_indicators(x, k = 3)

# groups_df
groups_df(x, k = 3, model = model, pvalue = 1, abmean = 0)
