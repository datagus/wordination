library(devtools)
load_all()
devtools::document()

#Documentation
?importing_words
?abundance_table
?optimal_incidence
?custom_abundance
?running_dca
?clusters_barplot
?cluster_indicators
?wordcloud
?wordendo
?groups_df
?ellipsis_plot


### Example folder path ###
folder_path = "/Users/jorgegustavorodriguezaboytes/data-science/unstructured/data/antonia2" #this is from MacOS


### importing_words()
to_remove <- NULL #you put there the file names you want to remove, including the .txt extension. Do no put the full path

output <- importing_words(folder_path, remove_papers=to_remove, remove_outliers=TRUE)
#You have to run the five following lines
articles_dfs <- output$articles_dfs
papers_titles <- output$papers_titles
npapers <- output$npapers
articles_length <- output$article_length
output$papers_removed #to check with articles were removed

boxplot(output$article_length) #checking the distribution, you can also plot a histogram

### abundance_table()
words <- abundance_table(articles_dfs, papers_titles, npapers, articles_length)
words_df <- words$abtab
moritz <- words$moritz

### optimal_inciddence
optimal_incidence(words_df, most_common=27)
incidence=7 #words with incidence higher than 7 will be included
drop_words <- c()

#customizing abundance_table
custom <- custom_abundance(words_df, drop_words=drop_words, incidence=incidence, site_correction=FALSE, multiplier=1000, logscaling=FALSE)
words_df2 <- custom$new_abundance
moritz2 <- custom$new_moritz

### running_dca (the abundance table needs to be log scale but without correction)
model <- running_dca(words_df2, drop_words=drop_words, incidence=incidence)
plot(model, type = "text" )


### cluster barplot
clusters_barplot(words_df2, maxclusters=10, printclusters = FALSE, showCounts=TRUE)
k <- 9 #change here the desired number of clusters

### cluster indicators
indicators <- cluster_indicators(words_df2,k=k)

### wordcloud
wordi <- wordcloud(words_df2, k=k, model=model,wordnumber=10,cex=1, xmin = 0, xmax = 0, ymin=0, ymax=0, xspace=0, yspace=0)
final_cluster <- wordi$final_cluster
subsets <- wordi$subsets

### dendogram
d <- wordendo(words_df2, k=k)

#for dendogram inspection
plot(d$sub_dend[[2]])
d$dend
d$clusters
d$site_names
d$clusters_length

### groups_df
clus_df <- groups_df(words_df2, k=k, model=model, wordnumber=10)


### ellipsis_plot --- Intersectionality example
group_labels <- c("cluster 1", "cluster 2", "cluster 3", "cluster 4", "cluster 5", "cluster 6", "cluster 7", "cluster 8", "cluster 9")
labelx <- c(-2, 4, -3.4, 2.7, 2.5, -1.9, 0, 2.5, -4.5)
labely <- c(-4.4, 2.3, 2, -3.4, -0.4, 4.4,-4.6, 5.5, -2.4)
elli <- ellipsis_plot(subsets,final_cluster, wordnumber=5,
                      cex=0.8, opacity=0.2, coverage=0.9, withborder=FALSE, showWords = TRUE,
                      with_labels=TRUE,group_labels=group_labels, colbox_text="black", labelx = labelx, labely=labely)



