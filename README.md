# wordination

<!-- badges: start -->
<!-- badges: end -->

wordination applies ordination and clustering methods from community
ecology to text corpora. Documents are treated like sampling sites and
words like species: the package converts PDF files to text, filters
words by part-of-speech tags, builds word abundance tables, and then
uses detrended correspondence analysis (DCA), Ward hierarchical
clustering, and indicator value analysis to identify groups of
documents and their characteristic words, ultimately producing 
a dendrogram and ellipsis plot ready to be published in a report.

## Installation

Install the released version from CRAN:

``` r
install.packages("wordination")
```

Or the development version from [GitHub](https://github.com/datagus/wordination):

``` r
# install.packages("pak")
pak::pak("datagus/wordination")
```

## Example

``` r
library(wordination)

# Example word abundance table (documents x words)
data(transformative_abundance)

# Ordination with detrended correspondence analysis
data("transformative_abundance")
words_df <- transformative_abundance
custom <- custom_abundance(words_df, incidence=6)
words_df_custom <- custom$new_abundance
incidence_df_custom <- custom$new_moritz
model <- run_dca(words_df_custom)
plot(model, type = "text", display="species")
model

# Cluster documents into 6 groups and find indicator words
k <- 6
wc <- wordcloud(words_df_custom, k=k, model=model,wordnumber= 5,cex=0.8)
final_cluster <- wc$final_cluster
subsets <- wc$subsets
head(final_cluster)
```

For the full workflow, from PDF files to clusters and plots, see the vignette:

``` r
vignette("main-vignette", package = "wordination")
```

## Acknowledgements

This package was developed within an NFDI4DS Speed Boat Project and 
granted through Leuphana University Lueneburg.
NFDI4DS (National Research Data Infrastructure for Data Science and
Artificial Intelligence) is funded by the German Research Foundation (DFG).
