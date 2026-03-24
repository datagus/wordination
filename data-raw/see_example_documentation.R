# Script to quickly verify example implementation
# to be deleted later

# new changes
devtools::document()

# somewhat buggy so new installation required
devtools::install()

# after re-starting R session, load library
library(wordination)

# now, "Run examples" should work

# Not with our toy example
?pdf_to_text
?filter_words_in_text

?import_words

?abundance_table

##################
# Using our Toy example:

?wordendo
# should'nt it be called denRo --> wordendRo?
# Figure is super big?

?clusters_barplot
# Figure is super big

?cluster_indicators

?optimal_incidence
# to update final incidence downstream; once we have setteled on example

?custom_abundance

?run_dca

?groups_df

?wordcloud
# We plot this dot at (0, 0) by default --> wanted or not?
# no option given in function to surpress it

?ellipsis_plot
