#' Example word abundance table
#'
#' A small word abundance table for use in examples and vignettes. The table
#' contains 30 words across 12 articles arranged in three thematic clusters
#' (energy/technology, governance/social sciences, environment/life sciences)
#' plus three ubiquitous words (community, development, system) appearing across
#' all articles. The structure mirrors a scaled-down version of a real-world
#' corpus analysis with realistic incidence and abundance patterns.
#'
#' @format A data frame with 30 rows and 12 columns.
#'   Row names are words, column names are article identifiers.
#'   Values represent word counts per article.
#'
#' @source Simulated data modeled after a community energy research corpus.
#'
#' @examples
#' data(example_abundance)
#' dim(example_abundance)
#' rownames(example_abundance)
"example_abundance"
