example_abundance <- data.frame(
  # -- Cluster 1 articles: energy technology --
  #    governance+citizen leak from cl2
  art01 = c(16, 4, 9, 3,10, 7, 2, 5,13,   4, 3, 0, 0, 0, 0, 0, 0, 0,   1, 1, 1, 1, 1, 1, 1, 1, 0,   2, 3, 1),
  art02 = c(14, 5, 8, 4, 9, 6, 3, 6,11,   5, 4, 0, 0, 0, 0, 3, 0, 0,   1, 1, 1, 1, 1, 1, 1, 1, 0,   3, 2, 2),
  art03 = c( 6,12, 4,10, 3, 2, 8,11, 5,   3, 2, 0, 0, 0, 0, 5, 0, 0,   1, 1, 1, 1, 1, 1, 1, 1, 0,   1, 4, 1),
  art04 = c( 7,11, 5, 12, 4, 3, 7,10, 6,   4, 3, 0, 0, 0, 0, 1, 0, 0,   1, 1, 1, 1, 1, 1, 1, 1, 0,   2, 3, 2),

  # -- Cluster 2 articles: social sciences --
  #    energy+grid leak from cl1; climate+emission leak from cl3
  art05 = c( 7, 5, 0, 1, 0, 0, 1, 2, 0,  13, 5,10, 3, 12, 7, 2, 4,12,   6, 4, 4, 0, 0, 0, 0, 0, 1,   4, 3, 2),
  art06 = c( 6, 4, 0, 1, 0, 0, 1, 2, 0,  12, 6, 9, 4, 8, 6, 3, 5,11,   5, 3, 4, 0, 0, 0, 0, 0, 1,   2, 5, 1),
  art07 = c( 5, 3, 0, 0, 0, 0, 0, 3, 0,   5,13, 4,11, 3, 2, 9,12, 6,   7, 5, 5, 0, 0, 0, 0, 0, 1,   5, 2, 3),
  art08 = c( 8, 6, 0, 1, 0, 0, 0, 4, 0,   6,12, 5,10, 4, 3, 8,11, 7,   4, 3, 6, 0, 0, 0, 0, 0, 1,   3, 4, 2),

  # -- Cluster 3 articles: life sciences --
  #    engagement+democracy leak from cl2
  art09 = c( 0, 0, 1, 0, 1, 4, 0, 0, 0,   0, 0, 0, 2, 1, 0, 2, 3, 4,  14, 5,10, 3, 4, 6, 2,13, 4,   3, 2, 2),
  art10 = c( 0, 0, 0, 0, 1, 4, 0, 0, 0,   0, 0, 0, 2, 1, 0, 4, 4, 3,  13, 6, 9, 4, 7, 5, 3,12, 5,   1, 4, 3),
  art11 = c( 0, 0, 2, 0, 1, 3, 4, 0, 0,   0, 0, 0, 2, 1, 0, 0, 2, 5,   6,12, 4,11, 3, 9, 8, 5,13,   4, 3, 1),
  art12 = c( 0, 0, 0, 0, 1, 1, 4, 0, 0,   0, 0, 0, 2, 1, 0, 0, 5, 3,   5,11, 5,10, 8, 8, 7, 6,12,   2, 5, 2)
)

rownames(example_abundance) <- c(
  "energy", "grid", "turbine", "storage", "biomass",
  "solar", "battery", "electricity", "power",

  "governance", "citizen", "stakeholder", "policy", "participation",
  "institution", "regulation", "democracy", "engagement",

  "climate", "emission", "carbon", "biodiversity", "pollution",
  "ecosystem", "habitat", "conservation", "sustainability",

  "community", "development", "system"
)

usethis::use_data(example_abundance, overwrite = TRUE)
