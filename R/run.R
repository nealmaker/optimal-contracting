library('forester')
library('tidyverse')
library("rgenoud")

trees <- as_tibble(simtrees_sample)
treesgo <- trees[!is.na(trees$spp), ]
paramsg <- params_default

# for ranking trees
spp_ranks <-
  data.frame(value = c(.8, 1.2, 1.2, .8, 0, .8, 1, 1.2, 1.2, 1, 0, 1.2, .8, 2,
                       1, 2, 1, .8, 0, 1, 1, .8, 1, 2, 1, .8, .9, 0))
row.names(spp_ranks) <- levels(simtrees_sample$spp)
