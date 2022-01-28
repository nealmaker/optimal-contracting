library('forester')
library('tidyverse')
library("rgenoud")
options(dplyr.summarise.inform=F) # quiets "friendly" warning from summarise()

trees <- as_tibble(simtrees_sample)
treesgo <- trees[!is.na(trees$spp), ]
paramsg <- params_default
paramsg$endyr <- 30

# for ranking trees
spp_ranks <-
  data.frame(value = c(.8, 1.2, 1.2, .8, 0, .8, 1, 1.2, 1.2, 1, 0, 1.2, .8, 2,
                       1, 2, 1, .8, 0, 1, 1, .8, 1, 2, 1, .8, .9, 0))
row.names(spp_ranks) <- levels(simtrees_sample$spp)

source("R/forester-objective.R")
source("R/forester-opt.R")
source("R/landowner-objective.R")

# landowner and forester should be able to have different discount rates!!!!!!!!!!!!!
# (now they both use params$drate)
