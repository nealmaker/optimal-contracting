library('forester')
library('tidyverse')
library("rgenoud")
library("parallel")
options(dplyr.summarise.inform=F) # quiets "friendly" warning from summarise()

trees <- as_tibble(simtrees_sample)
treesgo <- trees[!is.na(trees$spp), ]
paramsg <- params_default
paramsg$steplength <- 10
paramsg$endyr <- 40
paramsg$drate <- .04 # forester's discount rate
l_drate <- .03 # landowner's discount rate

# for ranking trees
spp_ranks <-
  data.frame(value = c(.8, 1.2, 1.2, .8, 0, .8, 1, 1.2, 1.2, 1, 0, 1.2, .8, 2,
                       1, 2, 1, .8, 0, 1, 1, .8, 1, 2, 1, .8, .9, 0))
row.names(spp_ranks) <- levels(simtrees_sample$spp)

source("R/forester-objective.R")
source("R/forester-opt.R")
source("R/landowner-objective.R")
source("R/landowner-opt.R")

best_comp_pcakage <- land_opt(treesgo, paramsg)
