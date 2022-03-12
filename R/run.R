################################################################################
plot_num <- 1
################################################################################
library('forester')
library('tidyverse')
library("rgenoud")
library("parallel")
options(dplyr.summarise.inform=F) # quiets "friendly" warning from summarise()

load("dat-simready.rda")
dat <- dat[dat$plot == unique(dat$plot)[plot_num],]
trees <- as_tibble(simtrees_sample)
treesgo <- trees[!is.na(trees$spp),]
paramsg <- params_default
paramsg$steplength <- 10
paramsg$endyr <- 40
paramsg$drate <- .04 # forester's discount rate
l_drate <- .03 # landowner's discount rate

# #2 log prices from PBF averages
paramsg$prices$mill_grade2 <-
  c(470, 170, 220, 190, 400, 320, 370, 160, 490, 290, 400, 160, 290, 450, 220,
    220, 350, 280, 160, 280, 350, 320, 340, 190, 170)

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
