################################################################################
plot_num <- 3
################################################################################
library('forester')
library('tidyverse')
library("rgenoud")
library("GA")
library("GenSA")
library("hydroPSO")
library("DEoptim")
library("DEoptimR")
library("parallel")
options(dplyr.summarise.inform=F) # quiets "friendly" warning from summarise()

load("dat-simready.rda")
dat <- dat[dat$plot == unique(dat$plot)[plot_num],]
dat$dbh <- as.double(dat$dbh)
dat$cr <- as.double(dat$cr)
class(dat) <- c("tbl_df", "tbl", "data.frame")
# trees <- as_tibble(simtrees_sample)
# treesgo <- trees[!is.na(trees$spp),]
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
source("R/landowner-opt-genoud.R")
source("R/landowner-opt-ga.R")
source("R/landowner-opt-sa.R")
source("R/landowner-opt-pso.R")

out <- land_opt(dat, paramsg, algo = "ga")

# testing Forester's Optimizer
# ptemp <- c(3, .037, 22.6, 17.8, .068)
#
# system.time(out_genoud <- for_opt(dat, paramsg, "w", ptemp[[1]], ptemp[[2]],
#                                   ptemp[[3]], ptemp[[4]], ptemp[[5]]))
# system.time(out_de <- for_opt_de(dat, paramsg, "w", ptemp[[1]], ptemp[[2]],
#                                  ptemp[[3]], ptemp[[4]], ptemp[[5]]))
