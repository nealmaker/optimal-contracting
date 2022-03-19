library('tidyverse')
library('forester')

dat <- read.csv("data/psc-hw-data.csv")
dat <- dat %>%
  mutate(stand = 1,
         tree = 1:nrow(dat),
         dbh = as.double(dbh),
         logs = as.character(logs),
         forest_type = "Northern hardwood",
         site_class = 5,
         lat = 44.4,
         lon = -74.3,
         elev = 1700,
         tpa_tree = 20,
         ba_tree = 0.005454 * dbh ^ 2 * tpa_tree) %>%
  group_by(plot) %>%
  mutate(ba = sum(ba_tree),
         bal = bal(dbh, ba_tree)) %>%
  ungroup()

dat$spp <- factor(dat$spp, levels = levels(simtrees_sample$spp))

save(dat, file = "data/dat-temp.rda")

# Do this section on laptop, w/ ht_model_op ------------------------------------
library(tidyverse)
load("data/dat-temp.rda")
load("../big-rdas/ht-model-op.rda")

dat$ht <- predict(ht_model_op, newdata = dat)

dat <- select(dat, stand, plot, tree, spp, dbh, cr, logs, ba, bal, forest_type,
              site_class, lat, lon, elev, ht, tpa_tree, ba_tree)
class(dat) <- c("tbl_df", "tbl", "data.frame", "simready")

save(dat, file = "dat-simready.rda")
