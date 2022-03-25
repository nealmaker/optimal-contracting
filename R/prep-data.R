library('tidyverse')
library('forester')

################################################################################
## Hardwood data from Paul Smith's
################################################################################
dat <- read.csv("data/psc-hw-data.csv")
dat <- dat %>%
  mutate(stand = 1,
         tree = 1:nrow(dat),
         dbh = as.double(dbh),
         cr = as.double(cr),
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
class(dat) <- c("tbl_df", "tbl", "data.frame")

save(dat, file = "dat-simready.rda")

################################################################################
## Lower quality softwood data from Eagle Ledge
################################################################################
load("../eagle-ledge/data/el-trees-fixed.rda")

temp <- el_trees_fixed %>% group_by(plot) %>%
       summarize(pct_spfr = sum(spp %in% c("fir", "spruce", "hemlock"))/n()) %>%
       arrange(desc(pct_spfr))
View(filter(el_trees_fixed, plot %in% temp$plot[temp$pct_spfr == 1]))
# 3_31 is 4 eh well differentiated; 1_4 is 6 eh-sp-fr; 1_6 is 3 sp;
# 4_10 is 4 eh pretty similar; 1_10 is 5 sp similar quality range of sizes

new <- filter(el_trees_fixed, plot %in% c("3_31", "1_4", "1_10"))

load("dat-simready.rda")
new <- select(new, names(dat))
new <- new %>% mutate(plot = case_when(plot == "3_31" ~ 4,
                                       plot == "1_4" ~ 5,
                                       TRUE ~ 6),
                      stand = 2) %>%
  arrange(plot)
new$tree <- (max(dat$tree) + 1):(max(dat$tree) + nrow(new))

dat <- rbind(dat, new)

save(dat, file = "dat-simready.rda")
