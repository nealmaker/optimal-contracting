#' Forester's NPV from harvest schedule for optimizer
#'
#' Calculates net present value of a single plot from a given harvest schedule
#'
#' @param schedule a numeric vector of harvest steps (harvest years / step
#'   length) corresponding to (and of the same length as) \code{trees}.
#' @param trees a \code{simready} object containing information about sampled
#'   trees in a single plot and their growing conditions.
#' @param params a \code{sim_params} object containing parameters to guide the
#'   simulation.
#' @param models which submodels to use in \code{forester::growth}. Defaults to
#'   Weiskittel's
#' @param gamma forester's pay per cord harvested
#' @param lambda % of timber revenue paid to forester
#' @param rho forester's pay per acre harvested
#' @param theta fixed pay per acre to forester every period, regardless of
#'   harvesting
#' @param phi % of exit value paid to forester
#'
#' @return returns the average, per acre net present value for the forester,
#' property-wide.
#' @export
for_obj <- function(schedule, trees, params = forester::params_default,
                    models = "w", gamma, lambda, rho, theta, phi) {
  schedule <- floor(schedule) # allows use of continuous differential evolution algos
  # Add 1 to upper box constraint if using continuous solver.
  schedule <- schedule * params$steplength # TO ALLOW INTEGER PROGRAMMING ------
  steps <- params$endyr / params$steplength
  trees$cumsurv <- 1 # cumulative survival rate starts at 100%

  # income and costs are present value, discounted inside timestep loop
  income <- 0
  costs <- 0
  lv <- vector(mode = "numeric", length = length(schedule))
  # rank accounts for first 2 logs (2nd only 1/4 weight) and species
  rank <- as.numeric(stringr::str_extract(trees$logs, "^.")) +
    as.numeric(stringr::str_sub(trees$logs, 2, 2)) / 4 +
    spp_ranks[trees$spp, 1]
  t <- 0

  # for each step record terminal values of harvest trees, update ba and bal,
  # and grow one step
  ##################### CANDIDATE FOR C++ LOOP? #######################################
  for(i in 1:steps) {
    cut <- schedule == t
    keep <- schedule > t

    # forester's costs and income based on cutting regime
    if(any(cut)) {
      lv[cut | keep] <- forester::stumpage(trees[cut | keep, ], params)
      cut_vol <- forester::make_logs(trees[cut,]) %>%
        group_by(tree) %>% summarize(vol_sum = sum(vol_ac)) %>%
        full_join(trees[cut,], by = "tree")
      income <- income +
        (theta + rho + gamma * sum(cut_vol$vol_sum * trees$cumsurv[cut]) +
           lambda * sum(lv[cut] * trees$tpa_tree[cut] * trees$cumsurv[cut])) /
        (1 + params$drate) ^ t

      if(sum(keep) == 0) { # clearcut
        costs <- costs + 35 / (1 + params$drate) ^ t
      } else if(min(lv[cut]) >= max(lv[keep]) & # highgrade thin
                abs(sum(trees$ba_tree[keep]) - 65) < # closest to b-line
                abs(sum(trees$ba_tree[keep]) +
                    trees$ba_tree[cut][which.min(lv[cut])] - 65) &
                abs(sum(trees$ba_tree[keep]) - 65) <
                abs(sum(trees$ba_tree[keep]) -
                    trees$ba_tree[keep][which.max(lv[keep])] - 65)) {
        costs <- costs +
          (14 + sum(trees$tpa_tree[cut] * trees$cumsurv[cut])) /
          (1 + params$drate) ^ t
      } else if(min(rank[cut]) >= max(rank[keep]) & # lowgrade thin
                abs(sum(trees$ba_tree[keep]) - 65) < # closest to b-line
                abs(sum(trees$ba_tree[keep]) +
                    trees$ba_tree[cut][which.min(rank[cut])] - 65) &
                abs(sum(trees$ba_tree[keep]) - 65) <
                abs(sum(trees$ba_tree[keep]) -
                    trees$ba_tree[keep][which.max(rank[keep])] - 65)) {
        costs <- costs +
          (14 + 1.5 * sum(trees$tpa_tree[cut] * trees$cumsurv[cut])) /
          (1 + params$drate) ^ t
      } else { # careful tending
        costs <- costs +
          (70 + 2 * sum(trees$tpa_tree[cut] * trees$cumsurv[cut])) /
          (1 + params$drate) ^ t
      }
    } else { # no cutting
      income <- income + theta / (1 + params$drate) ^ t
    }

    if(!any(keep)) break

    # stocking modified by survival rate to account for mortality
    trees$ba[keep] <- sum(trees$ba_tree[keep] * trees$cumsurv[keep]) +
      (trees$ba_tree[keep] * (1 - trees$cumsurv[keep]))
    trees$bal[keep] <- bal(trees$dbh[keep],
                           trees$ba_tree[keep] * trees$cumsurv[keep])

    # Grow to next timestep
    trees[keep,] <- data.frame(forester::grow(trees[keep,], params,
                                              models = models))
    t <- t + params$steplength
  }

  # add exit value
  if(any(keep)) {
    lv[keep] <- forester::stumpage(trees[keep, ], params)
    income <- income +
      phi * sum(lv[keep] * trees$tpa_tree[keep] * trees$cumsurv[keep]) /
      (1 + params$drate) ^ params$endyr
  }

  # return plot's per-acre NPV for forester
  return(income - costs) # ONLY NEGATIVE FOR DE!!!
}
