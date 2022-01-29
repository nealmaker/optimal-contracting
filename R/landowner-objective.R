#' Landowner's NPV for optimizer
#'
#' Calculates net present value of a single plot from a given forester
#' compensation package
#'
#' @param for_comp a numeric vector of coefficients that correspond to the
#'   forester's compensation package. Ordered gamma, lambda, rho, theta, phi
#'   (see \code{land_opt} documentation)
#' @param trees a \code{simready} object containing information about sampled
#'   trees in a single plot and their growing conditions.
#' @param params a \code{sim_params} object containing parameters to guide the
#'   simulation.
#' @param models which submodels to use in \code{forester::growth}. Defaults to
#'   Weiskittel's
#'
#' @return returns the average, per acre net present value for the landowner,
#' property-wide.
#' @export
land_obj <- function(for_comp, trees, params = forester::params_default,
                     models = "w") {
  gamma <- for_comp[1]
  lambda <- for_comp[2]
  rho <- for_comp[3]
  theta <- for_comp[4]
  phi <- for_comp[5]

  # get cut schedule from forester opt
  cutyrs <- for_opt(trees = trees, params = params, models = models,
                    gamma = gamma, lambda = lambda, rho = rho, theta = theta,
                    phi = phi)

  steps <- params$endyr / params$steplength
  trees$cumsurv <- 1 # cumulative survival rate starts at 100%

  # loop over plots, then over timesteps w/in each plot ------------------------
  out <- sapply(unique(trees$plot), function(i) {
    treesg <- trees[trees$plot == i,]
    schedule <- cutyrs[trees$plot == i]

    # income and costs are present value, discounted inside timestep loop
    income <- 0
    costs <- 0
    t <- 0

    ##################### CANDIDATE FOR C++ LOOP? #######################################
    for(i in 1:steps) {
      cut <- schedule == t
      keep <- schedule > t

      if(any(cut)) {
        gross <- sum(forester::stumpage(treesg[cut,], params) *
                       treesg$tpa_tree[cut] * treesg$cumsurv[cut])
        cut_vol <- forester::make_logs(treesg[cut,]) %>%
          group_by(tree) %>% summarize(vol_sum = sum(vol_ac)) %>%
          full_join(treesg[cut,], by = "tree")
        income <- income + gross / (1 + l_drate) ^ t
        costs <- costs +
          (theta + rho +
             gamma * sum(cut_vol$vol_sum * treesg$cumsurv[cut]) +
             lambda * gross) /
          (1 + l_drate) ^ t
      } else { # no cutting
        costs <- costs + theta / (1 + l_drate) ^ t
      }

      if(!any(keep)) break

      # stocking modified by survival rate to account for mortality
      treesg$ba[keep] <- sum(treesg$ba_tree[keep] * treesg$cumsurv[keep]) +
        (treesg$ba_tree[keep] * (1 - treesg$cumsurv[keep]))
      treesg$bal[keep] <- bal(treesg$dbh[keep],
                             treesg$ba_tree[keep] * treesg$cumsurv[keep])

      # Grow to next timestep
      treesg[keep,] <- data.frame(forester::grow(treesg[keep,], params,
                                                models = models))
      t <- t + params$steplength
    }

    # add exit value
    if(any(keep)) {
      lv <- sum(forester::stumpage(treesg[keep, ], params) *
        treesg$tpa_tree[keep] * treesg$cumsurv[keep])
      costs <- costs + (phi * lv / (1 + l_drate) ^ params$endyr)
      income <- income + (lv / (1 + l_drate) ^ params$endyr)
    }

    # return plot's per-acre NPV for landowner
    return(income - costs)
  })
  return(mean(out))
}
