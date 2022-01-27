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
#'
#' @return returns the average, per acre net present value for the forester,
#' property-wide.
#' @export
for_obj <- function(schedule, trees, params = forester::params_default,
                    models = "w") {
  schedule <- schedule * params$steplength # TO ALLOW INTEGER PROGRAMMING ------
  steps <- params$endyr / params$steplength
  trees$cumsurv <- 1 # cumulative survival rate starts at 100%

  # terminal value ($/tree)
  tv <- vector(mode = "numeric", length = length(schedule))
  t <- 0

  # for each step record terminal values of harvest trees, update ba and bal,
  # and grow one step
  ##################### CANDIDATE FOR C++ LOOP? #######################################
  for(i in 1:steps) {
    cut <- schedule == t
    keep <- schedule > t

    if(any(cut)) {
      tv[cut] <- forester::stumpage(trees[cut,], params) * trees$cumsurv[cut]
    }

    if(!any(keep)) break

    # stocking modified by survival rate to account for mortality
    trees$ba[keep] <- sum(trees$ba_tree[keep] * trees$cumsurv[keep]) +
      (trees$ba_tree[keep] * (1 - trees$cumsurv[keep]))
    trees$bal[keep] <- bal(trees$dbh[keep],
                           trees$ba_tree[keep] * trees$cumsurv[keep])

    trees[keep,] <- data.frame(forester::grow(trees[keep,], params,
                                              models = models))

    t <- t + params$steplength
  }

  # record terminal values for trees harvested in last step
  cut <- schedule == t
  if(any(cut)) {
    tv[cut] <- forester::stumpage(trees[cut,], params) * trees$cumsurv[cut]
  }

  # return plot's per-acre NPV
  sum(trees$tpa_tree * tv / (1 + params$drate) ^ schedule)
}
