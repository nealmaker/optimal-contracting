#' Landowner's optimizer
#'
#' Determines forester compensation package that maximizes landowner's profit
#'
#' @param trees a \code{simready} object containing information about sampled
#'   trees in a single plot and their growing conditions.
#' @param params a \code{sim_params} object containing parameters to guide the
#'   simulation.
#' @param models which submodels to use in \code{forester::growth}. Defaults to
#'   Weiskittel's
#' @param algo which optimization algorithm to use. One of 'genoud', 'ga', 'sa',
#'   or 'pso'.
#'
#' @return returns the optimizer's output; varies based on algorithm used.
#' @export
land_opt <- function(trees, params = forester::params_default, models = "w",
                     algo = "genoud") {
  if(algo == "genoud") land_opt_genoud(trees, params, models)
  else if(algo == "ga") land_opt_ga(trees, params, models)
  else if(algo == "sa") land_opt_sa(trees, params, models)
  else if(algo == "pso") land_opt_pso(trees, params, models)
  else stop(paste("No optimization algorithm of type", algo, "configured."))
}
