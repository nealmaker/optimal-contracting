#' Landowner's optimizer pso
#'
#' Determines forester compensation package that maximizes landowner's profit
#'
#' @param trees a \code{simready} object containing information about sampled
#'   trees in a single plot and their growing conditions.
#' @param params a \code{sim_params} object containing parameters to guide the
#'   simulation.
#' @param models which submodels to use in \code{forester::growth}. Defaults to
#'   Weiskittel's
#'
#' @return returns a data frame of optimal coefficients for the forester's
#'   compensation package, which includes:
#'   \code{gamma}: price paid to forester per cord harvested
#'   \code{lambda}: proportion of timber revenue paid to forester
#'   \code{rho}: payment to forester per harvested acre in each timestep
#'   \code{theta}: fixed per acre payment to forester in each timestep
#'   \code{phi}: proportion of exit value (ctv at end) paid to forester
#' @export
land_opt_pso <- function(trees, params = forester::params_default, models = "w") {
  out <- hydroPSO(fn = land_obj,
                trees = trees,
                params = params,
                models = models,
                lower = rep(0, 5),
                upper = c(50, 1, 300, 300, 1),
                control = list(MinMax = 'max',
                               parallel = 'none')) #need par.pkgs to load packages onto cores, not sure how to load objects & don't want to figure it out.
  return(out)
}
