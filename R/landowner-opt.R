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
#'
#' @return returns a data frame of optimal coefficients for the forester's
#'   compensation package, which includes:
#'   \code{gamma}: price paid to forester per cord harvested
#'   \code{lambda}: proportion of timber revenue paid to forester
#'   \code{rho}: payment to forester per harvested acre in each timestep
#'   \code{theta}: fixed per acre payment to forester in each timestep
#'   \code{phi}: proportion of exit value (ctv at end) paid to forester
#' @export
land_opt <- function(trees, params = forester::params_default, models = "w") {
  cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(cores)
  coefs <- genoud(land_obj,
                  nvars = 5,
                  max = TRUE,
                  pop.size = 20,
                  max.generations = 50,
                  wait.generations = 6,
                  hard.generation.limit = TRUE,
                  Domains = matrix(c(rep(0, 5),
                                     c(50, 1, 300, 300, 1)),
                                   ncol = 2),
                  solution.tolerance = 2,
                  boundary.enforcement = 2,
                  data.type.int = FALSE,
                  print.level = 1,
                  trees = trees,
                  params = params,
                  models = models,
                  cluster = cl)$par
  parallel::stopCluster(cl)
  return(data.frame(coef = c("gamma", "lambda", "rho", "theta", "phi"),
                    value = coefs))
}
