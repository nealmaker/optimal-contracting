#' Landowner's optimizer genoud
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
#' @return returns genoud output, parameters include:
#'   \code{gamma}: price paid to forester per cord harvested
#'   \code{lambda}: proportion of timber revenue paid to forester
#'   \code{rho}: payment to forester per harvested acre in each timestep
#'   \code{theta}: fixed per acre payment to forester in each timestep
#'   \code{phi}: proportion of exit value (ctv at end) paid to forester
#' @export
land_opt_genoud <- function(trees, params = forester::params_default, models = "w") {
  library(rgenoud)
  cores <- 11 #parallel::detectCores() - 1
  cl <- parallel::makeCluster(cores)
  parallel::clusterExport(cl, varlist =
                            c("for_obj", "for_opt", "land_obj", "spp_ranks",
                              "treesgo", "paramsg", "l_drate"))
  clusterEvalQ(cl, library("rgenoud"))
  clusterEvalQ(cl, library("dplyr"))
  clusterEvalQ(cl, library("magrittr"))
  clusterEvalQ(cl, library("forester"))
  out <- genoud(land_obj,
                  nvars = 5,
                  max = TRUE,
                  pop.size = 10,
                  max.generations = 30,
                  wait.generations = 6,
                  hard.generation.limit = TRUE,
                  Domains = matrix(c(rep(0, 5),
                                     c(50, 1, 300, 300, 1)),
                                   ncol = 2),
                  solution.tolerance = 2,
                  boundary.enforcement = 2,
                  data.type.int = FALSE,
                  print.level = 2,
                  trees = trees,
                  params = params,
                  models = models,
                  cluster = cl)
  parallel::stopCluster(cl)
  return(out)
}
