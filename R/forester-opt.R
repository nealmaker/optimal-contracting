#' Forester's optimizer
#'
#' Determines harvest schedule that maximizes forester's profit
#'
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
#' @return returns a vector of harvest years corresponding to the trees in
#'   \code{trees}
#' @export
for_opt <- function(trees, params = forester::params_default,
                    models = "w", gamma, lambda, rho, theta, phi) {
  out <- lapply(unique(trees$plot), function(i) {
    treesg <- dplyr::filter(trees, plot == i)
    treelength <- nrow(treesg)

    cutyr <- genoud(for_obj,
                    nvars = treelength,
                    max = TRUE,
                    pop.size = 50,
                    max.generations = 50,
                    wait.generations = 6,
                    hard.generation.limit = TRUE,
                    Domains = matrix(c(rep(0, treelength),
                                       rep(paramsg$endyr / paramsg$steplength,
                                           treelength)),
                                     ncol = 2),
                    solution.tolerance = 2,
                    boundary.enforcement = 2,
                    data.type.int = TRUE,
                    print.level = 1,
                    trees = treesg,
                    params = params,
                    models = models,
                    gamma = gamma,
                    lambda = lambda,
                    rho = rho,
                    theta = theta,
                    phi = phi)$par

    return(data.frame(tree = treesg$tree, cutyr = cutyr))
  })

  cutyrs <- do.call(rbind, out)
  return((dplyr::left_join(trees, cutyrs, by = "tree") %>%
           dplyr::mutate(cutyr = cutyr * params$steplength))$cutyr)
}
