for_opt_de <- function(trees, params = forester::params_default,
                    models = "w", gamma, lambda, rho, theta, phi) {
  out <- lapply(unique(trees$plot), function(i) {
    treesg <- dplyr::filter(trees, plot == i)
    treelength <- nrow(treesg)

    cutyr <- JDEoptim(lower = rep(0, treelength),
                     upper = rep((params$endyr / params$steplength) + 2, #added one for integer programming w/ 'floor' in objective
                                 treelength),
                     fn = for_obj,
                     trees = treesg,
                     params = params,
                     models = models,
                     gamma = gamma,
                     lambda = lambda,
                     rho = rho,
                     theta = theta,
                     phi = phi)

    return(data.frame(tree = treesg$tree, cutyr = cutyr))
  })

  cutyrs <- do.call(rbind, out)
  return((dplyr::left_join(trees, cutyrs, by = "tree") %>%
            dplyr::mutate(cutyr = cutyr * params$steplength))$cutyr)
}
